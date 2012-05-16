%% File    : wtp_responder.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WTP Responder
%%% Created : 28 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>
%%%
%%% Note:
%%% - Everything related to segmentation is handled elsewhere, thus only
%%%   complete WTP PDUs are handled here.
%%%
-module(wtp_responder).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wtp_responder.erl,v 1.2 2001/07/10 12:42:39 johblo Exp $ ').
-modified('$Date: 2001/07/10 12:42:39 $ ').
-modified_by('$Author: johblo $ ').
-vsn("1").

%% Component internal interface
-export([request/2,
	 first_rcv_invoke/5,
	 start/1,stop/1]).

%% gen_fsm callbacks
-export([listen/2,tidok_wait/2,invoke_resp_wait/2,result_wait/2,
	 result_resp_wait/2,wait_timeout/2,
	 init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3]).

-import(wtp_common,[start_ack_timer/3,start_retry_timer/3,start_wait_timer/3,
		  stop_timer/1]).

-include("wtp.hrl").
-include("wtpif.hrl").

%% sdu - WSP Session data unit. Used to
%%     + store session data sent by tr_result_req, and eventually resent
%%     + data got in a tr_invoke_ind, and eventually sent later when the Tid is
%%       verified.
-record(state,{wsp,     % (pid) - Reference to application above (WSP)
	       wdp,     % (pid) - Reference to application below (WDP)
	       tpar,    % (tuple) - {Appref,DestAddress} tuple
	       bearer,  % (int) - Bearer to send messages on (from wdp.hrl)
	       tdb,     % (pid) - Transaction database
	       tid,     % (int) - The Tid to use in all PDUs in this transaction
	       rcr,     % (int) - Number of retransmissions performed
	       aec,     % (int) - Number of expired acks performed
	       uack,    % (bool) True if User Ack has been requested
	       tcl,     % (0-2) Class of this transaction
	       ack_sent,    % (bool) True if an Ack has been sent already
	       sdu,         % (binary) WSP Session data unit
	       ack_exitinfo,% (list) Exitinfo in the Ack on the invoke (Class 1)
	       ack,         % (timer) Ack timer
	       retry,       % (timer) Retry timer
	       wait         % (timer) Wait timer
	      }).

%-------------------------------------------------------------------------------
%% API
first_rcv_invoke(WTPres,Tpar,Pdu,WSP,WDP) ->
    gen_fsm:send_event(WTPres,{Pdu,Tpar,WSP,WDP}).


%% Request to the transaction from application (WSP) or WDP
request(WTPpid,Content) ->
    gen_fsm:send_event(WTPpid, Content).

%-------------------------------------------------------------------------------
% Maintenance
start(Args) ->
    case gen_fsm:start_link(?MODULE,Args,?START_OPTIONS) of
	{ok,Wtp} ->
	    Wtp;
	A ->
	    throw({error,cant_start_wtp_responder})
    end.


stop(Wtp) ->
    gen_server:call(Wtp,stop).


%% .............................................................................
init(Tdb) ->
    {ok,listen,#state{tdb=Tdb}}.

terminate(Reason, StateName, StatData) ->
    ok.

%% -----------------------------------------------------------------------------
%% >>>The listen state<<<
%% Handles an incoming invoke.ind from WDP, needs to
%% 1. Validate the indication PDU
%% 2. Validate the Tid
%% 3. Check Class,User Ack and validity of Tid and do the corresponding action
%%    as described in the WTP Responder state machine.
%% Note:
%% - RealTid includes the direction bit (bit 16), Tid does not.
%%   The direction bit is already toggled, ie good for sending responses back.
listen({#invoke_pdu{tid=TID,tidnew=TIDNew,tcl=TCL,uack=UAck,data=WSPdata},
	Tpar,WSP,WDP},State) ->
    {_,{_,_,Bearer}}=Tpar,
    ToggledTid=wtp_man:toggle_tid(TID),
    Valid_tid=wtp_man:validate_tid(TID,TIDNew,Tpar,State#state.tdb),
    wtp_db:add_transaction(State#state.tdb,{Tpar,responder,ToggledTid},
			   {self(),TIDNew}),

    State2=State#state{wsp=WSP,wdp=WDP,tpar=Tpar,bearer=Bearer,
		       tid=ToggledTid,
		       rcr=0,aec=0,
		       uack=UAck,
		       tcl=TCL,ack_sent=false,
		       sdu=WSPdata,ack_exitinfo=undef,
		       ack=undef,retry=undef,wait=undef},
    case {UAck,Valid_tid} of
	{?TRUE,true} ->
	    {next_state, invoke_resp_wait, tr_invoke_ind(State2)};
	{?FALSE,true} ->
	    {next_state, invoke_resp_wait, tr_invoke_ind(State2)};
	{_,false} ->
	    BareTid=TID band 32767,
	    ?debug("On first_rcv_invoke >>Invalid Tid: ~p<<",[BareTid],listen),
	    ack(?FALSE,?TRUE,[],State2),
	    {next_state, tidok_wait, State2}
    end;
listen(Event, State) ->
    ?error("Got",[Event],listen),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).

%% -----------------------------------------------------------------------------
%% >>>The tidok_wait state<<<
tidok_wait(#ack_pdu{tidver=?TRUE}, State) ->
    {next_state, invoke_resp_wait,tr_invoke_ind(State)};
tidok_wait(#abort_pdu{}, State) ->
    next_state_listen(State);
tidok_wait(#invoke_pdu{rid=RID},State) ->
    if
	RID==?FALSE ->
	    ignore;
	RID==?TRUE ->
	    ack(?FALSE,?TRUE,[],State)
    end,
    {next_state,tidok_wait, State};
%% rcv_error
tidok_wait(Event,State) ->
    ?error("Event: ~p",[Event],tidok_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).


%% -----------------------------------------------------------------------------
%% >>>The invoke_resp_wait state<<<
invoke_resp_wait({tr_invoke_res,WSP,Exitinfo}, State) ->
    Bearer=State#state.bearer,    
    Uack=State#state.uack,
    case {State#state.tcl,Exitinfo} of
	{?CLASS1,[]} ->
	    queue_ack(?FALSE,?FALSE,[],State),
	    W=start_wait_timer(Bearer,{base,Uack},self()),
	    NewState=State#state{wait=W,wsp=WSP},
	    {next_state,wait_timeout, NewState};
	{?CLASS1,Exitinfo} ->
	    ExitinfoList=case Exitinfo of
			     [] -> [];
			     Info -> [{info_tpi,Info}]
			 end,
	    queue_ack(?FALSE,?FALSE,ExitinfoList,State),
	    W=start_wait_timer(Bearer,{base,Uack},self()),
	    NewState=State#state{wait=W,ack_exitinfo=ExitinfoList,wsp=WSP},
	    {next_state,wait_timeout , NewState};
	{?CLASS2,_} ->
	    A=start_ack_timer(Bearer,{base,Uack},self()),
	    NewState=State#state{ack=A,wsp=WSP},
	    {next_state,result_wait , NewState}
    end;
invoke_resp_wait({tr_result_req,Data}, State) ->
    Bearer=State#state.bearer,    
    result(?FALSE,Data,State),
    R=start_retry_timer(Bearer,{base,State#state.uack},self()),
    State1=State#state{rcr=0,retry=R,sdu=Data},
    {next_state,result_resp_wait, State1};
invoke_resp_wait({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_listen(State);
invoke_resp_wait(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_listen(State);
invoke_resp_wait(#invoke_pdu{}, State) -> % Ignore this one
    {next_state,invoke_resp_wait , State};
invoke_resp_wait(timerTO_A, State) ->
    Bearer=State#state.bearer,    
    TCL=State#state.tcl,
    Uack=State#state.uack,
    AEC=State#state.aec,
    AEC_Max=wtp_common:max_ack_expirations(Bearer),
    if
	AEC<AEC_Max,Uack==?TRUE ->
	    A=start_ack_timer(Bearer,{base,Uack},self()),
	    State1=State#state{aec=AEC+1,ack=A},
	    {next_state, invoke_resp_wait, State1};
	AEC==AEC_Max,Uack==?TRUE ->
	    abort(?PROVIDER,?NORESPONSE,State),
	    next_state_listen(State);
	TCL==?CLASS1,Uack==?FALSE ->
	    queue_ack(?TRUE,?FALSE,[],State),
	    W=start_wait_timer(Bearer,{base,Uack},self()),
	    State1=State#state{wait=W},
	    {next_state, wait_timeout, State1};
	TCL==?CLASS2,Uack==?FALSE ->
	    ack(?TRUE,?FALSE,[],State),
	    {next_state, result_wait, State}
    end;
%% rcv_error
invoke_resp_wait(A,State) ->
    ?error("Event: ~p",[A],invoke_resp_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).

%% -----------------------------------------------------------------------------
%% >>>The result_wait state<<< (Class 2 only)
result_wait({tr_result_req,Data}, State) ->
    Bearer=State#state.bearer,    
    R=start_retry_timer(Bearer,{base,State#state.uack},self()),
    result(?FALSE,Data,State),
    State1=State#state{rcr=0,retry=R,sdu=Data},
    {next_state,result_resp_wait , State1};
result_wait(#invoke_pdu{rid=RID},State) ->
    if
	RID==?FALSE ->
	    ignore;
	RID==?TRUE,State#state.ack_sent==false ->
	    ignore;
	RID==?TRUE ->
	    ack(?TRUE,?TRUE,[],State)
    end,
    {next_state,result_wait, State};
result_wait({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_listen(State);
result_wait(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_listen(State);
result_wait(timerTO_A, State) ->
    ack(?TRUE,?FALSE,[],State),
    State1=State#state{ack_sent=true},
    {next_state, result_wait, State1};
%% rcv_error
result_wait(A,State) ->
    ?error("Event: ~p",[A],result_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).


%% -----------------------------------------------------------------------------
%% >>>The result_resp_wait state<<< (Class 2 only)
result_resp_wait({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_listen(State);
result_resp_wait(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_listen(State);
result_resp_wait(#ack_pdu{tidver=TIDver,tpilist=TPIList}, State) ->
    tr_result_cnf(TPIList,State),
    next_state_listen(State);
result_resp_wait(timerTO_R, State) ->
    RCR=State#state.rcr,
    MaxRCR=wtp_common:max_retransmissions(State#state.bearer),
    if
	RCR<MaxRCR ->
	    result(?TRUE,State#state.sdu,State),
	    R=start_retry_timer(State#state.bearer,
				{base,State#state.uack},self()),
	    State1=State#state{rcr=RCR+1,retry=R},
	    {next_state, result_resp_wait, State1};
	RCR==MaxRCR ->
	    tr_abort_ind(?PROVIDER,?NORESPONSE,State),
	    next_state_listen(State)
    end;
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that these events, in this state, is not part of the WTP specification
result_resp_wait(timerTO_A, State) ->
    Tpar=State#state.tpar,
    wtp_common:out_queue(Tpar),
    {next_state, result_resp_wait, State};
result_resp_wait(#invoke_pdu{}, State) -> % Ignore this one
    {next_state,result_resp_wait , State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% rcv_error
result_resp_wait(A,State) ->
    ?error("Event: ~p",[A],result_resp_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).

%% -----------------------------------------------------------------------------
%% >>>The wait_timeout state<<< (Class 1 only)
wait_timeout(#invoke_pdu{rid=RID,tpilist=TpiListIn},State) ->
    case {RID,TpiListIn} of
	{?FALSE,_} ->
	    ignore;
	{?TRUE,[]} ->
	    ack(RID,?FALSE,[],State);
	{?TRUE,ExitInfo} ->
	    ExitinfoList=State#state.ack_exitinfo,
	    ack(RID,?FALSE,ExitinfoList,State)
    end,
    {next_state, wait_timeout, State};
wait_timeout(#ack_pdu{rid=RID,tidver=?TRUE,tpilist=TpiListIn}, State) ->
    case {RID,TpiListIn} of
	{?FALSE,_} ->
	    ignore;
	{?TRUE,[]} ->
	    ack(RID,?FALSE,[],State);
	{?TRUE,ExitInfo} ->
	    ExitinfoList=State#state.ack_exitinfo,
	    ack(RID,?FALSE,ExitinfoList,State)
    end,
    {next_state, wait_timeout, State};
wait_timeout(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_listen(State);
wait_timeout(timerTO_W, State) ->
    next_state_listen(State);
wait_timeout({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_listen(State);
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WTP specification
wait_timeout(timerTO_A, State) ->
    Tpar=State#state.tpar,
    wtp_common:out_queue(Tpar),
    {next_state, wait_timeout, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
wait_timeout(A,State) ->
    ?error("Event: ~p",[A],wait_timeout),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_listen(State).

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {nextstate, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {nextstate, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

next_state_listen(State) ->
    wtp_db:reset_responder(State#state.tdb,self()),
    wtp_db:remove_transaction(State#state.tdb,{State#state.tpar,responder,
					       State#state.tid}),
    stop_timer(State#state.ack),
    stop_timer(State#state.retry),
    stop_timer(State#state.wait),
    {next_state,listen, State}.

%%% ----------------------------------------------------------------------------
%% Primitives used to access WTP on the "other side"
%% Default values for the result pdu are GTR=?TRUE, TTR=?TRUE, tpilist=[]
result(RID,Data,State) ->
    wtp_man:result(#result_pdu{rid=RID,data=Data,tid=State#state.tid},
		    State#state.wdp,State#state.tpar).

ack(RID,TIDver,TPIList,State) ->
    Data=wtp_pdu:encode_pdu(#ack_pdu{tidver=TIDver,rid=RID,tid=State#state.tid,
				     tpilist=TPIList}),
    wdp_man:unitdata_req(State#state.wdp,State#state.tpar,Data).


queue_ack(RID,TIDver,TPIList,State) ->
    Data=wtp_pdu:encode_pdu(#ack_pdu{tidver=TIDver,rid=RID,tid=State#state.tid,
				     tpilist=TPIList}),
    wtp_common:in_queue(wdp_man,State#state.wdp,State#state.tpar,?Ack,Data).

abort(AbortType,AbortReason,State) ->
    wtp_db:remove_transaction(State#state.tdb,
			      {State#state.tpar,responder,State#state.tid}),
    Data=wtp_pdu:encode_pdu(#abort_pdu{type=AbortType,reason=AbortReason,
				       tid=State#state.tid,tpilist=[]}),
    wdp_man:unitdata_req(State#state.wdp,State#state.tpar,Data).


%% Primitives used to access WSP on the "same side"
tr_abort_ind(AbortFrom,AbortReason,State) ->
    Reason=if
	       AbortFrom==?USER ->    {wsp,AbortReason};
	       AbortFrom==?PROVIDER ->{wtp,AbortReason}
	   end,
    wtp_db:remove_transaction(State#state.tdb,
			      {State#state.tpar,responder,State#state.tid}),
    abort_ind(State#state.wsp,self(),Reason).


tr_invoke_ind(State) ->
    A=wtp_common:start_ack_timer(State#state.bearer,
				 {base,State#state.uack},self()),
    invoke_ind(State#state.wsp,self(),State#state.tpar,
	       State#state.tcl,State#state.uack,
	       State#state.sdu),
    State#state{ack=A}.


tr_result_cnf(Tpilist,State) ->
    result_cnf(State#state.wsp,self(),wtp_pdu:lookup_tpi(info_tpi,Tpilist)).


