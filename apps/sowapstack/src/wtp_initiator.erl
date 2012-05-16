%%% File    : wtp_initiator.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WTP Initiator
%%% Created : 28 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wtp_initiator).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wtp_initiator.erl,v 1.2 2001/07/10 12:43:26 johblo Exp $ ').
-modified('$Date: 2001/07/10 12:43:26 $ ').
-modified_by('$Author: johblo $ ').
-vsn("1").

%% Internal Component interface
-export([tr_invoke_req/6,
	 request/2,
	 start/1,stop/1]).

%% Internal gen_fsm callbacks
-export([null/2,result_wait/2, result_resp_wait/2, wait_timeout/2,
	 init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3]).

-import(wtp_common,[start_ack_timer/3,start_retry_timer/3,start_wait_timer/3,
		  stop_timer/1]).

-include("wtp.hrl").
-include("wtpif.hrl").

-record(state,{wsp,     % (pid) Reference to application above (WSP)
	       wdp,     % (pid) Reference to application below (WDP)
	       tpar,    % (tuple) {Appref,DestAddress} tuple
	       bearer,  % (int) Bearer to send messages on (from wdp.hrl)
	       tdb,     % (pid) Transaction database
	       tid,     % (int) The Tid to use in all PDUs in this transaction
	       tidnew,  % (bool) True if the TidNew flag has been set
	       rcr,     % (int) Number of retransmissions performed
	       aec,     % (int) Number of expired acks performed
	       ack_tidok, % (bool) True if an Ack(TidOk) has been sent
	       holdon,  % (bool) True if HoldOn ack has been received
	       uack,    % (bool) True if User Ack has been requested
	       tcl,     % (0-2) Class of this transaction
	       sdu,         % (binary) WSP Session data unit
	       ack_exitinfo,% (list) Exitinfo in the Ack on the result (Class 2)
	       ack,         % (timer) Ack timer
	       retry,       % (timer) Retry timer
	       wait         % (timer) Wait timer
	      }).

%-------------------------------------------------------------------------------
%% API
tr_invoke_req(WTPini,Tpar,Content,WSP,WSPdata,WDP) ->
    gen_fsm:send_event(WTPini,{tr_invoke_req,Tpar,Content,WSP,WSPdata,WDP}).



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
	    throw({error,cant_start_wtp_initiator})
    end.


stop(Wtp) ->
    gen_server:call(Wtp,stop).


%% .............................................................................
init(Tdb) ->
    {ok,null,#state{tdb=Tdb}}.

terminate(Reason,StateName,State) ->
    ok.

%% -----------------------------------------------------------------------------
%% >>>The null state<<<
null({tr_invoke_req,Tpar,{TID,TIDNew,Uack,TCL},WSP,WSPdata,WDP},State) ->
    ?debug("TIDVER:tr_invoke_req Tid=~p,TidNew=~p",[TID,TIDNew],null),
    {_,{_,_,Bearer}}=Tpar,
    wtp_db:add_transaction(State#state.tdb,{Tpar,initiator,TID},
			   {self(),TIDNew}),
    State1=State#state{wsp=WSP,wdp=WDP,tpar=Tpar,
		       bearer=Bearer,
		       tid=TID,tidnew=TIDNew,
		       uack=Uack,
		       tcl=TCL,
		       rcr=0,aec=0,
		       ack_tidok=false,
		       holdon=false,
		       sdu=WSPdata,
		       ack_exitinfo=undef,ack=undef,retry=undef,wait=undef
		      },
    invoke(#invoke_pdu{tid=TID,tidnew=TIDNew,uack=Uack,tcl=TCL,data=WSPdata},
	   State1),
    {next_state,result_wait, State1};
null(timerTO_A, State) ->
    {next_state,null, State}.

%%% ----------------------------------------------------------------------------
%% >>>The result_wait state<<<
result_wait({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_null(State);
result_wait(#ack_pdu{tidver=TIDver,tpilist=TPIList}, State) ->
    Bearer=State#state.bearer,
    case {State#state.tcl,TIDver} of
	{?CLASS2,?FALSE} ->
	    stop_timer(State#state.retry),
	    tr_invoke_cnf([],State),
	    State1=State#state{holdon=true},
	    {next_state, result_wait, State1};
	{?CLASS1,?FALSE} ->
	    Exitinfo=wtp_pdu:lookup_tpi(info_tpi,TPIList),
	    stop_timer(State#state.retry),
	    tr_invoke_cnf(Exitinfo,State),
	    next_state_null(State);
	{_,?TRUE} -> % Tid verification with no additional TPIs or Data
	    Uack=State#state.uack,
	    ack(?FALSE,?TRUE,[],State),
	    stop_timer(State#state.retry),
	    R=start_retry_timer(Bearer,{base,Uack},self()),
	    State1=State#state{rcr=State#state.rcr+1,retry=R},
	    {next_state, result_wait, State1}
    end;
result_wait(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_null(State);
result_wait(timerTO_R, State) ->
    Bearer=State#state.bearer,    
    RCR=State#state.rcr,
    Uack=State#state.uack,
    Ack_Tidok=State#state.ack_tidok,
    MaxRCR=wtp_common:max_retransmissions(Bearer),
    if
	RCR<MaxRCR,Ack_Tidok==true ->
	    R=start_retry_timer(Bearer,{base,Uack},self()),
	    State1=State#state{rcr=RCR+1,retry=R},
	    ack(?TRUE,?TRUE,[],State),
	    {next_state, result_wait, State1};
	RCR<MaxRCR ->
	    R=start_retry_timer(Bearer,{base,Uack},self()),
	    State1=State#state{rcr=RCR+1,retry=R},
	    invoke(#invoke_pdu{rid=?TRUE,
			       tid=State#state.tid,tidnew=State#state.tidnew,
			       uack=Uack,tcl=State#state.tcl,
			       data=State#state.sdu},
		   State),
	    {next_state, result_wait, State1};
	RCR==MaxRCR ->
	    tr_abort_ind(?PROVIDER,?NORESPONSE,State),
	    next_state_null(State)
    end;
result_wait(#result_pdu{data=Data}, State) ->
    Bearer=State#state.bearer,    
    Tid=State#state.tid,
    TCL=State#state.tcl,
    HoldOn=State#state.holdon,
    Uack=State#state.uack,
    if
	HoldOn==true,TCL==?CLASS2 ->
	    stop_timer(State#state.retry),
	    tr_result_ind(Data,State),
	    A=start_ack_timer(Bearer,{base,Uack},self()),
	    State1=State#state{ack=A},
	    {next_state, result_resp_wait, State};
	HoldOn==false,TCL==?CLASS2 ->
	    stop_timer(State#state.retry),
	    tr_invoke_cnf([],State),
	    tr_result_ind(Data,State),
	    A=start_ack_timer(Bearer,{base,Uack},self()),
	    State1=State#state{ack=A},
	    {next_state, result_resp_wait, State};
	true ->
	    State1=State,
	    {next_state, result_wait, State}	    
    end;
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that these events, in this state, is not part of the WTP specification
result_wait({tr_result_res,_}, State) ->
    {next_state,result_wait, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% rcv_error
result_wait(Event,State) ->
    ?error("Event: ~p",[Event],result_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_null(State).


%% -----------------------------------------------------------------------------
%% >>>The result_resp_wait state<<<
result_resp_wait({tr_result_res,Exitinfo}, State) ->
    Bearer=State#state.bearer,
    Uack=State#state.uack,
    ExitinfoList=case Exitinfo of
		     [] -> [];
		     Info -> [{info_tpi,Info}]
		 end,
    queue_ack(?FALSE,?FALSE,ExitinfoList,State),
    W=start_wait_timer(Bearer,{base,Uack},self()),
    State1=State#state{wait=W,ack_exitinfo=ExitinfoList},
    {next_state, wait_timeout, State1};
result_resp_wait(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_null(State);
result_resp_wait({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_null(State);
result_resp_wait(#result_pdu{}, State) -> % ignore
    {next_state, result_resp_wait, State};
result_resp_wait(timerTO_A, State) ->
    Bearer=State#state.bearer,    
    Uack=State#state.uack,
    AEC=State#state.aec,
    AEC_Max=wtp_common:max_ack_expirations(Bearer),
    if
	AEC<AEC_Max,Uack==?TRUE ->
	    A=start_ack_timer(Bearer,{base,Uack},self()),
	    State1=State#state{aec=AEC+1,ack=A},
	    {next_state, result_resp_wait, State1};
	AEC==AEC_Max,Uack==?TRUE ->
	    abort(?PROVIDER,?NORESPONSE,State),
	    next_state_null(State);
	Uack==?FALSE ->
	    queue_ack(?TRUE,?FALSE,[],State),
	    W=start_wait_timer(Bearer,{base,Uack},self()),
	    State1=State#state{wait=W},
	    {next_state, wait_timeout, State1}
    end;
%% rcv_error
result_resp_wait(A,State) ->
    ?error("Event: ~p",[A],result_resp_wait),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_null(State).


%% -----------------------------------------------------------------------------
%% >>>The wait_timeout state<<<
wait_timeout(#result_pdu{rid=RID,tpilist=TPIListIn}, State) ->
    case {RID,TPIListIn} of
	{?FALSE,_} ->
	    {next_state, wait_timeout, State};
	{?TRUE,[]} ->
	    ack(?FALSE,?FALSE,[],State),
	    State1=State#state{ack_tidok=true},
	    {next_state, wait_timeout, State1};
	{?TRUE,ExitInfo} ->
	    ExitinfoList=State#state.ack_exitinfo,
	    ack(?FALSE,?FALSE,ExitinfoList,State),
	    State1=State#state{ack_tidok=true},
	    {next_state, wait_timeout, State1}
    end;
wait_timeout(#abort_pdu{type=AbortType,reason=AbortReason}, State) ->
    tr_abort_ind(AbortType,AbortReason,State),
    next_state_null(State);
wait_timeout(timerTO_W, State) ->
    next_state_null(State);
wait_timeout({tr_abort_req,Reason}, State) ->
    abort(?USER,Reason,State),
    next_state_null(State);
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WTP specification
wait_timeout({tr_result_res,_}, State) ->
    {next_state,wait_timeout, State};
wait_timeout(timerTO_A, State) ->
    Tpar=State#state.tpar,
    wtp_common:out_queue(Tpar),
    {next_state, wait_timeout, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% rcv_error
wait_timeout(A,State) ->
    ?error("Event: ~p",[A],wait_timeout),
    abort(?PROVIDER,?WTP_PROTOERR,State),
    tr_abort_ind(?PROVIDER,?WTP_PROTOERR,State),
    next_state_null(State).


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
next_state_null(State) ->
    wtp_db:remove_transaction(State#state.tdb,
			      {State#state.tpar,initiator,State#state.tid}),
    wtp_db:reset_initiator(State#state.tdb,self()),
    stop_timer(State#state.ack),
    stop_timer(State#state.retry),
    stop_timer(State#state.wait),
    {next_state,null, State}.

%% -----------------------------------------------------------------------------
%% Primitives used to access WTP on the "other side"
invoke(Pdu,State) ->
    wtp_man:invoke(Pdu,State#state.wdp,State#state.tpar).

ack(RID,TIDver,TPIList,State) ->
    Data=wtp_pdu:encode_pdu(#ack_pdu{tidver=TIDver,rid=RID,tid=State#state.tid,
				     tpilist=TPIList}),
    wdp_man:unitdata_req(State#state.wdp,State#state.tpar,Data).


queue_ack(RID,TIDver,TPIList,State) ->
    Data=wtp_pdu:encode_pdu(#ack_pdu{tidver=TIDver,rid=RID,tid=State#state.tid,
				     tpilist=TPIList}),
    wtp_common:in_queue(wdp_man,State#state.wdp,State#state.tpar,?Ack,Data).

abort(AbortType,AbortReason,State) ->
    Data=wtp_pdu:encode_pdu(#abort_pdu{type=AbortType,reason=AbortReason,
				       tid=State#state.tid,tpilist=[]}),
    wdp_man:unitdata_req(State#state.wdp,State#state.tpar,Data).



%% Primitives used to access WSP on the "same side"
%% TR-Result.ind
tr_result_ind(Data,State) ->
    result_ind(State#state.wsp,self(),Data).

%% TR-Abort.ind
tr_abort_ind(AbortFrom,AbortReason,State) ->
    Reason=if
	       AbortFrom==?USER ->    {wsp,AbortReason};
	       AbortFrom==?PROVIDER ->{wtp,AbortReason}
	   end,
    ?debug("Send abort to ~p",[State#state.wsp],tr_abort_ind),
    abort_ind(State#state.wsp,self(),Reason).

%% TR-Invoke.cnf
tr_invoke_cnf(Tpilist,State) ->
    invoke_cnf(State#state.wsp,self(),wtp_pdu:lookup_tpi(?TpiInfo,Tpilist)).


