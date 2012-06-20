%%% File    : wsp_method_c.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Client Method state machine
%%% Created : 16 Sep 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_method_c).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_method_c.erl,v 1.1.1.1 2001/07/04 14:51:13 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:13 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% Internal Component interface
-export([request/3,request/2,
	 request_sync/2,request_sync/3,
	 pseudo_abort/2,
	 start_link/1,
	 stop/1]).

%% gen_fsm callbacks
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3, code_change/4]).

%% gen_fsm states
-export([completing/2,completing/3,
	 requesting/2,requesting/3,
	 waiting/2,waiting/3]).

-include("wsp.hrl").
-include("wspif.hrl").

-record(state,{wtp,       % (pid) to WTP initiator process
	       wsp,       % (pid) to WSP session process
	       sdb,       % (pid) to Session database
	       tpar,      % ({uint16,#address}) ApplicationId and DestAddress
	       env        % (#env) (Negotiated) parameters for this session
	      }).

%%% ----------------------------------------------------------------------------
%% Maintenance
start_link(Args)->
    case gen_fsm:start_link(?MODULE,{self(),Args},?START_OPTIONS) of
	{ok,Wsp} ->
	    Wsp;
	Error ->
	    ?error("Can't start WSP method, got ~p",[Error],start_link),
	    {error,cant_start_wsp_method}
    end.

%% Starts a database for the session data, for each WAP stack
init({WSPses,{Pdu,Env,WTPman,Tpar,Sdb}}) ->
    case wsp_man:tr_invoke_req(WTPman,self(),Tpar,?CLASS2,
			       Pdu,Env,(Env#env.cap)#cap.server_sdu) of
	{ok,WTPini} ->
	    wsp_db:add_method(Sdb,{Tpar,WTPini},{self(),released}),
	    {ok,requesting,#state{wtp=WTPini,wsp=WSPses,sdb=Sdb,tpar=Tpar,
				  env=Env
				 }};
	_Error ->
	    {stop,normal}
    end.


stop(WSPpid) ->
    gen_fsm:send_event(WSPpid,stop).

terminate(Reason,_StateName,_State) ->
    ?trace("WSP Method stopped:~w",[Reason],terminate).

code_change(_OldVsn, StateName, State, _Extra)->
    {ok, StateName, State}.

%-------------------------------------------------------------------------------
%% API
%% Request from the WSP session process
pseudo_abort(WSPmet,Reason) ->
    gen_fsm:send_event(WSPmet,{pseudo_abort,Reason}).
    

request_sync(WSPm,Atype,Content) ->
    gen_fsm:sync_send_event(WSPm, {Atype,Content}).
request_sync(WSPm,Atype) ->
    gen_fsm:sync_send_event(WSPm, Atype).
request(WSPm,Atype,Data) ->
    gen_fsm:send_event(WSPm, {Atype,Data}).
request(WSPm,Atype) ->
    gen_fsm:send_event(WSPm, Atype).


%%% ----------------------------------------------------------------------------
%%% The Client Method state machine

%% >>>The requesting state<<<
requesting(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
requesting(_Event,_,State) ->
    {reply, {error,invalid_command},requesting,State}.

requesting({pseudo_abort,Reason},State) ->
    ?debug("Got pseudo_abort ~p",[Reason],requesting),
    tr_abort_req(Reason,State),
    s_method_abort_ind(Reason,State),
    next_state_null(State);
requesting({tr_invoke_cnf,_,_},State) ->
    s_method_invoke_cnf(State),
    {next_state,waiting ,State};
requesting({tr_abort_ind,_WTPini,{_Type,Reason}},State) ->
    ?debug("Got tr_abort_ind ~p=~p",[Reason,?DISCONNECT],requesting),
    if
	Reason==?DISCONNECT ->
	    pseudo_disconnect(State);
	Reason==?SUSPEND ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    {next_state,requesting ,State};
	true ->
	    s_method_abort_ind(Reason,State),
	    next_state_null(State)
    end;
requesting(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],requesting),
    handle_illegal_event(State).

%%% ----------------------------------------------------------------------------
%%% >>>The waiting state<<<
waiting(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
waiting(_Event,_,State) ->
    {reply, {error,invalid_command},waiting,State}.

waiting({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State);
waiting({tr_result_ind,_WTPini,BinPdu},State) ->
    case catch wsp_man:tr_result_ind(
		  BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(?MRUEXCEEDED,State),
	    s_method_abort_ind(?MRUEXCEEDED,State),
	    next_state_null(State);
	#reply{status=Status,headers=Headers,contenttype=CT,data=Data} ->
	    s_method_result_ind({Status,Headers,CT,Data},State),
	    {next_state, completing, State};
	_Other ->
	    tr_abort_req(?WSP_PROTOERR,State),
	    s_method_abort_ind(?WSP_PROTOERR,State),
	    next_state_null(State)
    end;
waiting({tr_abort_ind,_WTPini,{_Type,Reason}},State) ->
    ?debug("Got tr_abort_ind ~p",[Reason],waiting),
    if
	Reason==?DISCONNECT ->
	    pseudo_disconnect(State);
	Reason==?SUSPEND ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    {next_state,waiting ,State};
	true ->
	    s_method_abort_ind(Reason,State),
	    next_state_null(State)
    end;
waiting(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],waiting),
    handle_illegal_event(State).


%%% ----------------------------------------------------------------------------
%%% >>>The completing state<<<
completing({method_result_res,Exitinfo},_,State) ->
    case ((State#state.env)#env.cap)#cap.ackhead of
	true ->
	    tr_result_res(Exitinfo,State);
	_ ->
	    tr_result_res([],State)
    end,
    next_state_null(State,ok);
completing(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
completing(_Event,_,State) ->
    {reply, {error,invalid_command},completing,State}.

completing({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State);
completing({tr_abort_ind,_WTPini,{_Type,Reason}},State) ->
    ?debug("Got tr_abort_ind ~p",[Reason],completing),
    if
	Reason==?DISCONNECT ->
	    pseudo_disconnect(State);
	Reason==?SUSPEND ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    {next_state,completing ,State};
	true ->
	    s_method_abort_ind(Reason,State),
	    next_state_null(State)
    end;
completing(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],completing),
    handle_illegal_event(State).


%%% ----------------------------------------------------------------------------
%% This is the catch all case in all gen_fsm's
%% Incoming events from WTP
handle_illegal_event(State) ->
    tr_abort_req(?WSP_PROTOERR,State),
    next_state_null(State).


%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

pseudo_disconnect(State) ->
    case wsp_db:lookup_session(State#state.sdb,State#state.tpar) of
	{ok,Spid} ->
	    ?debug("pseudo disconnecting ~p",[Spid],pseudo_disconnect),
	    wsp_db:remove_method(State#state.sdb,
				 {State#state.tpar,State#state.wtp}),
	    wsp_session_c:remove_method(State#state.wsp),
	    ?pseudo_disconnect(Spid),
	    {stop,normal,State};
	_ ->
	    {error,illegal_session}
    end.

pseudo_suspend(Sdb,Tpar) ->
    case wsp_db:lookup_session(Sdb,Tpar) of
	{ok,Spid} ->
	    wsp_session_c:request(Spid,pseudo_suspend);
	_ ->
	    {error,illegal_session}
    end.

next_state_null(State) ->
    wsp_db:remove_method(State#state.sdb,{State#state.tpar,State#state.wtp}),
    wsp_session_c:remove_method(State#state.wsp),
    {stop,normal,State}.

next_state_null(State,_Reply) ->
    wsp_db:remove_method(State#state.sdb,{State#state.tpar,State#state.wtp}),
    wsp_session_c:remove_method(State#state.wsp),
    {stop,normal,ok,State}.

%% =============================================================================
s_method_abort_ind(Reason,State) ->
    method_abort_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		     self(),Reason).
s_method_result_ind(HTTPCont,State) ->
    method_result_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		      self(),HTTPCont).
s_method_invoke_cnf(State) ->
    method_invoke_cnf(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		      self()).

tr_result_res(ExitInfo,State) ->
    ExitInfo2=wsp_bytecodes_headers:encode_headers(ExitInfo,State#state.env),
    wtp_initiator:result_res(State#state.wtp,ExitInfo2).

tr_abort_req(Reason,State) -> % Class 1 and 2 Abort
    wtp_initiator:abort_req(State#state.wtp,Reason).
