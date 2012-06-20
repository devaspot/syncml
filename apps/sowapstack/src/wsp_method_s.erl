%%% File    : wsp_method_s.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Server Method state machine 
%%% Created : 28 Aug 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_method_s).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_method_s.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").
 
%% Internal Component interface
-export([
	 pseudo_release/1,
	 pseudo_abort/2,
	 start_link/1,
	 stop/1]).

%% Internal gen_fsm callbacks
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3, code_change/4]).


%% Internal gen_fsm states
-export([holding/2,holding/3,
	 requesting/2,requesting/3,
	 processing/2,processing/3,
	 replying/2,replying/3]).

-include("wsp.hrl").
-include("wspif.hrl").

-record(state,{wtp,       % (pid) to WTP layer process
	       wsp,       % (pid) to WSP session process
	       sdb,       % (pid) to Session database
	       tpar,      % ({uint16,#address}) ApplicationId and DestAddress
	       env,       % (#env) (Negotiated) parameters for this session
	       pdu        % (#get or #post) Method request record
	      }).

%-------------------------------------------------------------------------------
% Maintenance
% Needs a manager of WSP sessions on both sides - Mobile and Server
start_link(Args)->
    case gen_fsm:start_link(?MODULE,{self(),Args},?START_OPTIONS) of
	{ok,Wsp} ->
	    Wsp;
	Error ->
	    ?error("Can't start WSP method, got ~p",[Error],start_link),
	    {error,cant_start_wsp_method}
    end.

%% Starts a database for the session data, for each WAP stack
init({WSPses,{Pdu,Env,WTPres,Tpar,Sdb}}) ->
    wsp_db:add_method(Sdb,{Tpar,WTPres},{self(),holding}),
    {ok,holding,#state{wtp=WTPres,wsp=WSPses,sdb=Sdb,tpar=Tpar,
		       pdu=Pdu,env=Env}}.


stop(WSPmet) ->
    gen_fsm:send_event(WSPmet,stop).

terminate(Reason,_StateName,_State) ->
    ?trace("WSP Method stopped:~w",[Reason],terminate).



%-------------------------------------------------------------------------------
%% API
%% Request from the WSP session process
pseudo_release(WSPmet) ->
    gen_fsm:send_event(WSPmet, pseudo_release).
pseudo_abort(WSPmet,Reason) ->
    gen_fsm:send_event(WSPmet,{pseudo_abort,Reason}).

%%------------------------------------------------------------------------------
%% The Server Method state machine

%% >>>The holding state<<<
holding(_Event,_,State) ->
    {reply, {error,invalid_command},holding,State}.

holding(pseudo_release,State) ->
    {HTTPMeth,HTTPCont}=extract_method(State#state.pdu),
    wsp_db:release_method(State#state.sdb,{State#state.tpar,State#state.wtp}),
    s_method_invoke_ind(HTTPMeth,HTTPCont,State),
    {next_state,requesting ,State};
holding({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    next_state_null(State);
holding({tr_abort_ind,_WTP,Info},State) ->
    if
	Info=={wsp,?DISCONNECT} ->
	    pseudo_disconnect(State);
	Info=={wsp,?SUSPEND} ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    next_state_null(State);
	true ->
	    next_state_null(State)
    end;
holding(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],holding),
    handle_illegal_event(State).


extract_method(#get{type=Method,uri=Uri,headers=Headers}) ->
    {Method,{Uri,Headers}};
extract_method(#post{type=Method,uri=Uri,headers=Headers,
		     contenttype=CT,data=Data}) ->
    {Method,{Uri,Headers,CT,Data}}.
    

%% >>>The requesting state<<<
requesting(method_invoke_res,_,State) ->
    tr_invoke_res(State),
    {reply,ok,processing ,State};
requesting(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
requesting(_Event,_,State) ->
    {reply, {error,invalid_command},requesting,State}.

requesting({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    s_method_abort_ind(Reason,State),
    next_state_null(State);
requesting({tr_abort_ind,_WTPres,Info},State) ->
    if
	Info=={wsp,?DISCONNECT} ->
	    pseudo_disconnect(State);
	Info=={wsp,?SUSPEND} ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    next_state_null(State);
	true ->
	    s_method_abort_ind(Info,State),
	    next_state_null(State)
    end;
requesting(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],requesting),
    handle_illegal_event(State).


%% >>>The processing state<<<
processing({method_result_req,{Status,Headers,CT,Data}},_,State) ->
    tr_result_req(#reply{status=Status,headers=Headers,
			 contenttype=CT,data=Data},
		  State),
    {reply, ok,replying, State};
processing(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
processing(_Event,_,State) ->
    {reply, {error,invalid_command},processing,State}.

processing({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    s_method_abort_ind(Reason,State),
    next_state_null(State);
processing({tr_abort_ind,_WTPres,Info},State) ->
    if
	Info=={wsp,?DISCONNECT} ->
	    pseudo_disconnect(State);
	Info=={wsp,?SUSPEND} ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    next_state_null(State);
	true ->
	    s_method_abort_ind(Info,State),
	    next_state_null(State)
    end;
processing(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],processing),
    handle_illegal_event(State).


%% >>>The replying state<<<
replying(method_abort_req,_,State) ->
    tr_abort_req(?PEERREQ,State),
    s_method_abort_ind(?USERREQ,State),
    next_state_null(State,ok);
replying(_Event,_,State) ->
    {reply, {error,invalid_command},replying,State}.

replying({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    s_method_abort_ind(Reason,State),
    next_state_null(State);
replying({tr_result_cnf,_WTPres,Exitinfo},State) ->
    case ((State#state.env)#env.cap)#cap.ackhead of
	true ->
	    s_method_result_cnf(Exitinfo,State);
	_ ->
	    s_method_result_cnf([],State)
    end,
    next_state_null(State);
replying({tr_abort_ind,_WTPres,Info},State) ->
    if
	Info=={wsp,?DISCONNECT} ->
	    pseudo_disconnect(State);
	Info=={wsp,?SUSPEND} ->
	    pseudo_suspend(State#state.sdb,State#state.tpar),
	    next_state_null(State);
	true ->
	    s_method_abort_ind(Info,State),
	    next_state_null(State)
    end;
replying(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],replying),
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


code_change(_OldVsn,StateName,State,_Extra)->
    {ok, StateName, State}.
%% .............................................................................
pseudo_disconnect(State) ->
    case wsp_db:lookup_session(State#state.sdb,State#state.tpar) of
	{ok,Spid} ->
	    ?debug("pseudo disconnecting ~p",[Spid],pseudo_disconnect),
	    wsp_db:remove_method(State#state.sdb,
				 {State#state.tpar,State#state.wtp}),
	    wsp_session_s:remove_method(State#state.wsp),
	    ?pseudo_disconnect(Spid),
	    {stop,normal,State};
	_ ->
	    {error,illegal_session}
    end.

pseudo_suspend(Sdb,Tpar) ->
    case wsp_db:lookup_session(Sdb,Tpar) of
	{ok,Spid} ->
	    wsp_session_s:request(Spid,pseudo_suspend);
	_ ->
	    {error,illegal_session}
    end.


next_state_null(State) ->
    wsp_db:remove_method(State#state.sdb,{State#state.tpar,State#state.wtp}),
    wsp_session_s:remove_method(State#state.wsp),
    {stop,normal,State}.

next_state_null(State,_Reply) ->
    wsp_db:remove_method(State#state.sdb,{State#state.tpar,State#state.wtp}),
    wsp_session_s:remove_method(State#state.wsp),
    {stop,normal,ok,State}.

%% =============================================================================
s_method_invoke_ind(HTTPReq,HTTPCont,State) ->
    method_invoke_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		      self(),HTTPReq,HTTPCont).
s_method_result_cnf(Exitinfo,State) ->
    method_result_cnf(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		      self(),Exitinfo).
s_method_abort_ind(Reason,State) ->
    method_abort_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		     self(),Reason).


tr_invoke_res(State) ->
    wtp_responder:invoke_res(State#state.wtp,self(),[]).

tr_result_req(Pdu,State) ->
    wsp_man:tr_result_req(State#state.wtp,Pdu,State#state.env,
			   ((State#state.env)#env.cap)#cap.client_sdu).

tr_abort_req(Reason,State) ->
    wtp_responder:abort_req(State#state.wtp,Reason).
