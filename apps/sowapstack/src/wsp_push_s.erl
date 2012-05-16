%%% File    : wsp_push_s.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Server Push state machine
%%% Created : 28 Aug 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_push_s).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_push_s.erl,v 1.1.1.1 2001/07/04 14:51:14 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:14 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% Internal Component interface
-export([request/3,
	 start_link/1,
	 stop/1]).


%% Internal gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
    case gen_server:start_link(?MODULE,{self(),Args},?START_OPTIONS) of
	{ok,Wsp} ->
	    Wsp;
	Error ->
	    ?error("Can't start WSP push, got ~p",[Error],start_link),
	    {error,cant_start_wsp_push}
    end.

%% Starts a database for the session data, for each WAP stack
init({WSPses,{Pdu,Env,WTPman,Tpar,Sdb}}) ->
    case wsp_man:tr_invoke_req(WTPman,self(),Tpar,?CLASS1,
				 Pdu,Env,(Env#env.cap)#cap.client_sdu) of
	{ok,WTPini} ->
	    wsp_db:add_push(Sdb,{Tpar,WTPini},{self(),?ConfirmedPush}),
	    ?trace("WSP push started ok",[],init),
	    {ok,#state{wtp=WTPini,wsp=WSPses,sdb=Sdb,tpar=Tpar,env=Env }};
	Error ->
	    {stop,normal}
    end.

stop(WSPpid) ->
    gen_server:call(WSPpid,stop).

terminate(Reason,State) ->
    ok.


%-------------------------------------------------------------------------------
%% API

%% Request from a WSP session
request(Ppid,Atype,Data) ->
    gen_server:cast(Ppid,{Atype,Data}).


%-------------------------------------------------------------------------------
% The Server Push state machine

%%% >>>The null state<<<
%% The NULL state in the client, waiting for a confirmed_push request from the
%% application, is handled in wsp_session_s


%% >>>The pushing state<<<
handle_cast({pseudo_abort,Reason},State) ->
    tr_abort_req(Reason,State),
    push_abort_ind(Reason,State),
    next_state_null(State);
handle_cast({tr_invoke_cnf,Exitinfo},State) ->
    case ((State#state.env)#env.cap)#cap.ackhead of
	true ->
	    confirmed_push_cnf(Exitinfo,State);
	_ ->
	    confirmed_push_cnf([],State)
    end,
    next_state_null(State);
handle_cast({tr_abort_ind,Reason},State) ->
    case Reason of
	?DISCONNECT ->
	    %% wsp_db:remove_push(State#state.sdb,{Tpar,Tid}),
	    %% disconnect_session(), OBS OBS OBS
	    {noreply ,State};
	?SUSPEND ->
	    %% wsp_db:remove_push(State#state.sdb,{Tpar,Tid}),
	    %% suspend_session(), OBS OBS OBS
	    {noreply ,State};
	_ ->
	    push_abort_ind(Reason,State),
	    next_state_null(State)
    end;
handle_cast(A,State) ->
    tr_abort_req(?WSP_PROTOERR,State),
    next_state_null(State).



%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, _, State) ->
    {stop, normal, State}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

next_state_null(State) ->
    wsp_db:remove_push(State#state.sdb,{State#state.tpar,State#state.wtp}),
    wsp_session_s:remove_push(State#state.wsp),
    {stop,normal,State}.

%% =============================================================================
tr_abort_req(Reason,State) ->
    wtp_initiator:abort_req(State#state.wtp,Reason).


push_abort_ind(Reason,State) ->
    push_abort_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		   self(),Reason).

confirmed_push_cnf(Exitinfo,State) ->
    confirmed_push_cnf(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		       self(),Exitinfo).

