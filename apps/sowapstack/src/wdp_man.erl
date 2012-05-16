%%% File    : wdp_man.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WDP and Bearer Adaptation manager
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wdp_man).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wdp_man.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").
 
-export([start_link/1,stop/1,app_reg/3,app_unreg/2]).

%% Internal gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("wdp.hrl").
-include("wdpif.hrl").

-record(state,{
	  above,    % (pid) to the user of WDP (normally WTP or WSP)
	  appref_db % ([{AppRef,Soc},..]) AppRef/Socket relations in this stack
	 }).

%-------------------------------------------------------------------------------
% Maintenance
start_link(Args) ->
    gen_server:start_link(?MODULE,Args,?START_OPTIONS).
stop(Wdp) ->
    gen_server:call(Wdp,stop).
app_reg(Wdp,AppRef,UAddr) ->
    gen_server:call(Wdp, {app_reg,AppRef,UAddr}).
app_unreg(Wdp,AppRef) ->
    gen_server:call(Wdp, {app_unreg,AppRef}).

%% .............................................................................
init({Sref,Above_pid}) ->
    AppRef_db=ets:new(appref_db,[set, private]),
    check_registered_applications(Sref,AppRef_db),
    ?trace("Started WDP manager ok",[],init),
    {ok,#state{above=Above_pid,
	       appref_db=AppRef_db}}.

terminate(Reason,State) ->
    ?trace("Stopped WDP manager:~p",[Reason],terminate).


%% Only do this during restart of the stack. Note that there cannot be any
%% applications registred during startup of the stack.
%% lookup_app returns a list with {{Address,Port,Bearer},{AppRef,App}} tuples
check_registered_applications(Sref,AppRef_db) ->
    lists:foreach(
      fun({{Address,Port,?ANY_ANY_IPv4},{AppRef,_}}) ->
	      open_port(Address,Port,AppRef,AppRef_db);
	 ({{Address,Port,?GSM_SMS_GSMMSISDN},{AppRef,_}}) ->
	      ok
      end,
      wap_stack_db:lookup_app(Sref)).


%-------------------------------------------------------------------------------
%% Call back functions

%% Allow for multiple local (source) addresses associated with this stack.
%% Thus the same port number can be opened a number of times here.
%% However, in the IP case, this is currently not possible to do in Erlang
handle_call({app_reg,AppRef,{Address,Port,Bearer}},_,State) ->
    case Bearer of
	?GSM_SMS_GSMMSISDN ->
	    {reply,{error,sms_not_a_valid_bearer},State};
	?ANY_ANY_IPv4 ->
	    Ans=open_port(Address,Port,AppRef,State#state.appref_db),
	    {reply,Ans,State};
	_ ->
	    {reply,{error,not_a_valid_bearer},State}
    end;

%% Addr is a {Address,Port,Bearer} tuple
handle_call({app_unreg,AppRef},_,State) -> 
    case ets:lookup(State#state.appref_db,AppRef) of
	[{{_,_,?GSM_SMS_GSMMSISDN},Handle}] ->
	    ets:delete(State#state.appref_db,AppRef),
	    {reply,ok,State};
	[{{_,_,?ANY_ANY_IPv4},Socket}] ->
	    case gen_udp:close(Socket) of
		{error, Reason} ->
		    {reply,{error,Reason},State};
		ok ->
		    ets:delete(State#state.appref_db,AppRef),
		    {reply,ok,State}
	    end
    end,
    {reply,ok,State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State}.


handle_cast({unitdata_req,{AppRef,{Da,Dp,Db}},Data},State) ->
    Tpar={AppRef,{Da,Dp,Db}},
    ?debug("~w on ~w:~w",[Data,Da,Dp],unitdata_req),
    case ets:lookup(State#state.appref_db,AppRef) of
	[]  ->
	    error_ind(State#state.above,Tpar,not_registered);
	[{_,Handle}] when Db==?GSM_SMS_GSMMSISDN ->
	    ?error("GSM SMS GSM_MSISDN not supported~n",[],handle_cast),
	    error_ind(State#state.above,Tpar,sms_not_supported);
	[{_,Socket}] when Db==?ANY_ANY_IPv4 ->
	    case gen_udp:send(Socket,Da,Dp,[Data]) of
		ok ->
		    ok;
		{error,Reason} ->
		    error_ind(State#state.above,Tpar,Reason)
	    end;
	Error ->
	    ?error("Sending on unsupported bearer",[],handle_cast),
	    error_ind(State#state.above,Tpar,unsupported_bearer)
    end,
    {noreply,State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({udp,Socket,Sa,Sp,BinPdu}, State) ->
    case ets:match_object(State#state.appref_db, {'_',Socket}) of
	[{AppRef,_}] ->
	    Tpar={AppRef,{Sa,Sp,?ANY_ANY_IPv4}},
	    unitdata_ind(State#state.above,Tpar,BinPdu)
    end,
    {noreply,State};
handle_info(normal, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?error("ERROR: ~p",[Info],handle_info),
    {noreply, State}.


%% =============================================================================
%% Opens a Port for a UDP (IPv4) connection
open_port(Address,Port,AppRef,UAddr_db) ->
    case gen_udp:open(Port, [binary]) of 
	{error, Reason} ->
	    {error, Reason};
	{ok, Socket} ->
	    ets:insert(UAddr_db,{AppRef,Socket}),
	    {ok,Socket}
    end.
