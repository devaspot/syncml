%%% File    : wap_stack_db.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Stack Database for data that should be accesible for ALL stacks
%%% Created : 27 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wap_stack_db).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wap_stack_db.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([
	 insert_stack/3,remove_stack/1,lookup_stack/1,print_stacks/0,
	 update_stack/2,find_stackref/1,
	 get_wsp_component/1,get_wtp_component/1,	 

	 print_config/0,lookup_config/1,
	 insert_app/2,remove_all_app/1,lookup_app/1,
	 start_link/0,stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wap_stack.hrl").
-include("wdp.hrl").

-define(SDBCALL,        {global,?MODULE}).
-define(CONFIG_KEYS,
	[
	 {stack_type,atom,server},     % Client or server stack
	 %% Bearer types supported by the stack
	 {supported_bearers,bearlist,[?ANY_ANY_IPv4]},
	 {wsp_use_uack,bool,?TRUE},% Set User Ack flag in Class 1 and 2 requests
	 {wsp_reuse_wtls,bool,?FALSE}, % Reuse WTLS session, when redirecting
	 {wsp_sessions,uint16,5},      % Max number of sessions.
	 {wsp_sessions_len,uint16,720},% Max session length in minutes.
	 {wtp_max_retrans,bearlist,[{?ANY_ANY_IPv4,3}]}, % Max retransmissions
	 {wtp_max_ackexp,bearlist,[{?ANY_ANY_IPv4,3}]},  % Max ack expirations
	 {wtp_retry_timer,bearlist,[{?ANY_ANY_IPv4,10}]},% Retry timer val
	 {wtp_ack_timer,bearlist,[{?ANY_ANY_IPv4,10}]},  % Retry ack val
	 {wtp_wait_timer,bearlist,[{?ANY_ANY_IPv4,10}]}, % Retry wait val
	 {wtp_segment,bool,?FALSE}  % Whether to use WTP segmentation
	]).

-record(state,{app_db,   % (ets) Maps a Stack reference with application (tpar)
	       stack_db, % (ets) Maps a Stack reference with a real stack (pid)
	       config_db % (ets) Configuration database
	      }).

%-------------------------------------------------------------------------------
% Interface
%% Stack database interface
insert_stack(Sref,Type,Stack) ->
    gen_server:call(?SDBCALL, {insert_stack,Sref,Type,Stack}).
update_stack(Sref,Comp) ->
    gen_server:call(?SDBCALL, {update_stack,Sref,Comp}).
remove_stack(Sref) ->
    gen_server:call(?SDBCALL, {remove_stack,Sref}).
lookup_stack(Sref) ->
    gen_server:call(?SDBCALL, {lookup_stack,Sref}).
find_stackref(AppRef) ->
    gen_server:call(?SDBCALL, {find_stackref,AppRef}).    
print_stacks() ->
    gen_server:call(?SDBCALL, print_stacks).

get_wsp_component(Sref) ->
    gen_server:call(?SDBCALL, {get_wsp_component,Sref}).
get_wtp_component(Sref) ->
    gen_server:call(?SDBCALL, {get_wtp_component,Sref}).



%% Configuration database interface
print_config() ->
    gen_server:call(?SDBCALL,print_config).
lookup_config(ConfigKey) ->
    gen_server:call(?SDBCALL,{lookup_config,ConfigKey}).


%% Port database interface
insert_app(Sref,AppInfo) ->
    gen_server:call(?SDBCALL,{insert_app,Sref,AppInfo}).
remove_all_app(Sref) ->
    gen_server:call(?SDBCALL,{remove_all_app,Sref}).
lookup_app(Sref) ->
    gen_server:call(?SDBCALL,{lookup_app,Sref}).


%%% -----------------------------------------------------------------------------
%%% Maintenance
%%% Database interface for handling of Stack references.
start_link() ->    
    case gen_server:start_link(?SDBCALL,?MODULE,[],?START_OPTIONS) of
	{ok,App} ->
	    App;
	_ ->
	    {error,cant_start_stack_db}
    end.

stop() ->
    gen_server:call(?SDBCALL, stop).

%% .............................................................................
init(_) ->
    Config_db=ets:new(config_db,[set, private]),
    read_configs(Config_db),
    ?trace("Started WAP stack database ok",[],init),

    {ok,#state{
	       app_db=ets:new(app_db,[set, private]),
	       stack_db=ets:new(stack_db,[set, private]),
	       config_db=Config_db}}.

terminate(Reason, State) ->
    ets:delete(State#state.app_db),
    ets:delete(State#state.stack_db),
    ets:delete(State#state.config_db),
    ?trace("Stopped WAP stack database:~p",[Reason],terminate).


code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% =============================================================================
%% Maintenance
handle_call(stop, _, Tab) ->
    {stop, normal, ok, Tab};

%% =============================================================================
%% Configuration Database
handle_call(print_config,_, State) ->
    print_configs(State#state.config_db),
    {reply,ok,State};
handle_call({lookup_config,ConfigKey},_, State) ->
    Ans=case ets:lookup(State#state.config_db,ConfigKey) of
	    [{_,_,ConfigVal}] ->
		{ok,ConfigVal};
	    [] ->
		{error,configdata_notfound}
	end,
    {reply,Ans,State};

%% .............................................................................
%% Stack Database
handle_call({insert_stack,Sref,Type,Stack},_, State) when is_atom(Type),
							  is_tuple(Stack) ->
    ets:insert(State#state.stack_db,{Sref,{Type,Stack}}),
    {reply,ok,State};
handle_call({update_stack,Sref,{Comp,Pid}},_, State) ->
    Ans=case ets:lookup(State#state.stack_db,Sref) of
	    [{_,{wspCL_wdp,{Wsp,_Wdp}}}] ->
		Stack=case Comp of
			  wdp -> {Wsp,Pid}
		      end,
		ets:insert(State#state.stack_db,{Sref,{wspCL_wdp,Stack}});
	    [{_,{wspCL_wtls_wdp,{Wsp,Wtls,Wdp}}}] ->
		Stack=case Comp of
			  wtls ->{Wsp,Pid,Wdp};
			  wdp -> {Wsp,Wtls,Pid}
		      end,
		ets:insert(State#state.stack_db,{Sref,{wspCL_wtls_wdp,Stack}});
	    [{_,{wspCO_wtp_wdp,{Wsp,Wtp,Wdp}}}] ->
		Stack=case Comp of
			  wtp -> {Wsp,Pid,Wdp};
			  wdp -> {Wsp,Wtp,Pid}
		      end,
		ets:insert(State#state.stack_db,{Sref,{wspCO_wtp_wdp,Stack}});
	    [{_,{wspCO_wtp_wtls_wdp,{Wsp,Wtp,Wtls,Wdp}}}] ->
		Stack=case Comp of
			  wtp -> {Wsp,Pid,Wtls,Wdp};
			  wtls ->{Wsp,Wtp,Pid,Wdp};
			  wdp -> {Wsp,Wtp,Wtls,Pid}
		      end,
		ets:insert(State#state.stack_db,
			   {Sref,{wspCO_wtp_wtls_wdp,Stack}});
	    _Error ->
		{error,no_valid_stack}
	end,
    {reply,Ans,State};
handle_call({lookup_stack,Sref},_,State) ->
    Ans=case ets:lookup(State#state.stack_db,Sref) of
	    [{_,Stack}] ->
		Stack;
	    [] ->
		{error,stack_notfound}
	end,
    {reply,Ans,State};
handle_call({remove_stack,Sref},_,State) ->
    ets:delete(State#state.stack_db,Sref),
    ets:delete(State#state.app_db,Sref),
    {reply,ok,State};
handle_call(print_stacks,_, State) ->
    L=case ets:tab2list(State#state.stack_db) of
	  [] ->  "No active";
	  A ->   A
      end,
    io:format("Running WAP Stacks: ~p~n",[L]),
    {reply,ok,State};
handle_call({get_wsp_component,Sref},_,State) ->
    Ans=case ets:lookup(State#state.stack_db,Sref) of
	    [{_,{wspCL_wdp,{Wsp,_}}}] ->
		{ok,Wsp};
	    [{_,{wspCL_wtls_wdp,{Wsp,_,_}}}] ->
		{ok,Wsp};
	    [{_,{wspCO_wtp_wdp,{Wsp,_,_}}}] ->
		{ok,Wsp};
	    [{_,{wspCO_wtp_wtls_wdp,{Wsp,_,_,_}}}] ->
		{ok,Wsp};
	    _ ->
		?warning("Couldn't find wsp component",[],handle_call),
		{error,no_valid_wsp_component}
	end,
    {reply,Ans,State};
handle_call({get_wtp_component,Sref},_,State) ->
    Ans=case ets:lookup(State#state.stack_db,Sref) of
	    [{_,{wspCO_wtp_wdp,{_,Wtp,_}}}] ->
		{ok,Wtp};
	    [{_,{wspCO_wtp_wlts_wdp,{_,Wtp,_,_}}}] ->
		{ok,Wtp};
	    _ ->
		?warning("Couldn't find wtp component",[],handle_call),
		{error,no_valid_wtp_component}
	end,
    {reply,Ans,State};


%% .............................................................................
%% AppInfo is a {{Address,Port,Bearer},Application} 
handle_call({insert_app,Sref,AppInfo},_,State) ->
    case ets:lookup(State#state.app_db, Sref) of
	[] ->
	    true=ets:insert(State#state.app_db, {Sref,[AppInfo]}),
	    {reply,ok,State};
	[{_,AppList}] ->
	    true=ets:insert(State#state.app_db, {Sref,[AppInfo|AppList]}),
	    {reply,ok,State}
    end;
handle_call({lookup_app,Sref},_,State) ->
    case ets:lookup(State#state.app_db, Sref) of
	[] ->
	    {reply,[],State};
	[{_,AppList}] ->
	    {reply,AppList,State}
    end;

%% Returns a tuple {ok,{StRef,Tpar}} or {error,Reason}
handle_call({find_stackref,AppRef},_,State) ->
    Ans=check_stack_applist(AppRef,ets:tab2list(State#state.app_db)),
    {reply,Ans,State};
handle_call({remove_all_app,Sref},_,State) ->
    ets:delete(State#state.app_db,Sref),
    {reply,ok,State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%% ----------------------------------------------------------------------------
check_stack_applist(_,[]) ->
    {error,not_found};
check_stack_applist(AppRef,[{StRef,AppList}|L]) ->
    case check_applist(AppRef,StRef,AppList) of
	{ok,Ans} ->
	    {ok,Ans};
	not_found ->
	    check_stack_applist(AppRef,L)
    end.

check_applist(_,_,[]) ->
    not_found;
check_applist(AppRef,StRef,[{Tpar,{AppRef,_}}|_AppList]) ->
    {ok,{StRef,Tpar}};
check_applist(AppRef,StRef,[_|AppList]) ->
    check_applist(AppRef,StRef,AppList).


%%% Read in Configuration from Application environment variables
read_configs(Config_db) ->
    read_config(?CONFIG_KEYS,Config_db).

read_config([],_Config_db) ->
    ok;
read_config([{ConfigKey,Type,Default}|Rest],Config_db) ->
    case application:get_env(wap_stack,ConfigKey) of
	undefined ->
	    ets:insert(Config_db,{ConfigKey,Type,Default}),
	    read_config(Rest,Config_db);
	{ok,ConfigVal} when Type==bool ->
	    ets:insert(Config_db,{ConfigKey,
				  Type,wsp_bytecodes:bool_to_bin(ConfigVal)}),
	    read_config(Rest,Config_db);
	{ok,ConfigVal} when Type==bearlist ->
	    ConfigVal2=find_bearervals(ConfigVal),
	    ets:insert(Config_db,{ConfigKey,Type,ConfigVal2}),
	    read_config(Rest,Config_db);
	{ok,ConfigVal} ->
	    ets:insert(Config_db,{ConfigKey,Type,ConfigVal}),
	    read_config(Rest,Config_db)
    end.

find_bearervals([]) ->
    [];
find_bearervals([{Bearer,Val}|ConfigVal]) ->
    Bearer2=find_bearerval(Bearer),
    [{Bearer2,Val}|find_bearervals(ConfigVal)];
find_bearervals([Bearer|ConfigVal]) ->
    Bearer2=find_bearerval(Bearer),
    [Bearer2|find_bearervals(ConfigVal)].

find_bearerval(any_any_ipv4) -> ?ANY_ANY_IPv4;
find_bearerval(gsm_sms_gsmmsisdn) -> ?GSM_SMS_GSMMSISDN.
    

print_configs(Config_db) ->
    io:format("Configuration data:~n",[]),
    print_config(?CONFIG_KEYS,Config_db).

print_config([],_Config_db) ->
    ok;
print_config([{ConfigKey,_,_}|Rest],Config_db) ->
    case ets:lookup(Config_db,ConfigKey) of
	[] ->
	    io:format("~p : ~p~n",[ConfigKey,undefined]),
	    print_config(Rest,Config_db);
	[{_,_,ConfigVal}] ->
	    io:format("~p : ~p~n",[ConfigKey,ConfigVal]),
	    print_config(Rest,Config_db)
    end.

