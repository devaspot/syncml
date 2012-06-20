%%% File    : wtp_db.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Local WTP component database interfaces.
%%% Created : 30 Apr 2001 by Johan Blom <johblo@dragon.cellpt.se>

-module(wtp_db).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wtp_db.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").


%% External exports
-export([
	 add_transaction/3,remove_transaction/2,
	 lookup_tid/2,lookup_unidata_tid/2,

	 add_responder/2,reset_responder/2,get_idle_responder/1,
	 add_initiator/2,reset_initiator/2,get_idle_initiator/1,

	 get_last_tid/2,set_last_tid/3,
	 get_last_failed/2,set_last_failed/3,
	 get_gentid/2,set_gentid/3,

	 print_tdb/2,active_transactions/1,
	 start/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wtp.hrl").

-record(state,{
	  tdb,    % (ets) Transaction database searchable on a Tid
	  respdb, % (ets) Responder database searchable on Process state
	  initdb, % (ets) Initiator database searchable on Process state
	  last_tid,% (ets) Tpar -> uint16, Last received Tid (responder)
	  last_failed, % (ets) Tpar -> bool, True if 3way handshake (initiator)
	  gentid % (ets) Tpar -> uint16, Next tid to use (initiator)
	 }).

-record(transaction,{
	  tpid,      % (pid) WTP initiator/responder process
	  tidnew_set % (bool) True if tidnew flag was set, ie request for 3wh
	 }).

%-------------------------------------------------------------------------------
% Interface
%% Transaction database interface
add_transaction(Tdbp,Ref,Entry) ->
    gen_server:call(Tdbp,{add_transaction,Ref,Entry}).
remove_transaction(Tdbp,Ref) ->
    gen_server:call(Tdbp,{remove_transaction,Ref}).
lookup_tid(Tdbp,Ref) ->
    gen_server:call(Tdbp,{lookup_tid,Ref}).    
lookup_unidata_tid(Tdbp,Ref) ->
    gen_server:call(Tdbp,{lookup_unidata_tid,Ref}).    

set_last_tid(Tdbp,Ref,Tid) ->
    gen_server:call(Tdbp,{set_last_tid,Ref,Tid}).
get_last_tid(Tdbp,Ref) ->
    gen_server:call(Tdbp,{get_last_tid,Ref}).

set_last_failed(Tdbp,Ref,Flag) ->
    gen_server:call(Tdbp,{set_last_failed,Ref,Flag}).
get_last_failed(Tdbp,Ref) ->
    gen_server:call(Tdbp,{get_last_failed,Ref}).

set_gentid(Tdbp,Ref,Tid) ->
    gen_server:call(Tdbp,{set_gentid,Ref,Tid}).
get_gentid(Tdbp,Ref) ->
    gen_server:call(Tdbp,{get_gentid,Ref}).

%% Management/Debug interface
active_transactions(Tdbp) ->
    gen_server:call(Tdbp,active_transactions).
print_tdb(Tdbp,Info) ->
    gen_server:call(Tdbp,{print_tdb,Info}).

%% Process database interface
add_responder(Tdbp,Responder) ->
    gen_server:call(Tdbp,{add_responder,Responder}).
reset_responder(Tdbp,Responder) ->
    gen_server:call(Tdbp,{add_responder,Responder}).
get_idle_responder(Tdbp) ->
    gen_server:call(Tdbp,get_idle_responder).

add_initiator(Tdbp,Initiator) ->
    gen_server:call(Tdbp,{add_initiator,Initiator}).
reset_initiator(Tdbp,Initiator) ->
    gen_server:call(Tdbp,{add_initiator,Initiator}).
get_idle_initiator(Tdbp) ->
    gen_server:call(Tdbp,get_idle_initiator).

%-------------------------------------------------------------------------------
%% Maintenance
start() ->
    gen_server:start_link(?MODULE,[],?START_OPTIONS).

init(_) ->
    Tdb = ets:new(tdb,[set, private]),
    Initdb = ets:new(initiator,[set, private]),
    Respdb = ets:new(responder,[set, private]),
    A = ets:new(responder,[set, private]),
    B = ets:new(responder,[set, private]),
    C = ets:new(responder,[set, private]),
    ?trace("Started WTP database ok",[],init),
    {ok, #state{tdb=Tdb,initdb=Initdb,respdb=Respdb,
		last_tid=A,last_failed=B,gentid=C}}.


stop(Serverref) ->
    gen_server:call(Serverref,stop).

terminate(_Reason,State) ->
    ets:delete(State#state.tdb),
    ets:delete(State#state.initdb),
    ets:delete(State#state.respdb),
    ets:delete(State#state.last_tid),
    ets:delete(State#state.last_failed),
    ets:delete(State#state.gentid),
    ?trace("Stopped WTP database ok",[],init).


code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% =============================================================================
%% Maintenance
handle_call(stop, _, Tab) ->
    {stop, normal, ok, Tab};

%% =============================================================================
%% Transaction Database
%% .............................................................................
%% Transaction database interface
%% The following functions are available:
%% add_transaction - Adds a transaction
%% remove_transaction - Removes a transaction
%% lookup_unidata_tid - Looks up a transaction a returns {ok,Pid}
%% lookup_tid -  Looks up a transaction a returns {ok,Pid,TidNew}
%% print_tdb - Prints the complete Transaction database, for debugging
%% Ref is a tuple {Tpar,TrType,Tid}

%% Add a new transaction to Tdb
handle_call({add_transaction,Ref,{Tpid,TidNew}},_, Tab) ->
    Entry1=#transaction{tpid=Tpid,tidnew_set=TidNew},
    Ans=ets:insert(Tab#state.tdb,{Ref,Entry1}),
    {reply,Ans,Tab};

%% Remove transaction
handle_call({remove_transaction, Ref},_,Tab) -> 
    ets:delete(Tab#state.tdb,Ref),
    {reply,ok,Tab};

%% Lookup a transaction
handle_call({lookup_unidata_tid,Ref},_, Tab) ->
    case ets:lookup(Tab#state.tdb,Ref) of
	[{{_,Trtype,_Tid},Entry}] ->
	    {reply,{ok,
		    Entry#transaction.tpid,Trtype,Entry#transaction.tidnew_set
		   },Tab};
	[] ->
	    {reply,{error,no_transaction},Tab}
    end;
handle_call({lookup_tid,Ref},_, Tab) ->
    case ets:lookup(Tab#state.tdb,Ref) of
	[{{_,Trtype,_Tid},Entry}] ->
	    {reply,{ok,Entry#transaction.tpid,Trtype},Tab};
	[] ->
	    {reply,{error,no_transaction},Tab}
    end;


handle_call({get_last_tid,Ref},_, Tab) ->
    Ans=case ets:lookup(Tab#state.last_tid,Ref) of
	    [{_,Last_tid}] -> Last_tid;
	    [] -> undefined
	end,
    {reply,Ans,Tab};
handle_call({set_last_tid,Ref,Tid},_, Tab) ->
    ets:insert(Tab#state.last_tid,{Ref,Tid}),
    {reply,ok,Tab};

handle_call({get_last_failed,Ref},_, Tab) ->
    Ans=case ets:lookup(Tab#state.last_failed,Ref) of
	    [{_,Last_failed}] -> Last_failed;
	    [] -> undefined
	end,
    {reply,Ans,Tab};
handle_call({set_last_failed,Ref,Flag},_, Tab) ->
    ets:insert(Tab#state.last_failed,{Ref,Flag}),
    {reply,ok,Tab};

handle_call({get_gentid,Ref},_, Tab) ->
    Ans=case ets:lookup(Tab#state.gentid,Ref) of
	    [{_,Tid}] -> Tid;
	    [] -> 0
	end,
    {reply,Ans,Tab};
handle_call({set_gentid,Ref,Tid},_, Tab) ->
    ets:insert(Tab#state.gentid,{Ref,Tid}),
    {reply,ok,Tab};


%% Responder and Initiator process database
handle_call({add_responder,Responder},_, Tab) ->
    ets:insert(Tab#state.respdb,{Responder,listen}),
    {reply,ok,Tab};
handle_call(get_idle_responder,_, Tab) ->
    A=case ets:match_object(Tab#state.respdb,{'_',listen}) of
	  [] ->
	      {error,no_idle_responder};
	  [{Responder,_}|_] ->
	      ets:insert(Tab#state.respdb,{Responder,active}),
	      {ok,Responder}
      end,
    {reply,A,Tab};

handle_call({add_initiator,Initiator},_, Tab) ->
    ets:insert(Tab#state.initdb,{Initiator,null}),
    {reply,ok,Tab};
handle_call(get_idle_initiator,_, Tab) ->
    A=case ets:match_object(Tab#state.initdb,{'_',null}) of
	  [] ->
	      {error,no_idle_initiator};
	  [{Initiator,_}|_] ->
	      ets:insert(Tab#state.initdb,{Initiator,active}),
	      {ok,Initiator}
      end,
    {reply,A,Tab};


%% Management/Debug interface
handle_call(active_transactions,_, Tab) ->
    case ets:tab2list(Tab#state.tdb) of
	[] ->
	    io:format("No active transaction~n",[]);
	L ->
	    io:format("Active transactions:~n",[]),
	    lists:foreach(
	      fun({{Tpar,Trtype,Tid},{_,Pid,Tidnew}}) ->
		      io:format("Tpar=~p Pid=~p
          Trtype=~p Tid=~p Tidnew=~p~n",
				[Tpar,Pid,Trtype,Tid,Tidnew])
	      end
	      ,L)
    end,	    
    {reply,ok,Tab};
handle_call({print_tdb,Info},_, Tab) ->
    case Info of    
	[] -> ok;
	_ -> io:format("print_tdb ~p ~n",[Info])
    end,
    case ets:tab2list(Tab#state.tdb) of
	[] ->
	    io:format("No transaction~n",[]);
	L ->
	    lists:foreach(
	      fun({{Tpar,Trtype,Tid},{_,Pid,Tidnew}}) ->
	      io:format("          Tpar=~p Pid=~p
          Trtype=~p Tid=~p Tidnew=~p~n",
			[Tpar,Pid,Trtype,Tid,Tidnew])
	      end
	      ,L)
    end,
    {reply,ok,Tab}.


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

