%%% File    : wsp_db.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Local WSP component database interfaces.
%%% Created : 30 Apr 2001 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_db).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_db.erl,v 1.1.1.1 2001/07/04 14:51:11 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:11 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([
	 lookup_appref/2,insert_appref/3,

	 add_session/7,remove_session/2,
	 lookup_session/2,lookup_session_cap/2,lookup_session_data/2,
	 update_session_addr/3,update_session_data/3,
	 get_next_sid/1,
	 print_sdb/2,

	 add_method/3,lookup_method/2,remove_method/2,release_method/2,
	 get_all_methods/2,

	 add_push/3,lookup_push/2,remove_push/2,

	 active_methods/1,active_pushes/1,active_sessions/1,
	 session_info/2,

	 start/0,
	 stop/1]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	 code_change/3]).

-include("wsp.hrl").

-define(WSPNAME,?MODULE).

-record(state,{sid,     % (int) Session id
	       apprefdb,% (ets) Maps an application reference to an application
	       sesdb,   % (ets) Session information database,
	       sesreqdb,% (ets) Session request database
	       reqdb,   % (ets) Method request database
	       pushdb   % (ets) Push request database
	      }).

%% Each entry in the Session Database (sesdb) has the following fields:
-record(session,{spid,    % (pid) To the WSP session process
		 type,    % (atom) 'client' or 'server' dependent on type
		 headlist,% (list)  (key/value pairs) of static headers
		 cap,     % (#cap{}) Capability record for session
		 encvers  % (list) (key/value pairs) of encoding versions
		}).


%% setup_pid (pid) - The process id to the request process when setting up
%%                the session. Set to true if the session is setup already
%% capabil (list) - Negotiated capabilities valid to the whole session
%% reqlist (list) - list of pending requests for this session
%-record(entry,{setup_pid,capabil,reqlist}).

%-------------------------------------------------------------------------------
% Interface
%% Session database interface
add_session(Sdb,Sref,Spid,Headlist,Cap,EVlist,Type) ->
    gen_server:call(Sdb,{add_session,Sref,Spid,Headlist,Cap,EVlist,Type}).
remove_session(Sdb,Sref) ->
    gen_server:call(Sdb,{remove_session,Sref}).
lookup_session(Sdb,Sref) ->
    gen_server:call(Sdb,{lookup_session,Sref}).
lookup_session_cap(Sdb,Sref) ->
    gen_server:call(Sdb,{lookup_session_cap,Sref}).
lookup_session_data(Sdb,Sref) ->
    gen_server:call(Sdb,{lookup_session_data,Sref}).
update_session_addr(Sdb,Sref,Tpar) ->
    gen_server:call(Sdb,{update_session_addr,Sref,Tpar}).
update_session_data(Sdb,Sref,Data) ->
    gen_server:call(Sdb,{update_session_data,Sref,Data}).
print_sdb(Sdb,Info) ->
    gen_server:call(Sdb,{print_sdb,Info}).

%% Method database interface
add_method(Sdb,Ref,Entry) ->
    gen_server:call(Sdb,{add_method,Ref,Entry}).
lookup_method(Sdb,Ref) ->
    gen_server:call(Sdb,{lookup_method,Ref}).
remove_method(Sdb,Ref) ->
    gen_server:call(Sdb,{remove_method,Ref}).
release_method(Sdb,Ref) ->
    gen_server:call(Sdb,{release_method,Ref}).
get_all_methods(Sdb,Sref) ->
    gen_server:call(Sdb,{get_all_methods,Sref}).
    
%% Mgmt interface    
active_methods(Sdb) ->
    gen_server:call(Sdb,active_methods).
active_pushes(Sdb) ->
    gen_server:call(Sdb,active_pushes).
active_sessions(Sdb) ->
    gen_server:call(Sdb,active_sessions).
session_info(Sdb,Sref) ->
    gen_server:call(Sdb,{session_info,Sref}).
    
%% Push database interface
add_push(Sdb,Ref,Tpid) ->
    gen_server:call(Sdb,{add_push,Ref,Tpid}).
lookup_push(Sdb,Ref) ->
    gen_server:call(Sdb,{lookup_push,Ref}).
remove_push(Sdb,Ref) ->
    gen_server:call(Sdb,{remove_push,Ref}).

%% Application Reference Database
insert_appref(Sdb,AppRef,App) ->
    gen_server:call(Sdb,{insert_appref,AppRef,App}).
lookup_appref(Sdb,AppRef) ->
    gen_server:call(Sdb,{lookup_appref,AppRef}).


%% Other 
get_next_sid(Sdb) ->
    gen_server:call(Sdb,next_sid).


%-------------------------------------------------------------------------------
%% Maintenance
start() ->
    gen_server:start_link(?WSPNAME,[],?START_OPTIONS).

init(_) ->
    Sdb = ets:new(sdb,[set, private]),
    SesreqDb = ets:new(sdb,[set, private]),
    RequestDb = ets:new(rdb,[set, private]),
    PushDb = ets:new(pdb,[set, private]),
    AppRefDb = ets:new(appref,[set, private]),
    ?trace("Started WSP database ok",[],init),
    {ok,#state{sid=0,
	       sesdb=Sdb,apprefdb=AppRefDb,
	       sesreqdb=SesreqDb,reqdb=RequestDb,pushdb=PushDb}}.

stop(Serverref) ->
    gen_server:call(Serverref,stop).

terminate(Reason,_State) ->
    ?trace("Stopped WSP database:~p",[Reason],terminate).

%% =============================================================================
%% Maintenance
handle_call(stop,_, Tab) ->
    ets:delete(Tab#state.sesdb),
    ets:delete(Tab#state.apprefdb),
    ets:delete(Tab#state.sesreqdb),
    ets:delete(Tab#state.reqdb),
    ets:delete(Tab#state.pushdb),
    {stop, normal, ok, Tab};
			       
%% =============================================================================
%% Session Database
%% .............................................................................
%% Add a new session - the Session Id will be added later
handle_call({add_session,Tpar,Spid,Headlist,Cap,EVlist,Type},_, Tab) ->
    Entry=#session{spid=Spid,type=Type,
		   headlist=Headlist,cap=Cap,encvers=EVlist},
    ets:insert(Tab#state.sesdb,{Tpar,undefined,Entry}),
    {reply,ok,Tab};

%% Remove session
%% Note:
%% - Assumes that all pending requests have been aborted, thus the pending list
%%   should be empty.
handle_call({remove_session, Sref},_,Tab) ->
    ets:delete(Tab#state.sesdb,Sref),
    {reply,ok,Tab};


% Get the session, given an Address Quadruple
handle_call({lookup_session,Sref},_, Tab)  ->
    case ets:lookup(Tab#state.sesdb,Sref) of
	[{_,_,Entry}] ->
	    {reply,{ok,Entry#session.spid},Tab};
	[] ->
	    {reply,{error,no_session},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

handle_call({lookup_session_cap,Sref},_, Tab) ->
    case ets:lookup(Tab#state.sesdb,Sref) of
	[{_,_,Entry}] ->
	    {reply,{ok,{Entry#session.spid,
			Entry#session.cap}},Tab};
	[] ->
	    {reply,{error,no_session},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

% Get session data, given a process id to a Session process
handle_call({lookup_session_data,Sref},_, Tab) ->
    case ets:lookup(Tab#state.sesdb,Sref) of
	[{Sref,_,Entry}] ->
	    {reply,{ok,{Entry#session.spid,
			Entry#session.type
		       }},Tab};
	[] ->
	    {reply,{error,no_session},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

%% Changes the address and bearer associated to a bearer
handle_call({update_session_addr,Sref,Tpar},_, Tab) ->
    case ets:lookup(Tab#state.sesdb,Sref) of
	[{_,Sid,Entry}] ->
	    ets:delete(Tab#state.sesdb,Sref),
	    ets:insert(Tab#state.sesdb,{Tpar,Sid,Entry}),
	    {reply,ok,Tab};
	[] ->
	    {reply,{error,no_session},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

%% Set session Id associated to a Session
handle_call({update_session_data,Sref,{Sid,Cap}},_, Tab) ->
    case ets:lookup(Tab#state.sesdb,Sref) of
	[{Sref,_,Entry}] ->
	    ets:insert(Tab#state.sesdb,{Sref,Sid,
					Entry#session{cap=Cap}}),
	    {reply,ok,Tab};
	[] ->
	    {reply,{error,no_session},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

%% .............................................................................
%% Session Id handling
%% If the Sid value reaches a maximum value the Sid is reset, i.e no check for
%% wrap arounds
handle_call(next_sid,_,Tab) ->
    Sid=if
	    Tab#state.sid<65536 -> Tab#state.sid+1;
	    true -> 0
	end,
    _Tab1=Tab#state{sid=Sid},
    {reply,Sid,Tab};

%% For mgmt interface
handle_call(active_methods,_, Tab) ->
    L=case ets:tab2list(Tab#state.reqdb) of
	  [] ->
	      "No active";
	  A ->
	      A
      end,
    io:format("Pending Methods: ~p ~n",[L]),
    {reply,ok,Tab};    
handle_call(active_sessions,_, Tab) ->
    L=case ets:tab2list(Tab#state.sesdb) of
	  [] ->
	      "No active";
	  A ->
	      A
      end,
    io:format("Active Sessions: ~p ~n",[L]),
    {reply,ok,Tab};
handle_call(active_pushes,_, Tab) ->
    L=case ets:tab2list(Tab#state.pushdb) of
	  [] ->
	      "No active";
	  A ->
	      A
      end,
    io:format("Pending Push: ~p ~n",[L]),
    {reply,ok,Tab};
handle_call({session_info,_Sref},_, Tab) ->
    L=case ets:tab2list(Tab#state.sesdb) of
	  [] ->
	      "Not active";
	  A ->
	      A
      end,
    io:format("Session: ~p ~n",[L]),
    {reply,ok,Tab};

%% For debugging purposes only
handle_call({print_sdb,Info},_, Tab) ->
    io:format("print_sdb ~p",[Info]),
    L=ets:tab2list(Tab#state.sesdb),
    L1=ets:tab2list(Tab#state.sesreqdb),
    L2=ets:tab2list(Tab#state.reqdb),
    L3=ets:tab2list(Tab#state.pushdb),
    io:format("type_of_transaction dbs: sesreqdb ~p reqdb ~p pushdb ~p sessiondb ~p",
[L1,L2,L3,L]),  
    {reply,ok,Tab};

%% =============================================================================
%% Method Database
%% .............................................................................
%% Note that requests stored on the client and server sides are different,
%% Client: Entry={Tpid,Type}
%% Server: Entry={Tpid,Type,Holding}

%% Add a new pending request - both sides
handle_call({add_method,Ref,Entry},_, Tab) ->
    ets:insert(Tab#state.reqdb,{Ref,Entry}),
    {reply,ok,Tab};

% Remove request - both sides
handle_call({remove_method, Ref},_,Tab) ->
    ets:delete(Tab#state.reqdb,Ref),
    {reply,ok,Tab};

%% Get request - both sides
%% Note that only the Tpid is returned on both sides, even on the server side
handle_call({lookup_method,Ref},_, Tab) ->
    case ets:lookup(Tab#state.reqdb,Ref) of
	[{Ref,{WSPmet,_}}] ->
	    {reply,{ok, WSPmet},Tab};
	[] ->
	    {reply,{error,method_notfound},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

%% Get request - both sides
%% Note that only the Tpid is returned on both sides, even on the server side
handle_call({release_method,Ref},_, Tab) ->
    case ets:lookup(Tab#state.reqdb,Ref) of
	[{Ref,{WSPmet,_}}] ->
	    ets:insert(Tab#state.reqdb,{Ref,{WSPmet,released}}),
	    {reply,ok,Tab};
	[] ->
	    {reply,{error,method_notfound},Tab};
	Ans ->
	    {reply,{error,Ans},Tab}
    end;

%% Get the current database - both sides
handle_call({get_all_methods,_Sref},_, Tab) ->
    {reply,ets:tab2list(Tab#state.reqdb),Tab};

%% =============================================================================
%% Push Database
%% .............................................................................
%% Add a new pending push - both sides
handle_call({add_push, Ref, Tpid},_, Tab) ->
    ets:insert(Tab#state.pushdb,{Ref,Tpid}),
    {reply,ok,Tab};

% Remove push - both sides
handle_call({remove_push, Ref},_,Tab) ->
    ets:delete(Tab#state.pushdb,Ref),
    {reply,ok,Tab};

%% Get push - both sides
handle_call({lookup_push,Ref},_, Tab) ->
    case ets:lookup(Tab#state.pushdb,Ref) of
	[{Ref,Tpid}] ->
	    {reply,{ok, Tpid},Tab};
	[] ->
	    {reply,{error,push_notfound},Tab};
	Ans ->
            {reply,{error,Ans},Tab}
    end;

%% =============================================================================
%% Database interface for AppRef/Application storage
handle_call({lookup_appref,{AppRef,_}},_, Tab) ->
    A=case ets:lookup(Tab#state.apprefdb,AppRef) of
	  [{_,App}] ->
	      App;
	  [] ->
	      {error,appref_notfound};
	  {error,Reason} ->
	      {error,Reason}
      end,
    {reply,A,Tab};
handle_call({lookup_appref,AppRef},_, Tab) ->
%    L=ets:tab2list(Tab#state.apprefdb),
    A=case ets:lookup(Tab#state.apprefdb,AppRef) of
	  [{_,App}] ->
	      App;
	  _ ->
	      {error,appref_notfound}
      end,
    {reply,A,Tab};

%% Add a new session to Sdb
handle_call({insert_appref,AppRef,App},_, Tab) ->
    ets:insert(Tab#state.apprefdb,{AppRef,App}),
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

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

