%%% File    : wsp_session_c.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Server Session state machine
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_session_c).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_session_c.erl,v 1.1.1.1 2001/07/04 14:51:14 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:14 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").
 
%% Internal Component interface
-export([start_link/1,stop/1,tr_invoke_ind/3, % Used by wsp_man
	 remove_method/1,% Used by wsp_method_c
	 remove_push/1   % Used by wsp_push_c
	]).

%% Internal gen_fsm callbacks
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3, code_change/4]).

%% Internal gen_fsm states
-export([
	 connecting/2,connecting/3,
	 connected/2,connected/3,
	 suspended/2,suspended/3,
	 resuming/2,resuming/3]).

-include("wsp.hrl").
-include("wspif.hrl").

-record(state,{wtp,       % (pid) to WTP layer process
	       sdb,       % (pid) to Session database
	       sid,       % (int) Session id for this session
	       tpar,      % ({uint16,#address}) ApplicationId and DestAddress
	       env,       % (#env) (Negotiated) parameters for this session
	       n_pushes,  % (int) Number of push processes active
	       n_methods, % (int) Number of method processes active
	       lastreq    % (int)  Tid to last "session only" related request
	      }).

%-------------------------------------------------------------------------------
%% API, see also wspif.hrl
tr_invoke_ind(WSPses,WTPres,Content) ->
    gen_fsm:send_event(WSPses, {tr_invoke_ind,WTPres,Content}).
remove_method(WSPses) ->
    gen_fsm:send_all_state_event(WSPses,remove_method).
remove_push(WSPses) ->
    gen_fsm:send_all_state_event(WSPses,remove_push).

%-------------------------------------------------------------------------------
% Maintenance
% Needs a manager of WSP sessions on both sides - Client and Server
start_link(Args)->
    case gen_fsm:start_link(?MODULE,Args,?START_OPTIONS) of
	{ok,Wsp} ->
	    Wsp;
	Error ->
	    ?error("Can't start WSP session, got ~p",[Error],start_link),
	    {error,cant_start_wsp_session}
    end.


stop(Wsp) ->
    gen_fsm:send_event(Wsp,stop).


%% .............................................................................
%% Call-back functions
%% Starts a database for the session data, for each WAP stack
%% connect_req
%% Note:
%% - Cap are WAP defaults + Connect_req capabilities
init({WTPman,Sdb,Tpar,Headers,Cap,EncodingVersion}) ->
    {_,{Addr,Port,Bear}}=Tpar,
    State=#state{wtp=WTPman,sdb=Sdb,
		 tpar=Tpar,
		 env=#env{encoding_version=EncodingVersion,
			  cap=Cap,
			  headers=Headers,
			  destination=#address{address=Addr,port=Port,
					       bearer=Bear}},
		 n_methods=0,n_pushes=0},

    case tr_invoke_req(?CLASS2,#connect{headers=Headers,capabilities=Cap},
		       State) of
	{ok,WTPini} ->
	    {ok,connecting,State#state{lastreq=WTPini}};
	Error ->
	    ?error("Cannot start WSP session (Dest=~p) got ~p",
		   [Tpar,Error],init),
	    {stop,normal}
    end.
	    
terminate(Reason,_StateName,State) ->
    ?trace("WSP session (Dest=~w) stopped:~w",
	   [State#state.tpar,Reason],terminate).

code_change(_OldVsn, StateName, State, _Extra)->
    {ok, StateName, State}.

%%% ----------------------------------------------------------------------------
%%% The Client Session state machine

%%% >>>The null state<<<
%% The NULL state in the client, waiting for a Connect request from the
%% application, is handled in wsp_man

%%% >>>The connecting state<<<
%% Note that the Session Id is undefined in this state!
%% Method invoke should return the WTPini pid() given by WTP.
connecting({method_invoke_req,HTTPMeth,HTTPCont},_,State) ->
    catch state_null_method(HTTPMeth,HTTPCont,State,connecting);
connecting(disconnect_req,_,State) ->
    WTPini=State#state.lastreq, % The Connect Transaction
    tr_abort_req(WTPini,?DISCONNECT),
    handle_abort(?DISCONNECT,State),
    s_disconnect_ind(?USERREQ,State),
    next_state_null(State,ok);
connecting(pseudo_disconnect,_,State) ->
    WTPini=State#state.lastreq, % The Connect Transaction
    tr_abort_req(WTPini,?DISCONNECT),
    handle_abort(?DISCONNECT,State),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
connecting(pseudo_suspend,_,State) ->
    WTPini=State#state.lastreq, % The Connect Transaction
    tr_abort_req(WTPini,?SUSPEND),
    handle_abort(?SUSPEND,State),
    s_disconnect_ind(?SUSPEND,State),
    next_state_null(State,ok);
connecting(_A,_,State) ->
    {reply, {error,invalid_command},connecting,State}.

connecting({tr_invoke_ind,WTPres,{Class,_,_}},State) ->
    if
	Class==?CLASS0 -> ignore;
	true -> wtp_responder:abort_req(WTPres,?WSP_PROTOERR)
    end,
    {next_state, connecting, State};
connecting({tr_result_ind,WTPini,BinPdu},State) ->
    case catch wsp_man:tr_result_ind(
		  BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPini,?MRUEXCEEDED),
	    handle_abort(?CONNECTERR,State),
	    s_disconnect_ind(?MRUEXCEEDED,State),
	    next_state_null(State);
	#connect_reply{server_sessionid=Sid,
		       capabilities=NewCap,headers=Headers} ->
	    tr_result_res(WTPini),
	    s_connect_cnf(Headers,NewCap,State),
	    wsp_db:update_session_data(State#state.sdb,
				       State#state.tpar,{Sid,NewCap}),
	    State1=State#state{sid=Sid,env=(State#state.env)#env{cap=NewCap}},
	    {next_state, connected, State1};
	#redirect{permanent=Perm,reuse_secure_session=ReuseSec,
		  rediradresses=Redir} ->
	    tr_result_res(WTPini),
	    handle_abort(?CONNECTERR,State),
	    s_disconnect_ind({redirect,{Perm,ReuseSec,Redir}},State),
	    next_state_null(State);
	#reply{status=Status,headers=Headers,contenttype=CT,data=Data} ->
	    tr_result_res(WTPini),
	    handle_abort(?CONNECTERR,State),
	    s_disconnect_ind({reply,{Status,Headers,CT,Data}},State),
	    next_state_null(State);
	_Other ->
	    tr_abort_req(WTPini,?WSP_PROTOERR),
	    handle_abort(?CONNECTERR,State),
	    s_disconnect_ind(?WSP_PROTOERR,State),
	    next_state_null(State)
    end;
connecting({tr_invoke_cnf,_,_},State) ->
    {next_state, connecting, State};
connecting({tr_abort_ind,_WTP,Info},State) ->
    ?debug("Got abort ~p",[Info],connecting),
    handle_abort(?CONNECTERR,State),
    s_disconnect_ind(Info,State),
    {next_state, connecting, State};
connecting(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],connecting),
    handle_illegal_event(State).



%%% ----------------------------------------------------------------------------
%%% >>>The connected state<<<
connected(disconnect_req,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind(?USERREQ,State),
	    next_state_null(State,ok);
	Error ->
	    {reply, Error, connected, State}
    end;
connected(pseudo_disconnect,_,State) ->
    ?debug("Got pseudo_disconnect",[],connected),
    handle_abort(?DISCONNECT,State),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
connected({method_invoke_req,HTTPMeth,HTTPCont},_,State) ->
    catch state_null_method(HTTPMeth,HTTPCont,State,connected);
connected(suspend_req,_,State) ->
    case tr_invoke_req(?CLASS0,#suspend{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    handle_abort(?SUSPEND,State),
	    s_suspend_ind(?USERREQ,State),
	    {reply,ok, suspended, State};
	Error ->
	    {reply,Error, connected, State}
    end;
connected(pseudo_suspend,_,State) ->
    handle_abort(?SUSPEND,State),
    case ((State#state.env)#env.cap)#cap.sresumef of
	true ->
	    s_suspend_ind(?SUSPEND,State),
	    {reply,ok, suspended, State};
	false ->
	    s_disconnect_ind(?SUSPEND,State),
	    next_state_null(State,ok)
    end;
connected(resume_req,_,State) ->
    case tr_invoke_req(?CLASS2,#resume{server_sessionid=State#state.sid},
		       State) of
	{ok,WTPini} ->
	    handle_abort(?USERREQ,State),	   
	    State1=State#state{lastreq=WTPini},
	    {reply,ok, resuming, State1};
	Error ->
	    {reply,Error, connected, State}
    end;
connected({resume_req,NewTpar},_,State) ->
    case tr_invoke_req(?CLASS2,#resume{server_sessionid=State#state.sid},
		       State) of
	{ok,WTPini} ->
	    handle_abort(?USERREQ,State),
	    wsp_db:update_session_addr(State#state.sdb,State#state.tpar,
				       NewTpar),
	    State1=State#state{tpar=NewTpar,lastreq=WTPini},
	    {reply,ok, resuming, State1};
	Error ->
	    {reply,Error, connected, State}
    end;
connected(_A,_,State) ->
    {reply, {error,invalid_command},connected,State}.

connected({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPres,?MRUEXCEEDED),
	    {next_state, connected, State};
	{?CLASS0,Pdu} when is_record(Pdu,disconnect) ->
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	{?CLASS0,#push{type=push,headers=Headers,contenttype=CT,data=Data}} ->
	    case ((State#state.env)#env.cap)#cap.pushf of
		true ->
		    s_push_ind({Headers,CT,Data},State);
		false ->
		    ok
	    end,
	    {next_state, connected, State};
	{?CLASS1,Pdu} when is_record(Pdu,push),Pdu#push.type==confirmed_push ->
	    S=case ((State#state.env)#env.cap)#cap.cpushf of
		  true ->
		      catch state_null_push(WTPres,Pdu,State);
		  false ->
		      State
	      end,
	    {next_state, connected, S};
	Other ->
	    ?warning("Received tr_invoke_ind with ~p got ~p",
		     [BinPdu,Other],connected),
	    {next_state, connected, State}
    end;
connected(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],connected),
    handle_illegal_event(State).


%%% ----------------------------------------------------------------------------
%%% >>>The suspended state<<<
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WSP specification
suspended({method_invoke_req,_},_,State) ->
    {reply, {error,session_suspended}, suspended, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
suspended(disconnect_req,_,State) ->
    s_disconnect_ind(?USERREQ,State),
    next_state_null(State,ok);
suspended(pseudo_disconnect,_,State) ->
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
suspended(resume_req,_,State) ->
    case tr_invoke_req(?CLASS2,#resume{server_sessionid=State#state.sid},
		       State) of
	{ok,WTPini} ->
	    handle_abort(?USERREQ,State),
	    State1=State#state{lastreq=WTPini},
	    {reply,ok, resuming, State1};
	Error ->
	    {reply,Error, suspended, State}
    end;
suspended({resume_req,NewTpar},_,State) ->
    case tr_invoke_req(?CLASS2,#resume{server_sessionid=State#state.sid},
		       State) of
    {ok,WTPini} ->
	    handle_abort(?USERREQ,State),
	    wsp_db:update_session_addr(State#state.sdb,State#state.tpar,
				       NewTpar),
	    NewState=State#state{tpar=NewTpar,lastreq=WTPini},
	    {reply,ok, resuming, NewState};
	Error ->
	    {reply,Error, suspended, State}
    end;
suspended(_A,_,State) ->
    {reply, {error,invalid_command},suspended,State}.


suspended({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPres,?MRUEXCEEDED),
	    {next_state, suspended, State};
	{?CLASS0,Pdu} when is_record(Pdu,disconnect) ->
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	{?CLASS1,Pdu} when is_record(Pdu,push),Pdu#push.type==confirmed_push ->
	    S=case ((State#state.env)#env.cap)#cap.cpushf of
		  true ->
		      catch state_null_push(WTPres,Pdu,State);
		  false ->
		      State
	      end,
	    {next_state, suspended, S};
	Other ->
	    ?warning("Received tr_invoke_ind with ~p got ~p",
		     [BinPdu,Other],suspended),
	    {next_state, suspended, State}
    end;
suspended({tr_invoke_cnf,_,_},State) ->
    {next_state, suspended, State};
suspended({tr_abort_ind,_,_},State) ->
    {next_state, suspended, State};
suspended(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],suspended),
    handle_illegal_event(State).

%%% ----------------------------------------------------------------------------
%%% >>>The resuming state<<<
resuming(disconnect_req,_,State) ->
    WTPini=State#state.lastreq, % The Resume Transaction
    tr_abort_req(WTPini,?DISCONNECT),
    handle_abort(?DISCONNECT,State),   
    s_disconnect_ind(?USERREQ,State),
    next_state_null(State,ok);
resuming(pseudo_disconnect,_,State) ->
    WTPini=State#state.lastreq, % The Resume Transaction
    tr_abort_req(WTPini,?DISCONNECT),
    handle_abort(?DISCONNECT,State),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
resuming({method_invoke_req,HTTPMeth,HTTPCont},_,State) ->
    catch state_null_method(HTTPMeth,HTTPCont,State,resuming);
resuming(suspend_req,_,State) ->
    case tr_invoke_req(?CLASS0,#suspend{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    WTPini=State#state.lastreq, % The Resume Transaction
	    tr_abort_req(WTPini,?SUSPEND),
	    handle_abort(?SUSPEND,State),
	    s_suspend_ind(?USERREQ,State),
	    {reply,ok, suspended, State};
	Error ->
	    {reply,Error, resuming, State}
    end;
resuming(pseudo_suspend,_,State) ->
    WTPini=State#state.lastreq, % The Resume Transaction
    tr_abort_req(WTPini,?SUSPEND),
    handle_abort(?SUSPEND,State),
    s_disconnect_ind(?SUSPEND,State),
    {reply,ok, suspended, State};
resuming(_A,_,State) ->
    {reply, {error,invalid_command},resuming,State}.

resuming({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPres,?MRUEXCEEDED),
	    {next_state, resuming, State};
	{?CLASS0,Pdu} when is_record(Pdu,disconnect) ->
	    WTPini=State#state.lastreq, % The Resume Transaction
	    tr_abort_req(WTPini,?DISCONNECT),
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	{?CLASS1,Pdu} when is_record(Pdu,push),Pdu#push.type==confirmed_push ->
	    tr_abort_req(WTPres,?WSP_PROTOERR),
	    {next_state, resuming, State};
	Other ->
	    ?warning("Received tr_invoke_ind with ~p got ~p",
		     [BinPdu,Other],resuming),
	    {next_state, resuming, State}
    end;
resuming({tr_result_ind,WTPini,BinPdu},State) ->
    case catch wsp_man:tr_result_ind(
		  BinPdu,((State#state.env)#env.cap)#cap.client_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPini,?MRUEXCEEDED),
	    handle_abort(?SUSPEND,State),
	    s_suspend_ind(?MRUEXCEEDED,State),
	    next_state_null(State);
	#reply{status=Status} when 207<Status,Status<200 ->
	    tr_result_res(WTPini),
	    s_resume_cnf(State),
	    {next_state, connected, State};
	#reply{status=Status,headers=Headers,contenttype=CT,data=Data} ->
	    tr_result_res(WTPini),
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind({reply,{Status,Headers,CT,Data}},State),
	    next_state_null(State);
	_Other ->
	    tr_abort_req(WTPini,?WSP_PROTOERR),
	    handle_abort(?SUSPEND,State),
	    s_suspend_ind(?WSP_PROTOERR,State),
	    {next_state, suspended, State}
    end;
resuming({tr_invoke_cnf,_WTPini,_ExitInfo},State) ->
    {next_state, resuming, State};
resuming({tr_abort_ind,_WTPini,{_AbortType,AbortReason}},State) ->
    if
	AbortReason==?DISCONNECT ->
	    handle_abort(?DISCONNECT,State),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	true ->
	    handle_abort(?SUSPEND,State),
	    s_disconnect_ind(AbortReason,State),
	    {next_state, suspended, State}
    end;
resuming(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],resuming),
    handle_illegal_event(State).


%% -----------------------------------------------------------------------------
%% Aborts all outstanding method transactions
handle_abort(Reason,State) ->
    L=wsp_db:get_all_methods(State#state.sdb,State#state.tpar),
    abort_methods(L,Reason).

handle_abort(Sdb,Sref,Reason) ->
    L=wsp_db:get_all_methods(Sdb,Sref),
    abort_methods(L,Reason).

abort_methods([],_Reason) ->
    ok;
abort_methods([{_,{WSPmet,_}}|L],Reason) ->
    ?debug("Aborting ~p ~p",[WSPmet,Reason],abort_methods),
    wsp_method_c:pseudo_abort(WSPmet,Reason),
    abort_methods(L,Reason).

%% -----------------------------------------------------------------------------
%% Handles the null state in the method state table
%% On a method_invoke_req
state_null_method(Method,HTTPCont,S,SName) ->
    if
	S#state.n_methods<((S#state.env)#env.cap)#cap.mom ->
	    ok;
	true ->
	    throw({reply,{error,max_methods},SName,S})
    end,
    Pdu=wsp_man:create_method_pdu(Method,HTTPCont),
    case wsp_method_c:start_link({Pdu,S#state.env,
				  S#state.wtp,S#state.tpar,S#state.sdb}) of
	WSPmet when is_pid(WSPmet) ->
	    {reply,{ok,WSPmet},SName,S#state{n_methods=S#state.n_methods+1}};
	{error,Reason} ->
	    ?warning("Can't start WSP client method, got {error,~p}",
		     [Reason],state_null_method),
	    {reply,{error,invalid_command},SName,S}
    end.


%% -----------------------------------------------------------------------------
%% Handles the null state in the push state table
%% On a tr_invoke_ind
state_null_push(WTPres,Pdu,S) ->
    if
	S#state.n_pushes<((S#state.env)#env.cap)#cap.mop ->
	    ok;
	true ->
	    tr_abort_req(WTPres,?MOREXCEEDED),
	    throw(S)
    end,
    case wsp_push_c:start_link({Pdu,S#state.env,
				WTPres,S#state.tpar,S#state.sdb}) of
	WSPpus when is_pid(WSPpus) ->
	    s_confirmed_push_ind({Pdu#push.headers,
				  Pdu#push.contenttype,
				  Pdu#push.data},WSPpus,S),
	    S#state{n_methods=S#state.n_methods+1};
	{error,Reason} ->
	    ?warning("Can't start WSP client push, got {error,~p}",
		     [Reason],state_null_method),
	    S
    end.


%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}		|
%%	    {next_state, NextStateName, NextStateData, Timeout} |
%%	    {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(remove_method, StateName, State) ->    
    {next_state, StateName, State#state{n_methods=State#state.n_methods-1}};
handle_event(remove_push, StateName, State) ->    
    {next_state, StateName, State#state{n_pushes=State#state.n_pushes-1}}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%	    {next_state, NextStateName, NextStateData, Timeout}	  |
%%	    {reply, Reply, NextStateName, NextStateData}	  |
%%	    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%	    {stop, Reason, NewStateData}			  |
%%	    {stop, Reason, Reply, NewStateData}			   
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}		|
%%	    {next_state, NextStateName, NextStateData, Timeout} |
%%	    {stop, Reason, NewStateData}			 
%%----------------------------------------------------------------------
handle_info(_Reason, StateName, State) ->
    {next_state, StateName, State}.



%% -----------------------------------------------------------------------------
%% This is the catch all case in all gen_fsm's
%% Incoming events from WTP
handle_illegal_event(State) ->
    tr_abort_req(?DISCONNECT,State),
    handle_abort(State#state.sdb,State#state.tpar,?DISCONNECT),
%%%	    s_disconnect_ind(State#state.sdb,Tpar,?USERREQ),
    next_state_null(State).


next_state_null(State) ->
    wsp_db:remove_session(State#state.sdb,State#state.tpar),
    {stop,normal,State}.

next_state_null(State,_Reply) ->
    wsp_db:remove_session(State#state.sdb,State#state.tpar),
    {stop,normal,ok,State}.

%% =============================================================================
s_disconnect_ind(Reason,State) ->
    R=wsp_pdu:decode_abort_reason(Reason),
    disconnect_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		   self(),R).
s_suspend_ind(Reason,State) ->
    R=wsp_pdu:decode_abort_reason(Reason),
    suspend_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		self(),R).
s_push_ind(HTTPCont,State) ->
    push_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
	     HTTPCont).
s_confirmed_push_ind(#push{headers=Headers,contenttype=CT,data=Data},
		     WSPpus,State) ->
    confirmed_push_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		       WSPpus,{Headers,CT,Data}).

s_connect_cnf(Headlist,Cap,State) ->
    connect_cnf(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		self(),Headlist,Cap).
s_resume_cnf(State) ->
    resume_cnf(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
	       self()).


tr_invoke_req(Class,Pdu,State) ->
    wsp_man:tr_invoke_req(State#state.wtp,self(),State#state.tpar,Class,Pdu,
			  State#state.env,
			  ((State#state.env)#env.cap)#cap.server_sdu).


tr_result_res(WTPini) ->
%    Exitinfo2=http_header_handling:encode_headers(Exitinfo),
    wtp_initiator:result_res(WTPini,[]).
tr_abort_req(WTPini,Reason) -> % Only abort Class 1 and 2 transactions
    wtp_initiator:abort_req(WTPini,Reason).

