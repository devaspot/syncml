%%% File    : wsp_session_s.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Server Session state machine
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_session_s).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_fsm).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_session_s.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% Internal Component interface
-export([start_link/1,stop/1,tr_invoke_ind/3, % Used by wsp_man
	 remove_method/1,% Used by wsp_method_s
	 remove_push/1   % Used by wsp_push_s
	]).

%% Internal gen_fsm callbacks
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3]).

%% Internal gen_fsm states
-export([connecting/2,connecting/3,
	 connected/2,connected/3,
	 connecting_2/2,connecting_2/3,
	 terminating/2,terminating/3,
	 suspended/2,suspended/3,
	 resuming/2,resuming/3,
	 resuming_2/2,resuming_2/3]).

-include("wsp.hrl").
-include("wspif.hrl").

-record(state,{wtp,       % (pid) to WTP layer process
	       sdb,       % (pid) to Session database
	       sid,       % (int) Session id for this session
	       tpar,      % ({uint16,#address}) ApplicationId and DestAddress
	       env,       % (#env) (Negotiated) parameters for this session
	       n_pushes,  % (int) Number of push processes active
	       n_methods, % (int) Number of method processes active
	       lastreq,   % (int)  Tid to last "session only" related request
	       newsession % ({int,#address}) tpar to new session
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
% Needs a manager of WSP sessions on both sides - Mobile and Server
start_link(Args)->
    case gen_fsm:start_link(?MODULE,Args,?START_OPTIONS) of
	{ok,Wsp} ->
	    Wsp;
	Error ->
	    ?error("Can't start WSP session, got ~p",[Error],start_link),
	    throw({error,cant_start_wsp_session})
    end.

stop(Wsp) ->
    gen_fsm:send_event(Wsp,stop).

%% .............................................................................
%% Call-back functions
%% Starts a database for the session data, for each WAP stack
init({WTPman,Sdb,Tpar,Headers,Cap,EncodingVersion,WTPres})->
    {_,{Bear,Addr,Port}}=Tpar,
    State=#state{wtp=WTPman,sdb=Sdb,
		 sid=0,tpar=Tpar,
		 env=#env{encoding_version=EncodingVersion,cap=Cap,
			  headers=Headers,
			  destination=#address{address=Addr,port=Port,
					       bearer=Bear}},
		 lastreq=WTPres,
		 n_pushes=0,n_methods=0},
    tr_invoke_res(WTPres),
    s_connect_ind(?WSP_VERSION,Headers,Cap,State),
    {ok,connecting,State}.


terminate(Reason,StateName,State) ->
    wsp_db:remove_session(State#state.sdb,State#state.tpar),
    ?trace("WSP session (Dest=~w) stopped:~w",
	   [State#state.tpar,Reason],terminate).


%%% ----------------------------------------------------------------------------
%%% The Server Session state machine

%%% >>>The null state<<<
%% The NULL state in the server, waiting for a tr_invoke_ind with a Connect
%% request from the client, is handled in wsp_man

%%% >>>The connecting state<<<
connecting({connect_res,{StaticHeaders,NewCap}},_,State) ->
    % handle_disconnect(Sref,Sdb), % This can not happen !!
    %% The Connect transaction
    Sid=wsp_db:get_next_sid(State#state.sdb),
    case tr_result_req(#connect_reply{server_sessionid=Sid,
					    capabilities=NewCap,
					    headers=StaticHeaders},
			     State) of
	ok ->
	    handle_release(holding,State),
	    wsp_db:update_session_data(State#state.sdb,State#state.tpar,
				       {Sid,NewCap}),
	    State1=State#state{sid=Sid,env=(State#state.env)#env{cap=NewCap}},
	    {reply,ok,connecting_2,State1};
	Error ->
	    {reply,{error,invalid_command},connecting, State}
    end;
connecting({disconnect_req,ReasonCode},_,State) ->
    %% The Connect transaction
    case ReasonCode of
	{Status,ReuseSecure,RedirAddr} ->
	    Perm=
		if
		    Status==301 -> ?PermanentRedirect; % move_permanent
		    Status==302 -> ?FALSE; % move_temporary
		    true -> ?FALSE
		end,
	    case tr_result_req(#redirect{permanent=Perm,
					 reuse_secure_session=ReuseSecure,
					 rediradresses=RedirAddr},
			       State) of
		ok ->
		    handle_abort(State,?DISCONNECT),
		    s_disconnect_ind(?USERREQ,State),
		    {reply,ok,terminating, State};
		Error ->
		    {reply,{error,invalid_command},connecting, State}
	    end;
	{Status,CT,Headers,Data} ->
	    case tr_result_req(#reply{status=Status,headers=Headers,
				      contenttype=CT,data=Data},
			       State) of
		ok ->
		    handle_abort(State,?DISCONNECT),
		    s_disconnect_ind(?USERREQ,State),
		    {reply,ok, terminating, State};
		Error ->
		    {reply,{error,invalid_command},connecting, State}
	    end
    end;
connecting(pseudo_disconnect,_,State) ->
    WTPres=State#state.lastreq, % The Connect transaction
    tr_abort_req(WTPres,?DISCONNECT),
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
connecting(pseudo_suspend,_,State) ->
    WTPres=State#state.lastreq, % The Connect transaction
    tr_abort_req(WTPres,?DISCONNECT),
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(?SUSPEND,State),
    next_state_null(State,ok);
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WSP specification
connecting({push_req,_},_,State) ->
    {reply, {error,no_session}, connecting, State};
connecting({confirmed_push_req,_},_,State) ->
    {reply, {error,no_session}, connecting, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
connecting(A,_,State) ->
    {reply, {error,invalid_command},connecting,State}.

connecting({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{error,mruexceeded} ->
	    tr_abort_req(WTPres,?MRUEXCEEDED),
	    {next_state, connecting, State};
	{?CLASS2,Pdu} when record(Pdu,resume) ->
	    handle_abort(State,?DISCONNECT),
	    {next_state, connecting, State};
	Other ->
	    ?warning("Received tr_invoke_ind with ~p got ~p",
		     [BinPdu,Other],connecting),
	    handle_illegal_event(State)
    end;
connecting({tr_abort_ind,WTPini,Info},State) ->
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(Info,State),
    next_state_null(State);
connecting(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],connecting),
    handle_illegal_event(State).


%%% >>>The terminating state<<<
terminating(pseudo_disconnect,_,State) ->
    WTPres=State#state.lastreq,  % The Connect transaction
    tr_abort_req(WTPres,?DISCONNECT),
    next_state_null(State,ok);
terminating(pseudo_suspend,_,State) ->
    WTPres=State#state.lastreq, % The Connect transaction
    tr_abort_req(WTPres,?SUSPEND),
    next_state_null(State,ok);
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WSP specification
terminating({push_req,_},_,State) ->
    {reply, {error,no_session}, terminating, State};
terminating({confirmed_push_req,_},_,State) ->
    {reply, {error,no_session}, terminating, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
terminating(A,_,State) ->
    {reply, {error,invalid_command},terminating,State}.


terminating({tr_result_cnf,WTPres,ExitInfo},State) ->
    next_state_null(State);
terminating({tr_abort_ind,WTPini,Info},State) ->
    next_state_null(State);
terminating(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],terminating),
    handle_illegal_event(State).


%%% >>>The connecting_2 state<<<
connecting_2(disconnect_req,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    WTPres=State#state.lastreq, % The Connect transaction
	    tr_abort_req(WTPres,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State,ok);
	Error ->
	    {reply, Error,connecting_2 ,State}
    end;
connecting_2(pseudo_disconnect,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    WTPres=State#state.lastreq, % The Connect transaction
	    tr_abort_req(WTPres,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State,ok);
	Error ->
	    {reply, Error,connecting_2 ,State}
    end;
connecting_2({push_req,{Headers,CT,Data}},_,State) ->
    case tr_invoke_req(?CLASS0,#push{headers=Headers,contenttype=CT,data=Data},
		       State) of
	ok ->
	    {reply, ok,connecting_2 ,State};
	Error ->
	    {reply, Error,connecting_2 ,State}
    end;
connecting_2({confirmed_push_req,Content},_,State) ->
    catch state_null_push(Content,State,connecting_2);
connecting_2(pseudo_suspend,_,State) ->
    WTPres=State#state.lastreq, % The Connect transaction
    case ((State#state.env)#env.cap)#cap.sresumef of
	false ->
	    tr_abort_req(WTPres,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?SUSPEND,State),
	    next_state_null(State,ok);
	true ->
	    tr_abort_req(WTPres,?SUSPEND),
	    handle_abort(State,?SUSPEND),
	    s_suspend_ind(?SUSPEND,State),
	    {reply,ok,suspended ,State}
    end;
connecting_2(A,_,State) ->
    {reply, {error,invalid_command},connecting_2,State}.

connecting_2({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{?CLASS2,Pdu} when record(Pdu,get);record(Pdu,post) ->
	    S=case catch state_null_method(WTPres,Pdu,State) of
		{error,morexceeded} ->
		      State;
		State1 ->
		      handle_release(holding,State1),
		      State1
	      end,
	    {next_state, connecting_2, S};
	{?CLASS2,Pdu} when record(Pdu,resume) -> % Abort Connect transaction
	    case ((State#state.env)#env.cap)#cap.sresumef of
		true -> 
		    tr_invoke_res(WTPres),
		    tr_abort_req(State#state.lastreq,?RESUME),
		    handle_abort(State,?RESUME),
		    s_suspend_ind(?RESUME,State),
		    s_resume_ind(State),
		    NewState=State#state{newsession=State#state.tpar,
					 lastreq=WTPres},
		    {next_state, resuming, NewState};
		false ->
		    tr_abort_req(WTPres,?DISCONNECT),
		    {next_state, connecting_2, State}
	    end;
	{?CLASS0,Pdu} when record(Pdu,disconnect) -> % Abort Connect transaction
	    tr_abort_req(State#state.lastreq,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	{?CLASS0,Pdu} when record(Pdu,suspend) -> % Abort Connect transaction
	    case ((State#state.env)#env.cap)#cap.sresumef of
		true -> 
		    tr_abort_req(State#state.lastreq,?SUSPEND),
		    handle_abort(State,?SUSPEND),
		    s_suspend_ind(?SUSPEND,State),
		    {next_state, suspended, State};
		false ->
		    ?error("tr_invoke_ind with Suspend, but cap_sresumef=FALSE",
			   [],connecting_2),
		    {next_state, connecting_2, State}
	    end;
	Other ->
	    ?warning("tr_invoke_ind with ~p got ~p",
		     [BinPdu,Other],connecting_2),
	    handle_illegal_event(State)
    end;
connecting_2({tr_result_cnf,WTPres,Exitinfo},State) ->
    {next_state, connected, State};
connecting_2({tr_abort_ind,WTPini,Info},State) ->
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(Info,State),
    next_state_null(State);
connecting_2(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],connecting_2),
    handle_illegal_event(State).

%%% ............................................................................
%%% >>>The connected state<<<
connected(disconnect_req,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?USERREQ,State),
	    next_state_null(State,ok);
	Error ->
	    {reply, Error,connected ,State}
    end;
connected(pseudo_disconnect,_,State) ->
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
connected({push_req,{Headers,CT,Data}},_,State) ->
    case tr_invoke_req(?CLASS0,#push{headers=Headers,contenttype=CT,data=Data},
		       State) of
	ok ->
	    {reply, ok,connected ,State};
	Error ->
	    {reply, Error,connected ,State}
    end;
connected({confirmed_push_req,Content},_,State) ->
    catch state_null_push(Content,State,connected);
connected(pseudo_suspend,_,State) ->
    case ((State#state.env)#env.cap)#cap.sresumef of
	false ->
	    handle_abort(State,?SUSPEND),
	    s_disconnect_ind(?SUSPEND,State),
	    next_state_null(State,ok);
	true ->
	    handle_abort(State,?SUSPEND),
	    s_suspend_ind(?SUSPEND,State),
	    {reply,ok,suspended ,State}
    end;
connected(A,_,State) ->
    {reply, {error,invalid_command},connected,State}.

connected({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{?CLASS2,Pdu} when record(Pdu,get);record(Pdu,post) ->
	    S=case catch state_null_method(WTPres,Pdu,State) of
		  {error,morexceeded} ->
		      State;
		  State1 ->
		      handle_release(holding,State1),
		      State1
	      end,
	    {next_state, connected, S};
	{?CLASS2,Pdu} when record(Pdu,resume) ->
	    case ((State#state.env)#env.cap)#cap.sresumef of
		true -> 	    
		    tr_invoke_res(WTPres),
		    handle_abort(State,?RESUME),
		    s_suspend_ind(?RESUME,State),
		    s_resume_ind(State),
		    NewState=State#state{newsession=State#state.tpar,
					 lastreq=WTPres},
		    {next_state, resuming, NewState};
		false ->
		    tr_abort_req(WTPres,?DISCONNECT),
		    {next_state, connected, State}
	    end;
	{?CLASS0,Pdu} when record(Pdu,disconnect) ->
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	{?CLASS0,Pdu} when record(Pdu,suspend) ->
	    case ((State#state.env)#env.cap)#cap.sresumef of
		true -> 
		    handle_abort(State,?SUSPEND),
		    s_suspend_ind(?SUSPEND,State),
		    {next_state, suspended, State};
		false ->
		    ?error("tr_invoke_ind with Suspend with cap_sresumef=FALSE",
			   [],connected),
		    {next_state, connected, State}
	    end;
	Other ->
	    ?warning("tr_invoke_ind with ~p got ~p",[BinPdu,Other],connected),
	    handle_illegal_event(State)
    end;
connected(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],connected),
    handle_illegal_event(State).


%%% ............................................................................
%%% >>>The suspended state<<<
suspended(disconnect_req,_,State) ->
    next_state_null(State,ok);
suspended(pseudo_disconnect,_,State) ->
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WSP specification
suspended({push_req,_},_,State) ->
    {reply, {error,no_session}, suspended, State};
suspended({confirmed_push_req,_},_,State) ->
    {reply, {error,no_session}, suspended, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
suspended(A,_,State) ->
    {reply, {error,invalid_command},suspended,State}.


suspended({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{?CLASS2,Pdu} when record(Pdu,get);record(Pdu,post) ->
	    tr_abort_req(WTPres,?SUSPEND),
	    {next_state, suspended, State};
	{?CLASS2,Pdu} when record(Pdu,resume) ->
	    tr_invoke_res(WTPres),
	    s_resume_ind(State),
	    S1=State#state{newsession=State#state.tpar,lastreq=WTPres},
	    {next_state, resuming, S1};
	{?CLASS0,Pdu} when record(Pdu,disconnect) ->
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	Other ->
	    ?warning("tr_invoke_ind with ~p got ~p",[BinPdu,Other],suspended),
	    handle_illegal_event(State)
    end;
suspended(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],suspended),
    handle_illegal_event(State).


%%% ............................................................................
%%% >>>The resuming state<<<
resuming(disconnect_req,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    WTPres=State#state.lastreq, % The Resume transaction
	    tr_abort_req(WTPres,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?USERREQ,State),
	    next_state_null(State,ok);
	Error ->
	    {reply,Error,resuming, State}
    end;
resuming(pseudo_disconnect,_,State) ->
    WTPres=State#state.lastreq, % The Resume transaction
    tr_abort_req(WTPres,?DISCONNECT),
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
resuming(resume_res,_,State) ->
    %% The Resume transaction
    case tr_result_req(#reply{status=200},State) of
	ok ->
	    NewSref=State#state.newsession, % From the Resume indication
	    handle_disconnect(NewSref,State#state.sdb),
	    wsp_db:update_session_addr(State#state.sdb,State#state.tpar,
				       NewSref),
	    handle_release(holding,State),
	    {reply,ok, resuming_2, State};
	Error ->
	    {reply,{error,invalid_command},resuming, State}
    end;
resuming(pseudo_suspend,_,State) ->
    WTPres=State#state.lastreq, % The Resume transaction
    tr_abort_req(WTPres,?SUSPEND),
    handle_abort(State,?SUSPEND),
    s_suspend_ind(?SUSPEND,State),
    {reply,ok,suspended, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
%% Note that this event, in this state, is not part of the WSP specification
resuming({push_req,_},_,State) ->
    {reply, {error,no_session}, resuming, State};
resuming({confirmed_push_req,_},_,State) ->
    {reply, {error,no_session}, resuming, State};
%% OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS OBS 
resuming(A,_,State) ->
    {reply, {error,invalid_command},resuming,State}.

resuming({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{?CLASS2,Pdu} when record(Pdu,get);record(Pdu,post) ->
	    S=case catch state_null_method(WTPres,Pdu,State) of
		  {error,morexceeded} ->
		      State;
		  State1 ->
		      handle_release(holding,State1),
		      State1
	      end,
	    {next_state, resuming, S};
	{?CLASS2,Pdu} when record(Pdu,resume) -> % Abort Resume transaction
	    tr_invoke_res(WTPres),
	    tr_abort_req(State#state.lastreq,?RESUME),
	    handle_abort(State,?RESUME),
	    s_suspend_ind(?RESUME,State),
	    s_resume_ind(State),
	    S1=State#state{newsession=State#state.tpar,lastreq=WTPres},
	    {next_state, resuming, S1};
	{?CLASS0,Pdu} when record(Pdu,suspend) -> % Abort Resume transaction
	    tr_abort_req(State#state.lastreq,?SUSPEND),
	    handle_abort(State,?SUSPEND),
	    s_suspend_ind(?SUSPEND,State),
	    {next_state, suspended, State};
	{?CLASS0,Pdu} when record(Pdu,disconnect) -> % Abort Resume transaction
	    tr_abort_req(State#state.lastreq,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	Other ->
	    ?warning("tr_invoke_ind with ~p got ~p",[BinPdu,Other],resuming),
	    handle_illegal_event(State)
    end;
resuming({tr_abort_ind,WTPini,Info},State) ->
	if
	    Info=={wsp,?DISCONNECT} ->
		handle_abort(State,?DISCONNECT),
		s_disconnect_ind(?DISCONNECT,State),
		next_state_null(State);
	    true ->
		handle_abort(State,?SUSPEND),
		s_suspend_ind(Info,State),
		{next_state, suspended, State}
	end;
resuming(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],resuming),
    handle_illegal_event(State).


%%% ............................................................................
%%% >>>The resuming_2 state<<<
resuming_2(disconnect_req,_,State) ->
    case tr_invoke_req(?CLASS0,#disconnect{server_sessionid=State#state.sid},
		       State) of
	ok ->
	    WTPres=State#state.lastreq, % The Resume transaction
	    tr_abort_req(WTPres,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?USERREQ,State),
	    next_state_null(State,ok);
	Error ->
	    {reply,Error, resuming_2, State}
    end;
resuming_2(pseudo_disconnect,_,State) ->
    WTPres=State#state.lastreq, % The Resume transaction
    tr_abort_req(WTPres,?DISCONNECT),
    handle_abort(State,?DISCONNECT),
    s_disconnect_ind(?DISCONNECT,State),
    next_state_null(State,ok);
resuming_2({push_req,{Headers,CT,Data}},_,State) ->
    case tr_invoke_req(?CLASS0,#push{headers=Headers,contenttype=CT,data=Data},
		       State) of
	ok ->
	    {reply, ok,resuming_2 ,State};
	Error ->
	    {reply, Error,resuming_2 ,State}
    end;
resuming_2({confirmed_push_req,Content},_,State) ->
    catch state_null_push(Content,State,resuming_2);
resuming_2(pseudo_suspend,_,State) ->
    WTPres=State#state.lastreq, % The Resume transaction
    tr_abort_req(WTPres,?SUSPEND),
    handle_abort(State,?SUSPEND),
    s_suspend_ind(?SUSPEND,State),
    {reply,ok, suspended, State};
resuming_2(A,_,State) ->
    {reply, {error,invalid_command},resuming_2,State}.

resuming_2({tr_invoke_ind,WTPres,{Class,_,BinPdu}},State) ->
    case catch wsp_man:tr_invoke_ind(
		  Class,BinPdu,((State#state.env)#env.cap)#cap.server_sdu,
		  State#state.env) of
	{?CLASS2,Pdu} when record(Pdu,get);record(Pdu,post) ->
	    S=case catch state_null_method(WTPres,Pdu,State) of
		  {error,morexceeded} ->
		      State;
		  State1 ->
		      handle_release(holding,State1),
		      State1
	      end,
	    {next_state, resuming_2, S};
	{?CLASS2,Pdu} when record(Pdu,resume) -> % Abort Resume transaction
	    tr_invoke_res(WTPres),
	    tr_abort_req(State#state.lastreq,?RESUME),
	    handle_abort(State,?RESUME),
	    s_suspend_ind(?RESUME,State),
	    s_resume_ind(State),
	    S1=State#state{newsession=State#state.tpar,lastreq=WTPres},
	    {next_state, resuming, S1};
	{?CLASS0,Pdu} when record(Pdu,suspend) -> % Abort Resume transaction
	    tr_abort_req(State#state.lastreq,?SUSPEND),
	    handle_abort(State,?SUSPEND),
	    s_suspend_ind(?SUSPEND,State),
	    {next_state, suspended, State};
	{?CLASS0,Pdu} when record(Pdu,disconnect) -> % Abort Resume transaction
	    tr_abort_req(State#state.lastreq,?DISCONNECT),
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	Other ->
	    ?warning("tr_invoke_ind with ~p got ~p",[BinPdu,Other],resuming_2),
	    handle_illegal_event(State)
    end;
resuming_2({tr_result_cnf,WTPres,Exitinfo},State) ->
    {next_state, connected, State};
resuming_2({tr_abort_ind,WTPini,Info},State) ->
    if
	Info=={wsp,?DISCONNECT} ->
	    handle_abort(State,?DISCONNECT),
	    s_disconnect_ind(?DISCONNECT,State),
	    next_state_null(State);
	true ->
	    handle_abort(State,?SUSPEND),
	    s_suspend_ind(Info,State),
	    {next_state, suspended, State}
    end;
resuming_2(Event,State) ->
    ?warning("Cannot handle ~p event",[Event],resuming_2),
    handle_illegal_event(State).

%% -----------------------------------------------------------------------------
%% Handles the null state in the method state table
%% On a tr_invoke_ind, 
state_null_method(WTPres,Pdu,S) ->
    if
	S#state.n_methods<((S#state.env)#env.cap)#cap.mom ->
	    ok;
	true ->
	    tr_abort_req(WTPres,?MOREXCEEDED),
	    throw({error,morexceeded})
    end,
    case wsp_method_s:start_link({Pdu,S#state.env,
				  WTPres,S#state.tpar,S#state.sdb}) of
	WSPmet when pid(WSPmet) ->
	    S#state{n_methods=S#state.n_methods+1};
	{error,Reason} ->
	    ?warning("Can't start WSP server push, got {error,~p}",
		     [Reason],state_null_method),
	    S
    end.



%% -----------------------------------------------------------------------------
%% Handles the null state in the push state table
%% On a tr_invoke_ind
%% Content={ContentType,Headers,Data}
state_null_push({Headers,CT,Data},S,SName) ->
    if
	S#state.n_pushes<((S#state.env)#env.cap)#cap.mop ->
	    ok;
	true ->
	    throw({reply,{error,max_push}, SName, S})
    end,
    Pdu=#push{type=confirmed_push,headers=Headers,contenttype=CT,data=Data},
    case wsp_push_s:start_link({Pdu,S#state.env,
				S#state.wtp,S#state.tpar,S#state.sdb}) of
	WSPpus when pid(WSPpus) ->
	    {reply,{ok,WSPpus},SName,S#state{n_pushes=S#state.n_pushes+1}};
	{error,Reason} ->
	    ?warning("Can't start WSP server push, got {error,~p}",
		     [Reason],state_null_method),
	    {reply,{error,invalid_command},SName,S}
    end.

%% -----------------------------------------------------------------------------
%% Disconnect any sessions for this Tpar
handle_disconnect(Tpar,Sdb) ->
    ok.
%    case wsp_db:lookup_session(Sdb,Tpar) of
%	{ok,WSPses} ->
%	    wsp_db:remove_session(Sdb,Sref),
%	    wsp_session_s:pseudo_disconnect(WSPses);
%	_ ->
%	    ok
%    end.

%% -----------------------------------------------------------------------------
%% Release all methods in state StateName
handle_release(holding,State) ->
    L=wsp_db:get_all_methods(State#state.sdb,State#state.tpar),
    release_methods(L,State#state.sdb);
handle_release(_,X) ->
    throw({error,{undefined_release_state,X}}).

release_methods([],Sdb) ->
    ok;
release_methods([{_,{_,released}}|L],Sdb) ->
    release_methods(L,Sdb);
release_methods([{Ref,{WSPmet,holding}}|L],Sdb) ->
    wsp_method_s:pseudo_release(WSPmet),
    release_methods(L,Sdb).


%% -----------------------------------------------------------------------------
%% Aborts all outstanding method transactions from this session
handle_abort(State,Reason) ->
    handle_abort(State#state.sdb,State#state.tpar,Reason).

handle_abort(Sdb,Sref,Reason) ->
    L=wsp_db:get_all_methods(Sdb,Sref),
    abort_methods(L,Reason).

abort_methods([],Reason) ->
    ok;
abort_methods([{_,{WSPmet,_}}|L],Reason) ->
    wsp_method_s:pseudo_abort(WSPmet,Reason),
    abort_methods(L,Reason).

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(remove_method, StateName, State) ->    
    {next_state, StateName, State#state{n_methods=State#state.n_methods-1}};
handle_event(remove_push, StateName, State) ->    
    {next_state, StateName, State#state{n_pushes=State#state.n_pushes-1}}.

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
handle_info(Reason, StateName, State) ->
    ?error("Method going down ~p",[Reason],handle_info),
    {next_state,StateName, State}.



%% -----------------------------------------------------------------------------
%% This is the catch all case in all gen_fsm's handling illegal events in
%% a particular state.
%% Incoming events from WTP
handle_illegal_event(State) ->
    tr_abort_req(?DISCONNECT,State),
    handle_abort(State#state.sdb,State#state.tpar,?DISCONNECT),
    next_state_null(State).


next_state_null(State) ->
%%    wsp_db:reset_session(State#state.sdb,self()),
    {stop,normal,State}.


next_state_null(State,Reply) ->
%%    wsp_db:reset_session(State#state.sdb,self()),
    {stop,normal,ok,State}.


%% =============================================================================
s_connect_ind(Vers,Headlist,SerCap,State) ->
    connect_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		self(),Vers,Headlist,SerCap).


s_disconnect_ind(Reason,State) ->
    R=wsp_pdu:decode_abort_reason(Reason),
    disconnect_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		   self(),R).
s_suspend_ind(Reason,State) ->
    R=wsp_pdu:decode_abort_reason(Reason),
    suspend_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
		self(),R).
s_resume_ind(State) ->
    resume_ind(wsp_db:lookup_appref(State#state.sdb,State#state.tpar),
	       self()).


tr_invoke_req(Class,Pdu,State) ->
    wsp_man:tr_invoke_req(State#state.wtp,self(),State#state.tpar,Class,Pdu,
			  State#state.env,
			  ((State#state.env)#env.cap)#cap.client_sdu).
			    
tr_invoke_res(WTPres) ->
%    Exitinfo2=wsp_bytescodes_headers:encode_headers(Exitinfo),
    wtp_responder:invoke_res(WTPres,self(),[]).

tr_result_req(Pdu,State) ->
    WTPres=State#state.lastreq,
    wsp_man:tr_result_req(WTPres,Pdu,
			   State#state.env,
			   ((State#state.env)#env.cap)#cap.client_sdu).
	
			   

tr_abort_req(WTPres,Reason) ->
    wtp_responder:abort_req(WTPres,Reason).


