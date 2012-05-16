%%% File    : wsp_man.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP Manager process
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_man).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_man.erl,v 1.1.1.1 2001/07/04 14:51:10 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:10 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% Component external interface
-export([app_reg/3,start_link/1,stop/1,
	 active_methods/1,active_pushes/1,active_sessions/1,
	 session_info/2,
	 add_current_encoding_version/2,
	 pseudo_disconnect/2,pseudo_suspend/2
	]).

%% Internal component interface
-export([negotiate_caps/2,
	 disconnect_session/3,
	 tr_invoke_ind/4,tr_result_ind/3,
	 tr_invoke_req/7,tr_result_req/4,
	 create_method_pdu/2]).

%% Internal gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("wsp.hrl").
-include("wspif.hrl").
-include_lib("kernel/include/inet.hrl").

-define(MAX_SESSIONS,100).

-record(state,{above,   % (pid) to application, i.e. "above" WSP
	       below,   % (pid) to component below WSP (WDP,WTLS or WTP)
	       sdb,     % (pid) to Session database
	       sesnum,  % (int) Number of sesions
	       asesnum, % (int) Number of running sesions
	       stackref % (int) Stack reference
	      }).

%-------------------------------------------------------------------------------
%% User/Application Interface
%% Request from the application, ie most commonly a Gateway
%% - disconnect(WSP,Sref)
%% - disconnect(WSP,Sref,Reason)

%% - method_result(WSP,{Sref,Tid},Status,ContType,Headers,Data)
%% - method_abort(WSP,{Sref,Tid},Reason)

%% - push(WSP,Sref,Content)
%% - confirmed_push(WSP,Sref,Content)

%% - respond(WSP,Sref,Atype,Data)


%% WTP Interface
%% Request from WTP, ie most commonly a reply from the server
%% - indication(Wsp,Tpar,Atype,Data)
%% - confirm(WSP,Tpar,Atype,Data)

%% Management interface
%% - pseudo_disconnect(Wsp,Sref)
%% - pseudo_suspend(Wsp,Sref)

%% -----------------------------------------------------------------------------
%% Maintenance
start_link(Args) ->
    gen_server:start_link(wsp_man,Args,?START_OPTIONS).


stop(Wsp) ->
    gen_server:call(Wsp,stop).

app_reg(Wsp,AppRef,App) ->
    gen_server:call(Wsp, {app_reg,AppRef,App}).

active_methods(Wsp) ->
    gen_server:call(Wsp,active_methods).
active_pushes(Wsp) ->
    gen_server:call(Wsp,active_pushes).
active_sessions(Wsp) ->
    gen_server:call(Wsp,active_sessions).
session_info(Wsp,Sref) ->
    gen_server:call(Wsp,{session_info,Sref}).

pseudo_disconnect(Wsp,Sref) ->
    gen_server:call(Wsp,{pseudo_disconnect,Sref}).
pseudo_suspend(Wsp,Sref) ->
    gen_server:call(Wsp,{pseudo_suspend,Sref}).

%% .............................................................................
%% Needs a manager of WSP sessions on both sides (Client and Server) thus
init({Type,Sref}) ->
    {ok,WspDB}=wsp_db:start(),    
    Wsp=self(),
    Below=case Type of
	      wspCL_wdp ->
		  {ok,Wdp}=wdp_man:start_link({Sref,Wsp}),
		  wap_stack_db:insert_stack(Sref,Type,{Wsp,Wdp}),
		  Wdp;
	      wspCL_wtls_wdp ->
		  wap_stack_db:insert_stack(Sref,Type,{Wsp,undef,undef}),
		  %%	    {ok,Wtls}=?wtls_man:start_link({Sref,Flag,Wsp}),
		  %%	    wap_stack_db:update_stack(Sref,{wtls,Wtls}),
		  %%        Wtls;
		  ok;
	      wspCO_wtp_wdp ->
		  wap_stack_db:insert_stack(Sref,Type,{Wsp,undef,undef}),
		  {ok,Wtp}=wtp_man:start_link({Sref,Wsp}),
		  B=wap_stack_db:update_stack(Sref,{wtp,Wtp}),
		  Wtp;
	      wspCO_wtp_wtls_wdp ->
		  wap_stack_db:insert_stack(Sref,Type,{Wsp,undef,undef,undef}),
		  {ok,Wtp}=wtp_man:start_link({Sref,Wsp}),
		  wap_stack_db:update_stack(Sref,{wtp,Wtp}),
		  Wtp
	  end,
    check_registered_applications(Sref,WspDB),

    ?trace("Started WSP manager ok",[],init),
    {ok,#state{below=Below,sdb=WspDB,asesnum=0,sesnum=0,stackref=Sref}}.

terminate(Reason,State) ->
    ?trace("Terminated WSP manager: ~p",[Reason],terminate),
    wsp_db:stop(State#state.sdb).


%% Only do this during restart of the stack. Note that there cannot be any
%% (WAP Stack user) applications registred during startup of the stack.
%% lookup_app returns a list with {{Address,Port,Bearer},Application} tuples
check_registered_applications(Sref,AppRef_db) ->
    lists:foreach(
      fun({UAddr,{AppRef,App}}) ->
	      wsp_db:insert_appref(AppRef_db,AppRef,App)
      end,
      wap_stack_db:lookup_app(Sref)).

%-------------------------------------------------------------------------------
% Call back functions

%% General Maintenance 
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({app_reg,AppRef,App},_,State) ->
    ok=wsp_db:insert_appref(State#state.sdb,AppRef,App),
    {reply,ok,State};
handle_call(active_methods, _From, State) ->
    wsp_db:active_methods(State#state.sdb),
    {reply, ok, State};
handle_call(active_pushes, _From, State) ->
    wsp_db:active_pushes(State#state.sdb),
    {reply, ok, State};
handle_call(active_sessions, _From, State) ->
    wsp_db:active_sessions(State#state.sdb),
    {reply, ok, State};
handle_call({session_info,Sref}, _From, State) ->
    wsp_db:session_info(State#state.sdb,Sref),
    {reply, ok, State};


%% Stack Maintenance
handle_call({pseudo_disconnect,Tpar}, _From, State) ->
    Ans=case catch wsp_db:lookup_session(State#state.sdb,Tpar) of
	    {ok,WSPses} ->
		disconnect_session(WSPses,Tpar,State#state.sdb);
	    {error,Reason} ->
		?error("Cannot (pseudo) disconnect session ~p",
		       [Reason],handle_call),
		{error,Reason}
	end,
    {reply, Ans, State};
handle_call({pseudo_suspend,Tpar}, _From, State) ->
    Ans=case catch wsp_db:lookup_session(State#state.sdb,Tpar) of
	    {ok,WSPses} ->
		pseudo_suspend(WSPses);
	    {error,Reason} ->
		?error("Cannot (pseudo) suspend session ~p",
		       [Reason],handle_call),
		{error,Reason}
	end,
    {reply, Ans, State};


%% Requests from applications "above" WSP use handle_call
handle_call({connect_req,TparIn,Headers,Cap},_,State)->
    Tpar=make_uniform_addr(TparIn),
    Ans=case catch s_connect_req(Tpar,Headers,Cap,State) of
	    {ok,WSPses} ->
		{ok,WSPses};
	    {error,Reason} ->
		{error,Reason};
	    Error -> % Error in encoding outgoing request
		{error,invalid_command}
	end,
    {reply,Ans,State};


handle_call({unit_method_invoke_req,Tpar,Tid,HTTPmeth,WSPCont},_,State) ->
    ?trace("Rcv:ied unit_method_invoke_req ~p",
	   [{Tpar,Tid,HTTPmeth}],handle_call),
    {A,S1}=case catch handle_unit_method_invoke_req(Tpar,Tid,
						    HTTPmeth,WSPCont,State) of
	       {ok,Ans,State1} ->
		   {Ans,State1};
	       {error,Reason} ->
		   ?error("unit_method_invoke_req_req ~p",[Reason],handle_call),
		   {{error,invalid_command},State}
	   end,
    {reply,A,S1};

%% Push from the Application (e.g. gw/proxy)
handle_call({unit_push_req,Tpar,Tid,{Headers,CT,Data}},_,State) ->
    ?trace("Recvied unit_push_req ~p",[{Tpar,Tid,Headers,CT}],handle_call),
    Ans=case catch td_unitdata_req(Tpar,Tid,#push{headers=Headers,
						  contenttype=CT,
						  data=Data},
				   State) of
            {ok,T} ->
                {ok,T};
            {error,Reason} ->
		?error("unit_push_req ~p",[Reason],handle_call),
                {error,invalid_command}
        end,
    {reply,Ans,State};
handle_call({unit_method_result_req,{Tpar,Tid},{Sta,Head,CT,Data}},_,State) ->
    Ans=case catch td_unitdata_req(Tpar,Tid,#reply{status=Sta,
						   headers=Head,
						   contenttype=CT,
						   data=Data},
				   State) of
            {ok,Tid} ->
                ok;
            {error,Reason} ->
		?error("unit_method_result_req ~p",[Reason],handle_call),
                {error,invalid_command}
        end,
    {reply,Ans,State};
handle_call(A,B,State) ->
    ?error("Invalid command ~p ~p",[A,B],handle_call),
    {reply,{error,invalid_command},State}.


%% In general do the following on an incoming request from WTP:
%% 1. Lookup the WSP session process on AddrQuad/Sid and, if found, return
%%    pid to that process.
%% 2. Do the initial checkings
%% 3. Forward the request to the WSP session process 

%% Note:
%% - The session entry in the database should be already created, when the
%%   request from the browser was sent.
%% - Needs to send Tpar here as we might not know the bearer

%% From WDP: (for WSP/CL only)
handle_cast({unitdata_ind,Tpar,Data},State) ->
    case catch sendto_app(Tpar,unitdata_ind,Data,State) of
	ok ->
	    ok;
	Error ->
	    ?error("unitdata_ind on ~p got ~p Data=~p",
		   [Tpar,Error,Data],handle_call)
    end,
    {noreply,State};
handle_cast({error_ind,Tpar,Data},State) ->
    case catch sendto_app(Tpar,error_ind,Data,State) of
	ok ->
	    ok;
	Error ->
	    ?error("error_ind on ~p got ~p Data=~p",
		   [Tpar,Error,Data],handle_call)
    end,
    {noreply,State};

%% From WTP: tr_invoke indication (for WSP/CO only)
handle_cast({tr_invoke_ind,WTPres,Tpar,Class,UAck,BinPdu},State) ->
    case catch tr_invoke_ind(WTPres,Tpar,Class,UAck,BinPdu,State) of
	ok ->
	    ok;
	{error,mruexceeded} ->
	    tr_abort_req(WTPres,?MRUEXCEEDED);
	{ok,sent_abort} ->
	    ok;
	Error ->
	    ?trace("Illegal Class ~p PDU received ~p on ~p Data=~p",
		   [Class,Error,Tpar,BinPdu],handle_cast),
	    tr_abort_req(WTPres,?WSP_PROTOERR)
    end,
    {noreply,State};
handle_cast({exception,Tpar,Reason},State) ->
    ?error("exception_ind ~p",[{Reason,Tpar}],handle_call),
    {noreply,State};
handle_cast(Event,State) ->
    ?error("ERROR ~p",[Event],handle_call),
    {noreply,State}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Reason,State) ->
    ?trace("handle_info ~p",[Reason],handle_info),
    {noreply, State}.


%% -----------------------------------------------------------------------------

%% Handles an incoming unit_method_invoke request from Application.
handle_unit_method_invoke_req(Tpar,Tid,Method,HTTPCont,State) ->
    Ans=case catch create_method_pdu(Method,HTTPCont) of
	    {error,invalid_command} ->
		{error,invalid_command};
	    Pdu ->
		td_unitdata_req(Tpar,Tid,Pdu,State)
	end,
    {ok,Ans,State}.


%% Handles an incoming connect request from Application.
%% 1. Lookup the WSP session process and if found return an error
%% 2. Do the initial checkings 
%% 3. Forward the request to the WSP session process 
s_connect_req(Tpar,Headers,Cap,State) ->
    case wsp_db:lookup_session(State#state.sdb,Tpar) of
	{ok,WSPses} ->
	    ?trace("Existing session on connect_req",[],handle_call),
	    disconnect_session(WSPses,Tpar,State#state.sdb),
	    state_null_client(Tpar,Headers,Cap,State);
	{error,no_session} ->
	    state_null_client(Tpar,Headers,Cap,State);
	{error,Reason} ->
	    {error,invalid_command}
	end.


%%% ............................................................................
%% Handles an incoming tr_invoke indication from WTP.
%% FIXME!!!!!!!
%% Note:
%% - The Connect PDU only can happen at the server
%% - That it actually is a Connect PDU is checked in state_null_server/4
tr_invoke_ind(WTPres,Tpar,Class,UAck,BinPdu,State) ->
    case wsp_db:lookup_session_data(State#state.sdb,Tpar) of
	{ok,{WSPses,Type}} ->
	    case BinPdu of
		<<?Connect:8,Content/binary>> ->
		    disconnect_session(WSPses,Tpar,State#state.sdb),
		    check_size(BinPdu,(#cap{})#cap.server_sdu),
		    state_null_server(Tpar,BinPdu,WTPres,State); % FIXME!
		_ ->
		    case Type of
			'client' ->
			    wsp_session_c:tr_invoke_ind(WSPses,WTPres,
							{Class,UAck,BinPdu});
			'server' ->
			    wsp_session_s:tr_invoke_ind(WSPses,WTPres,
							{Class,UAck,BinPdu})
		    end
	    end;
	_ when Class==?CLASS2 ->
	    %% Generic test case 2c with no existing session
	    check_size(BinPdu,(#cap{})#cap.server_sdu),
	    state_null_server(Tpar,BinPdu,WTPres,State);
	_ when Class==?CLASS0 -> % Generic test case 6
	    ok;
	_ -> % Generic test case 3
	    tr_abort_req(WTPres,?DISCONNECT),
	    {ok,sent_abort}
    end.


    

%% Note:
%% - This is the normal handling of a tr_invoke_ind or tr_result_ind, used
%%   whenever a session reference is known to WTP.
tr_invoke_ind(Class,BinPdu,Maxsize,Env) ->
    check_size(BinPdu,Maxsize),
    {Class,wsp_pdu:decode(BinPdu,Env)}.

tr_result_ind(BinPdu,Maxsize,Env) ->
    check_size(BinPdu,Maxsize),
    wsp_pdu:decode(BinPdu,Env).


%-------------------------------------------------------------------------------
%% Handle the NULL state in the client session protocol
%% Cap is the Capabilities given by connect_req, i.e. may not be complete
%% Headers is the Header list sent from the client and includes additional
%% headers added by the WAP stack.
state_null_client(Tpar,Hlist,Cap,State) ->
    check_client_state(Tpar,State),
    EVlist=[],
%%%    Headers=[{'encoding-version',[2]}|Hlist],
    Headers=Hlist,

    case wsp_session_c:start_link({State#state.below,State#state.sdb,
				   Tpar,Headers,Cap,EVlist}) of
	WSPses when pid(WSPses) ->
	    wsp_db:add_session(State#state.sdb,Tpar,
			       WSPses,Headers,Cap,EVlist,'client'),
	    {ok,WSPses};
	Error ->
	    ?error("Could not initiate WSP session ~p",
		   [Error],state_null_client),
	    {error,invalid_command}
    end.


%% Handle the NULL state in the session server protocol
state_null_server(Tpar,BinPdu,WTPres,State) ->
    case catch wsp_pdu:decode(BinPdu,#env{}) of
	#connect{version=?WSP_VERSION,headers=Headers,capabilities=Cap} ->
	    check_server_state(Tpar,WTPres,State),
	    EVlist=getHeaderValue('encoding-version',Headers),
	    case wsp_session_s:start_link({State#state.below,State#state.sdb,
					   Tpar,Headers,Cap,EVlist,WTPres}) of
		WSPses when pid(WSPses) ->
		    wsp_db:add_session(State#state.sdb,Tpar,
				       WSPses,Headers,Cap,EVlist,'server'),
		    ok;
		Error ->
		    ?error("Could not decode incoming PDU ~p got ~p",
			   [BinPdu,Error],state_null_server),
		    tr_abort_req(WTPres,?DISCONNECT),
		    {ok,sent_abort}
	    end;
	Error ->
	    ?error("Could not decode incoming PDU ~p got ~p",
		   [BinPdu,Error],state_null_server),
	    tr_abort_req(WTPres,?DISCONNECT),
	    {ok,sent_abort}
    end.

%%% ............................................................................
%% Checks if it possible to start a new session,
%% Possible problems are:
%% - too many sessions
%% - no application registered to listen on port
check_server_state(Tpar,WTPres,State) ->
    M_Sessions=?MAX_SESSIONS,
    if
	State#state.sesnum>M_Sessions ->
	    tr_abort_req(WTPres,?DISCONNECT),
	    throw({ok,abort_sent});
	true ->
	    case wsp_db:lookup_appref(State#state.sdb,Tpar) of
		{error,Reason} ->
		    tr_abort_req(WTPres,?DISCONNECT),
		    throw({ok,abort_sent});
		_  ->
		    ok
	    end
    end.

check_client_state(Tpar,State) ->
    M_Sessions=?MAX_SESSIONS,
    if
	State#state.sesnum>M_Sessions ->
	    throw({error,max_session});
	true ->
	    case wsp_db:lookup_appref(State#state.sdb,Tpar) of
		{error,Reason} ->
		    throw({error,no_application});
		_  ->
		    ok
	    end
    end.


%% -----------------------------------------------------------------------------
%% Disconnect any session already associated to the AddrQuad
%% The synchronous disconnect is needed because we want to make sure events are
%% generated in the right order. I.e. on a reconnect, because of a connect_req
%% on an existing session, the events received then are (in order)
%% disconnect_ind, connect_ind, and NOT the opposite order.
disconnect_session(WSPses,Sref,Sdb) ->
    wsp_db:remove_session(Sdb,Sref),
    ?trace("Disconnecting session ~p",[Sref],disconnect_session),
    ?pseudo_disconnect(WSPses).


pseudo_suspend(WSPses) ->
    gen_fsm:sync_send_event(WSPses, pseudo_suspend).
    
%% -----------------------------------------------------------------------------
%% Send to the WTP/WTLS/WDP layer

%% Request from browser
%% Sends data to WTP and does the following:
%% 2. Send the packet to WTP
%% 3. Store it in the database of pending transactions for the session
%% Returns the Transaction (Tid) of the packet sent to WTP
%% Note:
%% - With a datagram bearer we would use WDP instead, that would also include
%%   the Tid in the Spdu
tr_invoke_req(WTP,WSP,Tpar,Class,Pdu,Env,Maxsize) ->
    case catch wsp_pdu:encode(Pdu,Env) of
	BinPdu when binary(BinPdu) ->
	    if
		Maxsize/=0,size(BinPdu)>Maxsize ->
		    {error,mruexceeded};
		true ->
		    UAck=case wap_stack_db:lookup_config(wsp_use_uack) of
			     {ok,Val} -> Val;
			     Error -> throw(Error)
			 end,
		    wtp_man:invoke_req(WTP,WSP,Tpar,Class,UAck,BinPdu)
	    end;
	Error ->
	    ?error("Invalid Pdu, cannot encode ~p got ~p",[Pdu,Error],
		   tr_invoke_req),
	    {error,invalid_command}
    end.


%%% Send tr_result_req
%% Note 
%% - Always sent to the WTP responder process.
tr_result_req(WTP,Pdu,Env,Maxsize) ->
    case catch wsp_pdu:encode(Pdu,Env) of
	BinPdu when binary(BinPdu) ->
	    if
		Maxsize/=0,size(BinPdu)>Maxsize ->
		    {error,mruexceeded};
		true ->
		    wtp_responder:result_req(WTP,BinPdu)
	    end;
	Error ->
	    ?error("Invalid Pdu, cannot encode ~p got ~p",[Pdu,Error],
		   tr_result_req),
	    Error
    end.


tr_abort_req(WTPres,Reason) -> % Only abort class 1 or 2 transaction
    ?trace("Sending to WTP ~p",[{WTPres,Reason}],tr_abort_req),
    wtp_responder:abort_req(WTPres,Reason).


%% Request to send data to WDP (for WSP/CL only)
%% Always add the encoding version used in a WSP CL request
%% Note: To avoid sending this all the time the information can be cached
%% during a session. BUT, in order of doing this both the client and server
%% needs to be aware which implies that for the WSP CL case this is in general
%% not applicable.
td_unitdata_req(Tpar,Tid,Pdu,State) ->
    case catch wsp_pdu:encode(Pdu,#env{}) of
	{error,Reason} ->
	    ?error("Cannot encode PDU ~p got:~p",[Pdu,Reason],td_unitdata_req),
	    throw({error,Reason});
	{'EXIT',Reason} ->
	    ?error("Cannot encode PDU ~p got:~p",[Pdu,Reason],td_unitdata_req),
	    throw({error,invalid_command});
	RawBin ->
	    Bin=concat_binary([<<Tid>>,RawBin]),
	    wdp_man:unitdata_req(State#state.below,Tpar,Bin),
	    {ok,Tid}
    end.



%% Send respons back to the browser (application)
%% If this stack is a WSP/Connection-less
sendto_app(Tpar,unitdata_ind,<<Tid:8,BinPdu/binary>>,State) ->
    App=wsp_db:lookup_appref(State#state.sdb,Tpar),
    case catch wsp_pdu:decode(BinPdu,#env{}) of
	#reply{status=Status,headers=Headers,contenttype=CT,data=Data} ->
	    unit_method_result_ind(App,Tid,{Status,Headers,CT,Data});
	#push{headers=Headers,contenttype=CT,data=Data} ->
	    unit_push_ind(App,Tpar,{Headers,CT,Data});
	#get{type=Method,uri=Uri,headers=Headers} ->
	    unit_method_invoke_ind(App,State#state.stackref,{Tpar,Tid},
				   Method,{Uri,Headers});
	#post{type=Method,uri=Uri,headers=Headers,contenttype=CT,data=Data} ->
	    unit_method_invoke_ind(App,State#state.stackref,{Tpar,Tid},
				   Method,{Uri,Headers,CT,Data})
    end;
sendto_app(Tpar,error_ind,Reason,State) ->
    App=wsp_db:lookup_appref(State#state.sdb,Tpar),
    ok.


%-------------------------------------------------------------------------------
%% Adds a header with the currently supported Encoding-Version of this
%% implementation.
%% The default encoding version, if no other version is sent from the other
%% party, is encoding version 1.2
%% FIXME: Don't bothering with encoding version for the moment.
add_my_encoding_version(A) ->
    {A,[]};
add_my_encoding_version({U,H,C,B}) ->
    H2=[{'encoding-version',4}|H],
    {{U,H2,C,B},[{0,4}]};
add_my_encoding_version({U,H}) ->
    H2=[{'encoding-version',4}|H],
    {{U,H2},[{0,4}]}.

%% The encoding version may be known from a previous request or be included in
%% this request.
add_current_encoding_version(HTTPCont,[]) ->
    extract_encoding_version(HTTPCont);
add_current_encoding_version(HTTPCont,EVlist) ->
    EVlist.


%% Extract the encoding version from a header list.
extract_encoding_version({U,H,C,B}) ->
    getHeaderValue('encoding-version',H);
extract_encoding_version({U,H}) ->
    getHeaderValue('encoding-version',H).

%% lists:keysearch(Attr,1,List).
getHeaderValue(Attr,[]) ->
    [];
getHeaderValue(Attr,[{Attr,Value}|Rest]) ->
    Value;
getHeaderValue(Attr,[_|Rest]) ->
    getHeaderValue(Attr,Rest).

%%% ----------------------------------------------------------------------------
%% Tests so that the size of incoming/outgoing PDUs are not too big.
%% This is also Generic test case 1
%% Note:
%% - A Maxsize is set to 0, means no limit
check_size(BinPdu,0) ->
    ok;
check_size(BinPdu,MaxSize) when size(BinPdu)>MaxSize ->
    ?debug("Size exceeded... ~p>~p",[size(BinPdu),MaxSize],check_size),
    throw({error,mruexceeded});
check_size(_,_) ->
    ok.


%% -----------------------------------------------------------------------------

create_method_pdu(Method,{Uri,Headers})
  when Method==get;Method==options;Method==head;Method==delete;Method==trace ->
    #get{type=Method,uri=Uri,headers=Headers};
create_method_pdu(Method,{Uri,Headers,CT,Data})
  when Method==post;Method==put ->
    #post{type=Method,uri=Uri,headers=Headers,
	  contenttype=CT,data=Data};
create_method_pdu(_,_) ->
    throw({error,invalid_command}).


%% =============================================================================
%% Assumptions on capabillity negotiation
%% - Client: Only the client can suggest new capabillities
%% - Server: The server can never suggest any "higher value" than the client,
%%   only less or equal.
%% - Server: Empty value means unknown capability
%% - Server: If the value received is accepted, the capabillity is not returned.
%% - Client/Server: Values always accepted for Userdefined capabilities
%% Creates a complete list of capabilities, with the requested changes from
%% the WSP user taken under consideration. Remove redundant capabilities
negotiate_caps([],Caplist) ->
    {[],Caplist};
negotiate_caps(AppCaps,Caplist) ->
    Caps=create_caplist_neg(AppCaps,Caplist),
    ToCaps=remove_known_caps(AppCaps,Caps),
    {ToCaps,Caps}.


%% Create a new, complete Capability list, unconditionally.
%% That is any Cap from the client is stored in the current Capabilitylist
negotiate_new_caps([],Caplist) ->
    {[],Caplist};
negotiate_new_caps(SerCaps,Caplist) ->
    Caps=create_caplist_noneg(SerCaps,Caplist),
    ToAppCaps=remove_known_caps(SerCaps,Caplist),
    {ToAppCaps,Caps}.


%% Create a new, complete Capability list, unconditionally.
%% That is any Cap from the application is stored in the current Capabilitylist,
%% except aliases that are used only by the other side.
create_caplist_noneg([],Caplist) ->
    Caplist;
create_caplist_noneg([{Key,Value}|NewCaplist],Knowncaps) ->
    Knowncaps2=lists:keyreplace(Key,1,Knowncaps,{Key,Value}),
    create_caplist_noneg(NewCaplist,Knowncaps2).


%% Create a new, complete Capability list, only if Cap has "lower" value than
%% in Knowncaps.
create_caplist_neg([],Caplist) ->
    Caplist;
create_caplist_neg([{Key,Value}|NewCaplist],Knowncaps) ->
    Knowncaps2=case catch wsp_db:lookup_cap(Key,Knowncaps) of
		   {error,_} -> % Just ignore unknown Caps from the server.
		       Knowncaps;
		   Cval ->
		       Cap=negotiate_cap(Key,Value,Cval),
		       lists:keyreplace(Key,1,Knowncaps,Cap)
	       end,
    create_caplist_neg(NewCaplist,Knowncaps2).


%% Capability negotiation between 2 capabilities of the same type where
%% - Cval is the current, stored value, and
%% - Val the new received, suggested.
negotiate_cap(cap_client_sdu,Val,Cval) when Val<Cval ->
    {cap_client_sdu,Val};
negotiate_cap(cap_client_sdu,Val,Cval) ->
    {cap_client_sdu,Cval};
negotiate_cap(cap_server_sdu,Val,Cval) when Val<Cval ->
    {cap_server_sdu,Val};
negotiate_cap(cap_server_sdu,Val,Cval) ->
    {cap_server_sdu,Cval};
negotiate_cap(cap_cpushf,false,Cval) ->
    {cap_cpushf,false};
negotiate_cap(cap_cpushf,Val,Cval) ->
    {cap_cpushf,Val};
negotiate_cap(cap_pushf,false,Cval) ->
    {cap_pushf,false};
negotiate_cap(cap_pushf,Val,Cval) ->
    {cap_pushf,Val};
negotiate_cap(cap_sresumef,false,Cval) ->
    {cap_sresumef,false};
negotiate_cap(cap_sresumef,Val,Cval) ->
    {cap_sresumef,Val};
negotiate_cap(cap_ackhead,false,Cval) ->
    {cap_ackhead,false};
negotiate_cap(cap_ackhead,Val,Cval) ->
    {cap_ackhead,Val};
negotiate_cap(cap_mom,Val,Cval) when Val<Cval ->
    {cap_mom,Val};
negotiate_cap(cap_mom,Val,Cval) ->
    {cap_mom,Cval};
negotiate_cap(cap_mop,Val,Cval) when Val<Cval ->
    {cap_mop,Val};
negotiate_cap(cap_mop,Val,Cval) ->
    {cap_mop,Cval};
negotiate_cap(cap_em,Val,Cval) ->
    {cap_em,intersection(Val,Cval)};
negotiate_cap(cap_hcp,Val,Cval) ->
    {cap_hcp,intersection(Val,Cval)};
negotiate_cap(cap_aliases,Val,Cval) ->
    {cap_aliases,intersection(Val,Cval)};
negotiate_cap(Userdef,Val,Cval) when list(Val),list(Cval) ->
    {Userdef,intersection(Val,Cval)};
negotiate_cap(Userdef,Val,Cval) ->
    {Userdef,Val}.

%% Return all tuples common between List1 and List2 
intersection(List1,List2) ->
    L1=intersection2(List1,List2),
    intersection2(List2,L1).

intersection2([],List2) ->
    [];
intersection2([Val|List1],List2) ->
    case lists:member(Val,List2) of
	true ->
	    [Val|intersection2(List1,List2)];	    
	false ->
	    intersection2(List1,List2)
    end.

%% -----------------------------------------------------------------------------
%% Remove any caps equal to the known already, or with undefined value
remove_known_caps([],Default) ->
    [];
remove_known_caps([{Key,undef}|AppCaps],Default) ->
    remove_known_caps(AppCaps,Default);
remove_known_caps([Cap|AppCaps],Default) ->
    case lists:member(Cap,Default) of
	true ->
	    remove_known_caps(AppCaps,Default);
	false ->
	    [Cap | remove_known_caps(AppCaps,Default)]
    end.

%% -----------------------------------------------------------------------------
%% Make uniform addresses.
%% IP addresses might be given both as strings and IP-tuples
%%
%% Always use the IP-tuples internally (and just pick the first one if several
%% possible)

make_uniform_addr({{Sa,Sp,Da,Dp},Bearer}) when Bearer==?ANY_ANY_IPv4 ->
    NSa=if
	    list(Sa) ->	get_addr(Sa);
	    true -> Sa
	end,
    NDa=if
	    list(Da) ->	get_addr(Da);
	    true -> Da
	end,    
    {{NSa,Sp,NDa,Dp},Bearer};
make_uniform_addr(Tpar) ->
    Tpar.

get_addr("127.0.0.1") ->
    {127,0,0,1};
get_addr(Addr) ->
    case hd(Addr) of
	A when A>=$0,A=<$9 ->
	    case inet:gethostbyaddr(Addr) of
		{ok,Hostent} ->
		    hd(Hostent#hostent.h_addr_list);
		A1 -> throw(A1)
	    end;
	A ->
	    case inet:gethostbyname(Addr) of
		{ok,Hostent} ->
		    hd(Hostent#hostent.h_addr_list);
		A1 -> throw(A1)
	    end

    end.
