%%% File    : wap_stack_man.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WAP Stack Manager
%%% Created : 28 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wap_stack_man).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wap_stack_man.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([
	 start_stack/1,app_reg/5,reapp_reg/2,stop_stack/1,
	 stacks/0,
	 active_methods/1,active_pushes/1,active_sessions/1,
	 session_info/2,
	 active_transactions/1,

	 suspend_session/2,disconnect_session/2,abort_method/4,abort_push/4,
	 unit_method_invoke_req/4,unit_method_result_req/3,unit_push_req/3,
	 connect_req/4,

	 start_link/1,stop/0]).

%% Internal gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("wdp.hrl").

-define(SMACALL,             {global,?MODULE}).
-define(next_tid(Tid),fun(Tid) -> if Tid<255 -> Tid+1; true -> 0 end end).

-record(state,{sref,  % (int) Next Stack reference
	       tid,   % (uint8) Transaction Id for WSP/Connection-less
	       bearer,% (list) Available bearers
	       port_c,% (uint16) Client port
	       appref % (int) Application Reference
	      }).

%% -----------------------------------------------------------------------------
%%% Stack handling 
%% General Maintainence
start_stack(Type) ->
    gen_server:call(?SMACALL,{start_stack,Type}).
stop_stack(Sref) ->
    gen_server:call(?SMACALL,{stop_stack,Sref}).
app_reg(Sref,AppType,App,Address,Bearer) ->
    gen_server:call(?SMACALL,{app_reg,Sref,AppType,App,Address,Bearer}).
reapp_reg(AppRef,App) ->
    gen_server:call(?SMACALL,{reapp_reg,AppRef,App}).
stacks() ->
    gen_server:call(?SMACALL,list_stacks).

%% Individual Stack Maintainence
active_methods(Sref) ->
    gen_server:call(?SMACALL,{active_methods,Sref}).
active_pushes(Sref) ->
    gen_server:call(?SMACALL,{active_pushes,Sref}).
active_sessions(Sref) ->
    gen_server:call(?SMACALL,{active_sessions,Sref}).
active_transactions(Sref) ->
    gen_server:call(?SMACALL,{active_transactions,Sref}).
session_info(Sref,SessionRef) ->
    gen_server:call(?SMACALL,{session_info,Sref,SessionRef}).

%% Management control of the statemachines, ie event passing.
suspend_session(StackRef,SessionRef) ->
    gen_server:call(?SMACALL,{suspend_session,StackRef,SessionRef}).
disconnect_session(StackRef,SessionRef) ->
    gen_server:call(?SMACALL,{disconnect_session,StackRef,SessionRef}).
abort_method(StackRef,SesRef,MethRef,Reason) ->
    gen_server:call(?SMACALL,{abort_method,StackRef,SesRef,MethRef,Reason}).
abort_push(StackRef,SesRef,PushRef,Reason) ->
    gen_server:call(?SMACALL,{abort_push,StackRef,SesRef,PushRef,Reason}).

%% WSP Access
connect_req(Sref,Tpar,Headers,Caplist) ->
    gen_server:call(?SMACALL,{connect_req,Sref,Tpar,Headers,Caplist}).
unit_method_invoke_req(Sref,UAddr,HTTPReq,HTTPCont) ->
    gen_server:call(?SMACALL,{unit_method_invoke_req,
			      Sref,UAddr,HTTPReq,HTTPCont}).
unit_method_result_req(Sref,URef,HTTPCont) ->
    gen_server:call(?SMACALL,{unit_method_result_req,Sref,URef,HTTPCont}).
unit_push_req(Sref,UAddr,HTTPCont) ->
    gen_server:call(?SMACALL,{unit_push_req,Sref,UAddr,HTTPCont}).

%-------------------------------------------------------------------------------
% Maintenance
% Needs a manager of WSP sessions on both sides - Mobile and Server
%% start_link/1 differs from start/0 since it is started by a supervisor
%% and must return {ok,Pid} (and will only be called once if used correctly).

start_link(StartArgs) ->
    ?trace("Starting WAP stack application:~p",[StartArgs],start_link),
    case gen_server:start_link(?SMACALL,?MODULE,StartArgs,?START_OPTIONS) of
	{ok,Pid} ->
	    {ok,Pid};
	Error ->
	    ?warning("Can't start WAP stack manager:~p~n",[Error],start_link),
	    {error,cant_start_stackmanager}
    end.

stop() ->
    gen_server:call(?SMACALL,stop).

%% .............................................................................
%% Starts a database for the session data, for each WAP stack
init(StartArgs) ->
    wap_stack_sup:start(StartArgs),
    wap_stack_db:start_link(),
    ?trace("Started WAP stack server manager:",[],init),
    {ok,#state{sref=0,tid=0,port_c=40000,appref=0}}.

terminate(Reason,State) ->
    wap_stack_db:stop(),
    ?trace("Stopped WAP stack manager ~p",[Reason],terminate).

			      


%% =============================================================================
%% Maintenance
handle_call(stop, _, State) -> 
    {stop, normal, ok, State};

%% Stack handling
%% .............................................................................
handle_call({start_stack,Type},_,State) ->
    Sref=State#state.sref,
    case wap_stack_sup:start_stack(Type,wsp_man,Sref) of
	ok ->
	    NewState=State#state{sref=Sref+1},
	    ?trace("Started stack:~p of type ~p",[Sref,Type],handle_call),
	    {reply,{ok,Sref},NewState};
	Error ->
	    ?error("Can't start stack ~p",[Error],handle_call),
	    {reply,{error,cant_start_stack},State}
    end;
handle_call({stop_stack,Sref},_,State) ->
    Ans=case wap_stack_db:lookup_stack(Sref) of
	    {error,Reason} ->
		?error("Can't stop stack:~p",[Reason],handle_call),
		{error,Reason};
	    {Type,Stack} ->
		wap_stack_db:remove_stack(Sref),
		wap_stack_sup:stop_stack(Sref),
		?trace("Stopped stack:~p of type ~p",[Sref,Type],handle_call),
		ok
	end,
    {reply,Ans,State};
handle_call({app_reg,Sref,AppType,App,Address,Bearer},_,State) ->
    {NewState,Ans}=register_application(Sref,AppType,App,Address,Bearer,State),
    {reply,Ans,NewState};
handle_call({reapp_reg,AppRef,App},_,State) ->
    Ans=reregister_application(AppRef,App,State),
    {reply,Ans,State};
handle_call(stacks,_,State) ->    
    wap_stack_db:print_stacks(),
    {reply,ok,State};
handle_call({active_methods,Sref},_,State) ->
    Ans=apply_wsp_function(Sref,active_methods,[]),
    {reply,Ans,State};
handle_call({active_pushes,Sref},_,State) ->
    Ans=apply_wsp_function(Sref,active_pushes,[]),
    {reply,Ans,State};
handle_call({active_sessions,Sref},_,State) ->
    Ans=apply_wsp_function(Sref,active_sessions,[]),
    {reply,Ans,State};
handle_call({session_info,Sref,SesRef},_,State) ->
    Ans=apply_wsp_function(Sref,session_info,[SesRef]),
    {reply,Ans,State};
handle_call({active_transactions,Sref},_,State) ->
    Ans=case catch wap_stack_db:get_wtp_component(Sref) of
	    {ok,Wtp} ->
		case catch wtp_man_s:active_transactions(Wtp) of
		    ok ->
			ok;
		    {'EXIT',Reason} ->
			{error,Reason}
		end;
	    Error ->
		?error("Couldn't list active transactions",[Error],handle_call),
		{error,invalid_command}
	end,
    {reply,Ans,State};
handle_call({suspend_session,Sref,SesRef},_,State) ->    
    Ans=apply_wsp_function(Sref,suspend_session,[SesRef]),
    {reply,Ans,State};
handle_call({disconnect_session,Sref,SesRef},_,State) ->    
    Ans=apply_wsp_function(Sref,disconnect_session,[SesRef]),
    {reply,Ans,State};
handle_call({abort_method,Sref,SesRef,MethRef,Reason},_,State) ->
    Ans=apply_wsp_function(Sref,abort_method,[SesRef,MethRef,Reason]),
    {reply,Ans,State};
handle_call({abort_push,Sref,SesRef,MethRef,Reason},_,State) ->    
    Ans=apply_wsp_function(Sref,abort_push,[SesRef,MethRef,Reason]),
    {reply,Ans,State};


handle_call({connect_req,Sref,Tpar,Headers,Caplist},_,State) ->
    Ans=apply_wsp_function(Sref,connect_req,[Tpar,Headers,Caplist]),
    {reply,Ans,State};
handle_call({unit_method_invoke_req,Sref,UAddr,HTTPReq,HTTPCont},_,State) ->
    Tid=State#state.tid,
    NewState=State#state{tid=?next_tid(Tid)},
    Ans=apply_wsp_function(Sref,
			   unit_method_invoke_req,[UAddr,Tid,HTTPReq,HTTPCont]),
    {reply,Ans,NewState};
handle_call({unit_method_result_req,Sref,URef,HTTPCont},_,State) ->
    Ans=apply_wsp_function(Sref,unit_method_result_req,[URef,HTTPCont]),
    {reply,Ans,State};
handle_call({unit_push_req,Sref,UAddr,HTTPCont},_,State) ->
    Tid=State#state.tid,
    NewState=State#state{tid=if Tid<255 -> Tid+1; true -> 0 end},
    Ans=apply_wsp_function(Sref,unit_push_req,[UAddr,Tid,HTTPCont]),
    {reply,Ans,NewState};
handle_call(A,_,State) ->    
    {reply,{error,invalid_command},State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(A, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% =============================================================================
apply_wsp_function(Sref,Func,Args) ->
    case catch wap_stack_db:get_wsp_component(Sref) of
	{ok,Wsp} ->
	    case catch apply(wsp_man,Func,[Wsp|Args]) of
		ok ->
		    ok;
		{ok,Ans} ->
		    {ok,Ans};
		Error ->
		    ?error("Couldn't apply ~p got ~p",
			   [Func,Error],apply_wsp_function),
		    Error
	    end;
	Error ->
	    ?error("Couldn't get wsp component got ~p",
		   [Func,Error],apply_wsp_function),
	    Error
    end.


%% -----------------------------------------------------------------------------

%% Register an application to a stack, that in return will get a port number
%% to be used when the stack is accessed.
%% Storage includes associatons on how to access
%% - application (pid etc) from WSP
%% - stack (port number) from application
%%   + Depending on the bearer the port number might need to be further
%%     associated with e.g. a socket, used by the bearer
%% The idea is that WSP and WDP needs to check for registered applications
%% whenever they are started. Whenever an application is registred the
%% correponding values are also stored in WSP and WDP.
%% A stack do need to exist when an application is registred.
%% Returns an Application Reference. Note that a single application (AppRef)
%% might be registred on a number of WAP stacks.
%% However, the current implementation increases AppRef for each registration!
register_application(Sref,AppType,App,Address,BearerType,State) ->
    ?debug("Register application ~p on bearer ~p",[AppType,BearerType],
	   register_application),
    Stack=case {BearerType,wap_stack_db:lookup_stack(Sref)} of
	      {any_any_ipv4,{Type,{Wsp,Wdp}}} ->
		  {ok,?ANY_ANY_IPv4,Type,Wsp,Wdp};
	      {any_any_ipv4,{Type,{Wsp,_,Wdp}}} ->
		  {ok,?ANY_ANY_IPv4,Type,Wsp,Wdp};
	      {any_any_ipv4,{Type,{Wsp,_,_,Wdp}}} ->
		  {ok,?ANY_ANY_IPv4,Type,Wsp,Wdp};
	      _ ->
		  ?error("Can't register, stack not started",
			 [],register_application),
		  {error,invalid_command}
	end,
    case Stack of
	{ok,Bearer,Type1,Wsp1,Wdp1} ->
	    {NewState1,Port}=find_port(Type1,AppType,State),
	    AppRef=NewState1#state.appref,
	    NewState2=NewState1#state{appref=AppRef+1},	    
	    ok=wsp_man:app_reg(Wsp1,AppRef,App),
	    Ans2=case wdp_man:app_reg(Wdp1,AppRef,{Address,Port,Bearer}) of
		     {ok,_} ->
			 ok=wap_stack_db:insert_app(Sref,{{Address,Port,Bearer},
							  {AppRef,App}}),
			 {ok,AppRef};
		     {error,Reason2} ->
			 ?error("Can't register in WDP bearer adaptation:~p",
				[Reason2],register_application),
			 {error,invalid_command}
		 end,
	    {NewState2,Ans2};
	Ans ->
	    {State,Ans}
    end.


%% Reregister the application identifier in the various databases.
%% The related ones are: 
%%   Global: StackRef -> {{Address,Port,Bearer},{AppRef,App}}
%%   WSP: AppRef -> App
%% Note that a single application (AppRef) might be registred on a number of
%% WAP stacks.
reregister_application(AppRef,App,State) ->
    ?debug("Re-Register application ~p",[AppRef],reregister_application),
    Stack=case wap_stack_db:find_stackref(AppRef) of
	      {ok,{StRef,Tpar}} ->
		  case wap_stack_db:lookup_stack(StRef) of
		      {Type,{Wsp,Wdp}} -> {ok,StRef,Tpar,Type,Wsp,Wdp};
		      {Type,{Wsp,_,Wdp}} -> {ok,StRef,Tpar,Type,Wsp,Wdp};
		      {Type,{Wsp,_,_,Wdp}} -> {ok,StRef,Tpar,Type,Wsp,Wdp};
		      {error,Reason} -> % Database error
			  {error,Reason};
		      _ ->
			  {error,invalid_command}
		  end;
	      {error,Reason} ->
		  ?trace("Application not registered (got ~p), so reregister",
			 [Reason],reregister_application),
		  {error,application_not_registred}
	  end,
    case Stack of
	{ok,StRef1,Tpar1,Type1,Wsp1,Wdp1} ->
	    wsp_man:app_reg(Wsp1,AppRef,App),
	    wap_stack_db:insert_app(StRef1,{Tpar1,{AppRef,App}});
	Ans ->
	    Ans
    end.
    
%%----------------------------------------------------------------------
find_port(Type,proxy,State) ->
    Port=case Type of
	     wspCL_wdp -> ?WAP_WSP;
	     wspCO_wtp_wdp -> ?WAP_WSP_WTP;
	     wspCL_wtls_wdp -> ?WAP_WSP_S;
	     wspCO_wtp_wtls_wdp -> ?WAP_WSP_WTP_S
	 end,
    {State,Port};
find_port(Type,vcard,State) ->
    Port=case Type of
	     vcard_wdp -> ?WAP_VCARD;
	     vcard_wtls_wdp -> ?WAP_VCARD_S
	 end,
    {State,Port};
find_port(Type,vcalendar,State) ->
    Port=case Type of
	     vcal_wdp -> ?WAP_VCAL;
	     vcal_wtls_wdp -> ?WAP_VCAL_S
	 end,
    {State,Port};
find_port(Type,wta,State) ->
    Port=case Type of
	     wta_wdp -> ?WAP_WTA_WSP_S;
	     wta_wtp_wdp -> ?WAP_WTA_WSP_WTP_S
	 end,
    {State,Port};
find_port(_,wae_ua,State) ->
    Port=State#state.port_c,
    {State#state{port_c=Port+1},Port};
find_port(_,wta_ua,State) ->
    Port=State#state.port_c,
    {State#state{port_c=Port+1},Port};
find_port(Type,push_useragent,State) ->
    ok.

