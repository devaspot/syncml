%%% File    : wap_stack_sup.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Supervisor for the WAP Stack application
%%% Created :  6 Aug 2000 by Johan Blom <johblo@dragon.cellpt.se>

%%% The supervisor supervises all stacks.
%%% When the user requests the starting of a stack is it added as a child
%%% process to this supervisor.
%%%----------------------------------------------------------------------

-module(wap_stack_sup).
-author('johblo@dragon.cellpt.se').
-behaviour(supervisor).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wap_stack_sup.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% External exports
-export([start/1,stop/1]).
-export([start_stack/3, stop_stack/1]).

%% Internal supervisor callbacks
-export([init/1]).

-define(SSVNAME,?MODULE).
-define(SSVCALL,{local,?SSVNAME}).

%%%----------------------------------------------------------------------
%%% External exports
%%%----------------------------------------------------------------------
start(StartArgs) ->
    supervisor:start_link(?SSVCALL,?SSVNAME, StartArgs).

stop(State) ->
    ok.

%%%----------------------------------------------------------------------
%%% supervisor callbacks
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(wap_stack_man) ->
    Children=[],
    {ok, {{one_for_one,1,1}, Children}}.

%%----------------------------------------------------------------------
%% start_stack(Name) -> {ok, StackSup} | {error, Reason}
%%   Name -> wspCL_wdp | wspCO_wtp_wdp
%%   StackSup -> pid()  Stack supervisor, see wap_stack_comp_sup.
%%   Reason -> term()
%% Starts a new stack of the specified type (unless it is already
%% running).
%%----------------------------------------------------------------------
start_stack(Type,WspModule,Sref) ->
    case supervisor:start_child(?SSVNAME,
				{Sref,{WspModule,start_link,[{Type,Sref}]},
				 permanent, 2000, worker,
				 [WspModule]}) of
	{ok,Wsp} when Type==wspCL_wdp ->
	    ok;
	{ok,Wsp} when Type==wspCO_wtp_wdp ->
	    ok
    end.



%%----------------------------------------------------------------------
%% stop_stack(Sref)
%%   Sref -> int()
%% Stops the stack with the specified Stack reference, returned when started.
%% NOTE: Still using the Type of the stack as Sref, this limits the maximum
%% number of concurrent stacks of the same type to 1 !!
%%----------------------------------------------------------------------
stop_stack(Sref) ->
    supervisor:terminate_child(?SSVNAME, Sref),
    supervisor:delete_child(?SSVNAME, Sref).

