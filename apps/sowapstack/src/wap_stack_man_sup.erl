%%%----------------------------------------------------------------------
%%% File    : wap_stack_man_sup.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Top Supervisor for the WAP stack application
%%% Created : 26 Sep 2000 by Johan Blom <johblo@dragon.cellpt.se>
%%%----------------------------------------------------------------------

-module(wap_stack_man_sup).
-author('johblo@dragon.cellpt.se').
-behaviour(supervisor).
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wap_stack_man_sup.erl,v 1.1.1.1 2001/07/04 14:51:12 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:12 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local,?MODULE},?MODULE,StartArgs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([StartModule]) ->
    Children =[{StartModule,{StartModule,start_link,[StartModule]},
		permanent,2000,worker,[StartModule]}],
    {ok,{{one_for_all,1,1}, Children}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
