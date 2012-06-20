%%%----------------------------------------------------------------------
%%% File    : wap_stack.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WAP Stack Application
%%% Created : 29 Aug 2000 by Johan Blom <johblo@dragon.cellpt.se>
%%%----------------------------------------------------------------------

-module(wap_stack).
-author('johblo@dragon.cellpt.se').
-behaviour(application).
-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wap_stack.erl,v 1.2 2001/07/09 12:39:40 johblo Exp $ ').
-modified('$Date: 2001/07/09 12:39:40 $ ').
-modified_by('$Author: johblo $ ').
-vsn("1").

%% Internal application callbacks
-export([start/2, stop/1]).

-include("stacklog.hrl").

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(_Type,StartArgs) ->
    ?trace("Starting WAP stack application:~p",[StartArgs],start),
    case wap_stack_man_sup:start_link(StartArgs) of
	{ok,Pid} ->
	    {ok,Pid};
	Error ->
	    ?warning("Can't start WAP stack application:~p~n",[Error],start),
	    {error,cant_start_stackmanager}
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(_State) ->
    ?trace("WAP Stack stopped",[],stop).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
