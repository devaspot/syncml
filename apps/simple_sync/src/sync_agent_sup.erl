-module(sync_agent_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1]).
-export([init/1]).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Msg)->
    supervisor:start_child(?MODULE, [Msg]).

init([])->
    SyncAgent = {sync_agent, {sync_agent, start_link, []},
    temporary, brutal_kill, worker, [sync_agent]},

    {ok, {{simple_one_for_one, 0, 1}, [SyncAgent]}}.