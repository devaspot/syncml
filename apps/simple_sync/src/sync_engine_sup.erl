-module(sync_engine_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/0]).
-export([init/1]).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child()->
    supervisor:start_child(?MODULE, []).

init([])->
    SyncEngine = {sync_engine, {sync_engine, start_link, []},
    temporary, brutal_kill, worker, [sync_engine]},

    {ok, {{simple_one_for_one, 0, 1}, [SyncEngine]}}.