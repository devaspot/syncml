-module(sync_data_item_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Value) ->
    supervisor:start_child(?MODULE, [Value]).

init([]) ->
    SyncItem = {sync_data_item, {sync_data_item, start_link, []},
    temporary, brutal_kill, worker, [sync_data_item]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [SyncItem]}}.