-module(sync_server_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child([InitData]) ->
    supervisor:start_child(?MODULE, [InitData]).

init([])->
    SimpleSync = {simple_sync, {simple_sync, start_link, []},
    temporary, brutal_kill, worker, [simple_sync]},

    {ok, {{simple_one_for_one, 0, 1}, [SimpleSync]}}.