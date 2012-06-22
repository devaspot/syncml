-module(simple_sync_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SyncAgentSup = {sync_agent_sup, {sync_agent_sup, start_link, []},
    permanent, 2000, supervisor, [sync_agent]},

    SyncServerSup = {sync_server_sup, {sync_server_sup, start_link, []},
    temporary, brutal_kill, worker, [simple_sync]},

    {ok, {{one_for_one, 4, 3600}, [SyncAgentSup, SyncServerSup]}}.

