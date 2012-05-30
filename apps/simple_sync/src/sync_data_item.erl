-module(sync_data_item).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([fetch/1, add/1, replace/2, delete/1]).
-record(state, {sync_item
% Source - if external entity, then Data is not specified. In this case Data should be retrieved from the specified location.
% Data
}).

fetch(Pid)->
    error_logger:info_msg("Get item", [Pid]),
    gen_server:call(Pid, fetch).

add(SyncItem)->
    error_logger:info_msg("Add item ", [SyncItem]),
    sync_data_item_sup:start_child(SyncItem).

replace(Pid, SyncItem)->
    error_logger:info_msg("Replace item ", [Pid, SyncItem]),
    gen_server:cast(Pid, {replace, SyncItem}).

delete(Pid)->
    error_logger:info_msg("Delete item", [Pid]),
    gen_server:cast(Pid, delete).

start_link(SyncItem)->
    gen_server:start_link(?MODULE, [SyncItem], []).

init([SyncItem])->
    {ok, #state{sync_item = SyncItem}}.

handle_cast({replace, SyncItem}, State)->
    {noreply, State#state{sync_item = SyncItem}};
handle_cast(delete, State)->
    {stop, normal, State}.

handle_call(fetch, _From, State)->
    #state{sync_item=SyncItem} = State,
    {reply, {ok, SyncItem}, State}.

terminate(_Reason, _State)->
    % store.delete(self())
    ok.

handle_info(_Info, State)->
    {noreply, State}.
code_change(_OldVsn, State, _Extra)->
    {ok, State}.