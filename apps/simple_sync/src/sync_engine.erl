-module(sync_engine).
-behaviour(gen_fsm).
-export([start_link/0, message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([build_message/3, build_message/2]).
-include_lib("xmerl/include/xmerl.hrl").
-record(state, {from}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

message(FSMPid, Msg)->
    gen_fsm:sync_send_event(FSMPid, {msg, Msg}, 10000).

init([]) ->
    {ok, build_message, #state{}}.

build_message({msg, Msg}, From, State)->
    error_logger:info_msg("build_message, receive 'msg'", []),
    template(Msg),
    {next_state, build_message, State#state{from=From}}.
build_message({sync_cmd, E}, State)->
    error_logger:info_msg("build_message, receive 'sync' command", []),
    build_command(E),
    {next_state, build_message, State};
build_message({put_cmd, E}, State)->
    error_logger:info_msg("build_message, receive 'put' command", []),
    {next_state, build_message, State};
build_message({final, E}, State)->
    error_logger:info_msg("build_message, Final is the reason to terminate", []),
    {stop, normal, State};
build_message({add_cmd, E}, State)->
    error_logger:info_msg("build_message, receive 'add' command", []),
    {next_state, build_message, State};
build_message({replace_cmd, E}, State)->
    error_logger:info_msg("build_message, receive 'replace' command", []),
    {next_state, build_message, State}.

build_command(E=#xmlElement{})->
    %% - Target and Source -
    % A relative URI can be specified, if the proper absolute URI can be constructed.
    % from prefixing the respective Target or Source value from the SyncHdr to this relative URI.
    % MUST use LocURI element. CGI script parameters can be appended to the URI to perform selection filtering on the server target.
    error_logger:info_msg("bulding command...", [E#xmlElement.content]).

handle_event(_Event, StateName, State)->
    error_logger:info_msg("handle_event", [StateName]),
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State)->
    error_logger:info_msg("handle_sync_event", [StateName]),
    {reply, ok, StateName, State}.
handle_info(_Info, StateName, State)->
    error_logger:info_msg("handle_info", [StateName]),
    {next_state, StateName, State}.
terminate(Reason, StateName, State)->
    error_logger:info_msg("terminate", [StateName, Reason]),
    gen_fsm:reply(State#state.from, final),
    ok.
code_change(_OldVsn, StateName, State, _Extra)->
    {ok, StateName, State}.

template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Sync'})->
    gen_fsm:send_event(self(), {sync_cmd, E}),
    xmerl_xs:xslapply(fun template/1, E);
template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Put'})->
    gen_fsm:send_event(self(), {put_cmd, E});
template(E=#xmlElement{parents=[{'Sync',_}|_], name='Add'})->
    gen_fsm:send_event(self(), {add_cmd, E});
template(E=#xmlElement{parents=[{'Sync',_}|_], name='Replace'})->
    gen_fsm:send_event(self(), {replace_cmd, E});
template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Final'})->
    gen_fsm:send_event(self(), {final, E});
template(E)->
    xmerl_xs:built_in_rules(fun template/1, E).
