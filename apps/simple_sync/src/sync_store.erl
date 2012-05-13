-module(sync_store).
-export([init/0, lookup/1, store/2]).
-record(session_id_to_pid, {session_id, pid}).

init()->
    mnesia:create_table(session_id_to_pid,
        [{index, [pid]},
         {attributes, record_info(fields, session_id_to_pid)}]).

lookup(SessionID)->
    case mnesia:dirty_read(session_id_to_pid, SessionID) of
    [{session_id_to_pid, SessionID, Pid}] ->
        case is_process_alive(Pid) of
        true -> {ok, Pid};
        false -> {error, not_found}
        end;
    [] ->
    	{error, not_found}
    end.

store(SessionID, Pid) ->
    mnesia:dirty_write(#session_id_to_pid{session_id=SessionID, pid=Pid}).