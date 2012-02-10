%%% File    : wap_log.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Simple logger
%%% Created : 30 Apr 2001 by Johan Blom <johblo@dragon.cellpt.se>

-module(log).
-author('johblo@dragon.cellpt.se').

-export([log/8]).

%log(Level, Str, Args, Mod, Func, Line, Pid, TS)
%  when Level == dbg; Level == tra ->
%    silent;
log(Level, Str, Args, Mod, Func, Line, Pid, TS) ->
    {_,_,MicroSecs} = TS,
    {{Y,Mon,D},{H,M,S}} = calendar:now_to_universal_time(TS),
    Timestamp = lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w,~2.2.0w:~2.2.0w:~6.3.0f",
                        [Y,Mon,D,H,M,S+(MicroSecs/1000000)])),
    Format = lists:append(["~s:~-3w:~15w~w:~-20w:~-4w: ", Str, "\n"]),
    List = [Timestamp,Level,Mod,Pid,Func,Line] ++ Args,
    safe_format(Format,List).


safe_format(Pid,Fmt,Args) ->
    case catch io:format(Pid,Fmt,Args) of
    {'EXIT',{badarg,_}} ->
        case log_pid() of
        {init,Init} when Init == Pid ->
            io:format("Bad io:format Fmt = ~p Args = ~p\n",[Fmt,Args]);

        _ ->
            safe_format(list_to_pid("<0.0.0>"),Fmt,Args)
        end;
    _ ->
        ok
    end.
                             
safe_format(Fmt,Args) ->
    case catch io:format(Fmt,Args) of
    {'EXIT',{badarg,_}} ->
        {_,LogPid} = log_pid(),
        safe_format(LogPid,Fmt,Args);
    _ ->
        ok
    end.

log_pid() ->
    case process_info(self(),group_leader) of
    {group_leader,GL} ->
        Ref = erlang:monitor(process,GL),
        receive
        {'DOWN',Ref,_,_,_} ->
            erlang:demonitor(Ref),
            {init,list_to_pid("<0.0.0>")}
        after 0 ->
            erlang:demonitor(Ref),
            {group_leader,GL}
        end;
    _ ->
        {init,list_to_pid("<0.0.0>")}
    end.
