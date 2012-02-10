-module(xml_print).
-copyright('Copyright (c) 1997 Ericsson Telecommunications AB').
-import(lists, [seq/2, foreach/2, map/2, seq/2]).
-export([pp/1]).

print_list(_, _, []) -> true;
print_list(S, Str, L) ->
    io:format(S, "<p>~s<p>~n<pre>~n", [Str]),
    foreach(fun(X) ->
           io:format(S, "~p<br>~n", [X])
       end, L),
    io:format(S, "</pre>~n", []).


list(File, Out) ->
    {ok, S1} = file:open(File ++ ".xml", read),
    list1(S1, Out),
    file:close(S1).

list1(In, Out) ->
    case io:get_line(In, '') of
    eof ->
        stop;
    Chars ->
        io:put_chars(Out, quote(Chars)),
        list1(In, Out)
    end.

quote([$\\,$n|T]) -> [$<,$b,$r,$>|quote(T)];
quote([$\\,$"|T]) -> [$"|quote(T)];
quote([$<|T]) -> [$&,$l,$t,$;|quote(T)];
quote([$>|T]) -> [$&,$g,$t,$;|quote(T)];
quote([$&|T]) -> [$&,$#,$3,$8,$;|quote(T)];
quote([H|T])  -> [H|quote(T)];
quote([])     -> [].

pp(X) ->
    quote(lists:flatten(io_lib:format("~p", [X]))).
