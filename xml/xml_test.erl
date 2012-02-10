-module(xml_test).

-export([start/0, files/0]).
-import(lists, [seq/2, foreach/2, map/2, seq/2]).

files() -> 
    files("ex", 1, 13) ++ files("bug", 1, 11) ++ files("fail", 1, 3).

files(Root, Min, Max) ->
    map(fun(I) ->
           Root ++ integer_to_list(I)
       end, seq(Min, Max)).

start() ->
    {ok, S} = file:open("results.html", write),
    io:format(S, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">~n",[]),
    io:format(S, "<html><head><title>XML parser resuts</title></head>~n",[]),
    io:format(S, "<body bgcolor=\"white\">~n",[]),
    testing("ex1",S),
    io:format(S, "</body>~n</html>~n", []),
    file:close(S).

testing(File, S) ->
    io:format("testing:~s~n", [File]),
    io:format(S, "<h3>~s</h3>~n", [File]),
    io:format(S, "<pre><b>~n",[]),
    list(File, S),
    io:format(S, "</b></pre>~n",[]),
    case xml:file(File) of
    {ok, Warn, Dict, Parse} ->
        io:format(S, "<p>Parse succeeded~n", []),
        print_list(S, "Warnings", Warn),
        io:format(S, "<p>Dictionary~n<p>~n<pre>~n~s~n</pre>~n", 
              [pp(Dict)]),
        io:format(S, "<p>Parse tree~n<p>~n<pre>~n~s~n</pre>~n", 
              [pp(Parse)]);
    {errors, Errors, warnings, Warn} ->
        io:format(S, "<p>Parse failed~n", []),
        print_list(S, "Errors", Errors),
        print_list(S, "Warnings", Warn)
    end.

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

quote([$<|T]) -> [$&,$l,$t,$;|quote(T)];
quote([$>|T]) -> [$&,$g,$t,$;|quote(T)];
quote([$&|T]) -> [$&,$#,$3,$8,$;|quote(T)];
quote([H|T])  -> [H|quote(T)];
quote([])     -> [].

pp(X) ->
    quote(lists:flatten(io_lib:format("~p", [X]))).
