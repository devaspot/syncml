-module(xml_tokenise).

%% -compile(export_all).

%% xml_tokenise:str2tags("<!DOCTYPE abc [
%%                          <!ENTITY % abc 123>
%%                          <!ELEMENT a (%abc;)>
%%                          ]>
%%                       <abc>
%%                       data
%%                       </abc>") =>
%%[{tag,Ln,"!DOCTYPE ...",
%% {tag,Ln,"abc"},
%% {raw,Ln,"\ndata\n"},
%% {tag,Lb,"abc"}].
%%
%% str2tags breaks down the top level structure into a sequence of tagged
%% object.

%% tagbody2toks breaks down the "inner" structure of a tag
%%
%% xml_tokenise:tagbody2toks(Line, "!ENTITY % abc 123") =>
%% ["ENTITY", '%' "123"]
%%
%% xml_tokenise:tagbody2toks("!DOCTYPE abc [ <!ENTITY ...") =>
%%   ['!', "DOCTYPE", "abc", '[', {tag,"!ENTITY"}, ...]

-export([collect_entity/1, collect_name/1, file/1, bin/1, stag/1, str2tags/1, 
	 tagbody2toks/2, tupStr/1,skip_space/2]).

-import(lists, [foreach/2, map/2, reverse/1, reverse/2]).
-import(xml,   [fatal/1,fatal/2,warning/2]).

file(File) ->
    {ok, B} = file:read_file(File ++ ".xml"),
    str2tags(binary_to_list(B)).
    
bin(Bin) ->
    str2tags(binary_to_list(Bin)).
    



test() ->
    Str = test_data(),
    test(Str).

test(Str) ->
    case str2tags(Str) of
	{ok, Tags} ->
	    io:format("str2tags(\"~s\") => ~p~n", [Str, Tags]),
	    foreach(fun({tag,Line,Str1}) ->
			   R = tagbody2toks(Line,Str1),
			   io:format("tagbody2tags(~w,\"~s\") => ~p~n", 
				     [Line,Str1, R]);
		    (_) -> true
		   end, Tags);
	Other ->
	    io:format("str2tags(\"~s\") => ~p~n", [Str, Other])
    end.

test_data() ->
    "<!DOCTYPE abc [
                    <!ENTITY % abc 123>
                    <!ELEMENT a (%abc;)>
                   ]>
     <abc>
    data
    </abc>".

str2tags(L) -> 
    case str2tags(L, new_env(), []) of
	{{_,0}, Toks} ->
	    {ok, Toks};
	{{_, N}, Toks} ->
	    errors
    end.

str2tags([$<|T], Env, Toks) ->
    %% io:format("getting a tag:~s~n", [[$<|T]]),
    {Tag, T1, Env1} = collect_tag([$<|T], Env),
    %% io:format("tag is:~p~nT1:|~s|~n", [Tag, T1]),
    str2tags(T1, Env1, [Tag|Toks]);
str2tags([H|T], Env, Toks) ->
    %% io:format("getting raw data:~s~n", [[H|T]]),
    {Raw, T1, Env1} = collect_raw([H|T], Env, []),
    %% io:format("raw is:~p~n:T1:~s~n", [Raw,T1]),
    str2tags(T1, Env1, [{raw_tag(Raw), lineno(Env), Raw}|Toks]);
str2tags([], Env, Toks) ->
    {Env, reverse(Toks)}.

raw_tag(Str) ->
    case all_spaces(Str) of
	true  -> space;
	false -> raw
    end.

all_spaces([H|T]) ->
    case is_Space(H) of
	true -> all_spaces(T);
	false -> false
    end;
all_spaces([]) ->
    true.

tagbody2toks(Line, T) ->
    tagbody2toks(T, {Line, 0}, []).

tagbody2toks([$"|T], E, Toks) ->
    {Str, T1, E1} = collect_literal($", lineno(E), T, E, []),
    tagbody2toks(T1, E1, [{lit, lineno(E), Str}|Toks]);
tagbody2toks([$'|T], E, Toks) ->
    {Str, T1, E1} = collect_literal($', lineno(E), T, E, []),
    tagbody2toks(T1, E1, [{lit, lineno(E), Str}|Toks]);
tagbody2toks([$<|T], E, Toks) ->
    {Tag, T1, Env1} = collect_tag([$<|T], E),
    tagbody2toks(T1, Env1, [Tag|Toks]);
tagbody2toks([$>|T], E, Toks) ->
    tagbody2toks(T, E, [$>|Toks]);
tagbody2toks([$ ,$-,$-|T], E, Toks) ->
    {Str, T1, E1} = collect_old_style_comment(T, E, []),
    warning(lineno(E), ["old style comment dropped " ++ Str]),
    tagbody2toks(T1, E1, Toks);
tagbody2toks([$%|T], E, Toks) ->
    case collect_entity([$%|T], E) of
	fail ->
	    tagbody2toks(T, E, ['%'|Toks]);
	{Entity, T1, E1} ->
	    tagbody2toks(T1, E1, [Entity|Toks])
    end;
tagbody2toks([H|T], E, Toks) ->
    case is_Space(H) of
	true ->
	    {T1, E1} = skip_space([H|T], E),
	    tagbody2toks(T1, E1, Toks);
	false ->
	    case is_Name(H) of
		true ->
		    {Str, T1, E1} = collect_name([H|T], E),
		    tagbody2toks(T1, E1, [Str|Toks]);
		false ->
		    tagbody2toks(T, E, [list_to_atom([H])|Toks])
	    end
    end;
tagbody2toks([], E, Toks) ->
    reverse(Toks).

collect_literal(Stop, StartLn, [Stop|T], E, L) ->
    {reverse(L), T,  E};
collect_literal(Stop, StartLn, [H|T], E, L) ->
    collect_literal(Stop, StartLn, T, bump_line(H, E), [H|L]);
collect_literal(Stop, StartLn, [], E, L) ->
    literal_error(StartLn, L, E).

literal_error(Line, Str, E) ->
    context(reverse(Str)),
    eof_error("Literal which started in line " ++ integer_to_list(Line),E).

context(Str) ->
    io:format("** context ** [~s]~n", [lists:sublist(Str, 20)]).

collect_name(Str) ->
    case collect_name(Str, {1,0}) of
	{Name, T, {_,0}} ->
	    {Name, T};
	_ ->
	    fail
    end.

collect_name([H|T], E) ->
    case is_Name(H) of
	true ->
	    {Str, T1, E1} = collect_while(fun is_NameChar/1, T, E),
	    {[H|Str], T1, E1};
	false ->
	    {[], [H|T], E}
    end;
collect_name([], E) ->
    {[], [], E}.

stag([$?|T]) ->
   {Str, T1, _} = collect_name(T, {0,0}),
   [$?|tupStr(Str)];
stag([$!|T]) ->
    {Str, T1, _} = collect_name(T, {0,0}),
    [$!|tupStr(Str)];
stag(T) ->
    {Str, T1, _} = collect_name(T, {0,0}),
    tupStr(Str).

%% Character predictes

%%  [3] MiscName ::= '.' | '-' | '_' | ':' | CombiningChar | Ignorable |
%%                   Extender
%%  [4] NameChar ::= Letter | Digit | MiscName
%%  [5]     Name ::= (Letter | '_' | ':') (NameChar)*
%%  [6]    Names ::= Name (S Name)*
%%  [7]  Nmtoken ::= (NameChar)+
%%  [8] Nmtokens ::= Nmtoken (S Nmtoken)*

%% CombiningChar and Ignorable and Extender have been ignored
is_MiscName($.) -> true;
is_MiscName($-) -> true;
is_MiscName($_) -> true;
is_MiscName($:) -> true;
is_MiscName(_)  -> false.

is_NameChar(X) ->
    case is_Letter(X) of
	true  -> true;
	false -> case is_Digit(X) of
		     true  -> true;
		     false -> is_MiscName(X)
		 end
    end.

is_Name($:) -> true;
is_Name($_) -> true;
is_Name(H)  -> is_Letter(H).

is_Space($ ) -> true;
is_Space($\t) -> true;
is_Space($\n) -> true;
is_Space(_) -> false.

is_SimpleDataChar($<) -> false;
is_SimpleDataChar($&) -> false;
is_SimpleDataChar(_) -> true.

is_Letter(X) when $a =< X, X =< $z -> true;
is_Letter(X) when $A =< X, X =< $Z -> true;
is_Letter(_) -> false.

is_Digit(X)  when $0 =< X, X =< $9 -> true;
is_Digit(_) -> false.

is_HexDigit(X) when X =< $0, X =< $9 -> true;
is_HexDigit(X) when X =< $a, X =< $z -> true;
is_HexDigit(X) when X =< $A, X =< $Z -> true;
is_HexDigit(_) -> false.

%% collect_tag(T, E) -> {Str, T', E'}
%% This collects all the stuff inside a tag as a single string
%% This is called at the top level

collect_tag([$<,$!,$-,$-|T], E) ->
    {Str, T1, E1} = collect_comment(T, E, []),
    {{comment, lineno(E), Str}, T1, E1};
collect_tag([$<,$!,$[,$C,$D,$A,$T,$A,$[|T], E) ->
    {Str, T1, E1} = collect_cdata(T, E, []),
    {{cdata, lineno(E), Str}, T1, E1};
collect_tag([$<|T], E) ->
    {Str, T1, E1} = collect_tag_args(T, 1, E, []),
    {{tag,lineno(E), Str}, T1, E1}.

collect_tag_args([$"|T], Level, E, L) ->
    {Str, T1, E1} = collect_literal($", lineno(E), T, E, [$"]),
    collect_tag_args(T1, Level, E1, [$"|reverse(Str, L)]);
collect_tag_args([$'|T], Level, E, L) ->
    {Str, T1, E1} = collect_literal($', lineno(E), T, E, [$']),
    collect_tag_args(T1, Level, E1, [$'|reverse(Str, L)]);
collect_tag_args([$<|T], Level, E, L) ->
    collect_tag_args(T, Level+1, E, [$<|L]);
collect_tag_args([$>|T], 1, E, L) ->
    {reverse(L), T, E};
collect_tag_args([$>|T], Level, E, L) ->
    collect_tag_args(T, Level-1, E, [$>|L]);
collect_tag_args([$ ,$-,$-|T], Level, E, L) ->
    {Str, T1, E1} = collect_old_style_comment(T, E, []),
    warning(lineno(E), ["old style comment dropped " ++ Str]),
    collect_tag_args(T1, Level, E1, L);
collect_tag_args([H|T], Level, E, L) ->
    collect_tag_args(T, Level, bump_line(H, E), [H|L]);
collect_tag_args([], Level, E, L) ->
    eof_error("in a tag", E).

collect_old_style_comment([$-,$-|T], E, L) ->
    {reverse(L), T, E};
collect_old_style_comment([H|T], E, L) -> 
    collect_old_style_comment(T, bump_line(H, E), [H|L]);
collect_old_style_comment([], E, L) -> 
    eof_error("Old_Style_Comment", E).

collect_comment([$-,$-,$>|T], E, L) ->
    {reverse(L), T, E};
collect_comment([$-,$-,H|T], E, L) -> 
    io:format("*** warning \"--\" in comment~n", []),
    collect_comment(T, E, [$-,$-|L]);
collect_comment([H|T], E, L) -> 
    collect_comment(T, bump_line(H, E), [H|L]);
collect_comment([], E, L) -> 
    eof_error("Comment", E).

collect_xml([$?,$>|T], E, L) ->
    {reverse(L), T, E};
collect_xml([H|T], E, L) -> 
    collect_xml(T, bump_line(H, E), [H|L]);
collect_xml([], E, L) -> 
    eof_error("?XML", E).

collect_cdata([$],$],$>|T], E, L) ->
    {reverse(L), T, E};
collect_cdata([H|T], E, L) -> 
    collect_cdata(T, bump_line(H, E), [H|L]);
collect_cdata([], E, L) -> 
    eof_error("CDATA", E).

collect_raw([$<|T], E, L) ->
    {reverse(L), [$<|T], E};
collect_raw([H|T], E, L) ->
    collect_raw(T, bump_line(H, E), [H|L]);
collect_raw([], E, L) ->
    {reverse(L), [], E}.
	    
collect_string([$<|T], E, L) ->
    {reverse(L), [$<|T], E};
collect_string([$&|T], E, L) ->
    {reverse(L), [$&|T], E};
collect_string([H|T], E, L) ->
    collect_string(T, bump_line(H, E), [H|L]);
collect_string([], E, L) ->
    {reverse(L), [], E}.

collect_entity(Str) ->
    case collect_entity(Str, new_env()) of
	{Entity, Str1, _} ->
	    {Entity, Str1};
	fail ->
	    fail
    end.

collect_entity([$%|T], E) ->
    case collect_name(T, E) of
	{Str, [$;|T1], E1} ->
	    {{pent,Str}, T1, E1};
	_ ->
	    fail
    end;
collect_entity([$&,$#|T], E) ->
    case collect_while(fun is_Digit/1, T, E) of
	{Str, [$;|T1], E1} ->
	    {{charent, list_to_integer(Str)}, T1, E1};
	_ ->
	    fail
    end;
collect_entity([$&|T], E) -> 
    case collect_name(T, E) of
	{Str, [$;|T1], E1} ->
	    {{gent, Str}, T1, E1};
	_ ->
	    fail
    end;
collect_entity(T, E) ->
    fail.

eof_error(Where, E) ->
    fatal(lineno(E), ["*** Unexpected EOF in ", Where]),
    {"", [], bump_errors(E)}.

new_env() -> {1,0}.

bump_line($\n, {N,M}) -> {N+1, M};
bump_line(_, X) -> X.

bump_errors({N,M}) -> {N, M+1}.

lineno({N,M}) -> N.

skip_space([], E) -> {[], E};
skip_space([H|T], E) ->
    case is_Space(H) of
	true -> skip_space(T, bump_line(H, E));
	false -> {[H|T], E}
    end.

%+type collect_while((X) -> bool(), [X], env()) -> {[X], [X], env()}.

collect_while(Fun, T, E) -> collect_while(Fun, T, [], E).

collect_while(Fun, [], L, E) ->
    {reverse(L), [], E};
collect_while(Fun, [H|T], L, E) ->
    case Fun(H) of
	true -> 
	    collect_while(Fun, T, [H|L], bump_line(H, E));
	false -> 
	    {reverse(L), [H|T], E}
    end.

tupStr(Str) when list(Str) -> map(fun tupChar/1, Str);
tupStr(Other) -> fatal(["tupStr bad arg",Other]),"error".

tupChar(X) when $a =< X, X =< $z ->  X - $a + $A;
tupChar(X) -> X.