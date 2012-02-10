-module(xml_verify).
-copyright('Copyright (c) 1997 Ericsson Telecommunications AB').

%% XML verifier version 1.0
%% Author Joe Armstrong <joe@cslab.ericsson.se>
%% Just check that a given document follows the DTD
%% - easy

%% Note this uses a backtracking context free grammar
%% to parse the XML src -- the next version will be determanistic

-export([verify/1]).

-import(xml_tokenise, [tupStr/1]).
-import(lists,        [last/1, reverse/1]).
-import(xml,          [fatal/1,first/1]).

verify(Toks) ->
    case (catch parse0({rule,get(top)}, Toks)) of
	{'EXIT', Why} ->
	    fatal(["internal error ", Why]),
	    errors;
	{fail, Why} ->
	    fatal(["Parse error", Why]),
	    errors;
	{Parse, Toks1} ->
	    xml_attributes:check_ids(),
	    case get(errors) of
		[] ->
		    {ok, Parse, Toks1};
		_ ->
		    errors
	    end
    end.

parse0(Goal, Toks) ->
    Toks1 = munge_toks(Toks, []),
    %io:format("Toks1:~p~n", [Toks1]),
    parse(Goal, Toks1).

munge_toks([{raw,Line,Str}|T], L) ->
    munge_toks(T, [{raw,Line,expand_str(Str)}|L]);
munge_toks([{tag,Line,Str}|T], L) ->
    Str1 = expand_str(Str),
    Args = xml_tokenise:tagbody2toks(Line, Str1),
    Tag = case Args of
	      ['/',Name|Rest] -> {etag,Line,tupStr(Name),Rest};
	      [Name|Rest]     -> 
		  case Rest of
		      [] ->
			  {stag,Line,tupStr(Name),Rest};
		      _ ->
			  case last(Rest) of
			      '/' ->
				  {empty,Line,tupStr(Name), first(Rest)};
			      _ ->
				  {stag, Line, tupStr(Name), Rest}
			  end
		  end
	  end,
    munge_toks(T, [Tag|L]);
munge_toks([{space,_,_}|T], L) ->
    munge_toks(T, L);
munge_toks([{cdata,Line,Str}|T], L) ->
    munge_toks(T, [{raw,Line,Str}|L]);
munge_toks([H|T], L) ->
    exit({munge_toks, H});
munge_toks([], L) ->
    reverse(L).


%% parse(Goal, Toks) -> {Tree, Toks1} | {fail, Why}

parse(Goal, Toks) ->
    %io:format("---------~nGoal:~p~nToks:~p~n",[Goal,Toks]),
    R = parse1(Goal, Toks),
    %io:format("=======~nGoal:~p~nToks:~p~nResult:~p~n",[Goal,Toks, R]),
    R.

%% I don't know about you but I rather like this
%% (see the code for {alt, A, B} -- nice
%% But not super efficient

parse1({rule,Lhs}, [{stag,Line,Lhs,Args}|Toks]) ->
    Rhs = lookup(Lhs),
    case parse(Rhs, Toks) of
	{fail, X} ->
	    {fail,{in_line,Line,Lhs,X}};
	{Tree, [{etag,_,Lhs,Rest1}|Toks1]} ->
	    Args1 = check_attr(Line, Lhs, Args),
	    {{Lhs,Args1,Tree}, Toks1};
	{Tree, [{stag,Line1,Type,_}|_]} ->
	    {fail, {in_line,Line1,"unexpected start tag " ++ Type}};
	{Tree, [{etag,Line1,Type,_}|_]} ->
	    {fail, {in_line,Line1,"unexpected end tag " ++ Type}};
	X ->
	    {fail, {found, X, "no end tag for ",Lhs," start tag was in line ", Line}}
    end;
%parse1(empty, Toks) ->
%    {[], Toks};
parse1({rule,Lhs}, [{empty,Line,Lhs,Args}|Toks]) ->
    Args1 = check_attr(Line, Lhs, Args),
    {{Lhs,Args1,[]}, Toks};
parse1({seq,H,T}, Toks) ->
    case parse(H, Toks) of
	{fail, Err1} ->
	    {fail, Err1};
	{R1, Toks1} ->
	    case parse(T, Toks1) of
		{fail, Err2} ->
		    {fail, Err2};
		{R2, Toks2} ->
		    {[R1|R2], Toks2}
	    end
    end;
parse1({alt,A,B}, Toks) ->
    case parse(A, Toks) of
	{fail, _} ->
	    parse(B, Toks);
	Other ->
	    Other
    end;
parse1(pcdata, [{raw,_,Str}|Toks]) ->
    {{pcdata, Str}, Toks};
parse1(nil, Toks) ->
    {[], Toks};
parse1(fail, Toks) ->
    {fail, "no alternatives match"};
parse1({plus, X}, Toks) ->
    plus(X, Toks);
parse1({question, X}, Toks) ->
    question_mark(X, Toks);
parse1({star, X}, Toks) ->
    star(X, Toks);
parse1({plus, X}, Toks) ->
    plus(X, Toks);
parse1(Goal, Toks) ->
    %io:format("**oops Goal=~p Toks=~p~n", [Goal,hd(Toks)]),
    {fail, "nothing matches"}.
    
plus(Goal, Toks) ->
    %% 1 or more
    case parse(Goal, Toks) of
	{fail, X} ->
	    {fail, X};
	{Tree, Toks1} ->
	    case star(Goal, Toks1) of
		{fail, _} ->
		    {Tree, Toks1};
		{Tree1, Toks2} ->
		    {[Tree|Tree1], Toks2}
	    end
    end.

star(Goal, Toks) ->
    %% zero or more
    case parse(Goal, Toks) of
	{fail, _} ->
	    {[], Toks};
	{Tree, Toks1} ->
	    {Tree1, Toks2} = star(Goal, Toks1),
	    {[Tree|Tree1], Toks2}
    end.

question_mark(Goal, Toks) ->
    %% 0 or 1
    case parse(Goal, Toks) of
	{fail, _} -> 
	    {[], Toks};
	{T,Toks1} -> 
	    {T, Toks1}
    end.

expand_str(Str) ->
    xml:expand_str(Str, false).

lookup(Lhs) ->
    case get({rule,Lhs}) of
	undefined ->
	    exit({cannot_find_grammar_rule, Lhs});
	Val -> 
	    Val
    end.

check_attr(Line, Lhs, Args) -> 
    case get({attr, Lhs}) of
	undefined -> 
	    [];
	Defined ->
	    xml_attributes:verify(Line, Defined, Args)
    end.



