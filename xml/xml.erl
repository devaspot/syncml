-module(xml).
-copyright('Copyright (c) 1997 Ericsson Telecommunications AB').

%% XML parser version 1.0
%% Author Joe Armstrong <joe@cslab.ericsson.se>
%%
%% Usage:
%%   xml:file("foo") -> {ok, ParseTree} | errors.
%%   parses foo.xml 
%%   If there are any errors the errors are written to standard output

-export([p_XMLDecl/1]).
-export([file/1,bin/1]).                            %% the public interface
-export([fatal/1, fatal/2,                    %% for reporting errors
	 warning/1, warning/2]).
-export([first/1, internal/2, internal_bin/2, expand_str/2]). %% used by other modules in the
                                              %% XML package
-import(lists,        [last/1,foreach/2]).
-import(xml_tokenise, [collect_entity/1, str2tags/1, tagbody2toks/2, tupStr/1]).

file(File) ->
    Pid = spawn_link(?MODULE, internal, [self(), File]),
    Ret = receive
	      {Pid, Reply} ->
		  Reply
	  end,
    Ret.

bin(Bin) ->
    Pid = spawn_link(?MODULE, internal_bin, [self(), Bin]),
    Ret = receive
	      {Pid, Reply} ->
		  Reply
	  end,
    Ret.

%% File1 gets itself spawned (So I'm going to break a golden rule and
%%  use lots of gets and puts (I know))
%% Here I'm going to use a process dictionary
%% {rule, Lhs} = Val  For rules
%% {gent, Key} = Val  For general entities &...
%% {pent, Key} = Val  For parameter entites %...
%% {attr, Key} = Val  For attributes
%% errors   = [Error]
%% warnings = [Warn]


internal_bin(Parent, Bin) ->
    put(errors, []),    %% no errors
    put(warnings, []),  %% or warnings
    put(ids, []),
    put(idrefs, []),
    Ret = case xml_tokenise:bin(Bin) of
	      {ok, Toks} ->
		  analyse(Toks);
	      errors ->
		  {errors,get(errors),warnings,get(warnings)}
	  end,
    Parent ! {self(), Ret}.


internal(Parent, File) ->
    put(errors, []),    %% no errors
    put(warnings, []),  %% or warnings
    put(ids, []),
    put(idrefs, []),
    Ret = case xml_tokenise:file(File) of
	      {ok, Toks} ->
		  analyse(Toks);
	      errors ->
		  {errors,get(errors),warnings,get(warnings)}
	  end,
    Parent ! {self(), Ret}.

analyse(Toks) ->
    %io:format("Analyse: ~P~n", [Toks,2500]),
    {XMLDecl, Toks1} = p_XMLDecl(Toks),
    {Misc, Toks2} = p_MiscStar(Toks1),
    Toks3 = p_doctypeDecl(Toks2),
    {XMLDecl2, ToksA} = p_XMLDecl(Toks3),
    case get(errors) of
	[] ->
	    case xml_verify:verify(ToksA) of
		{ok, Parse, Toks5} ->
		    {ok, get(warnings), get(), Parse};
		errors ->
		    {errors, get(errors),
		     warnings, get(warnings)}
	    end;
	L ->
	    {errors, get(errors), warnings, get(warnings)}
    end.

report_errors() ->
    case get(errors) of
	[] -> true;
	L  ->
	    foreach(fun(X) ->
			   io:format("*** ERROR *** ~p~n", [X])
		   end, L),
	    io:format("**** I GIVE UP ****~n"),
	    errors
    end.

%% [28]

%% <!DOCTYPE memo SYSTEM "memo.dtd">
%%    get the DTD from a file

%% <!DOCTYPE here [
%%    <!ELEMENT here (...)>
%%    ]>
%% Get the Dtd locally
 
%% <!DOCTYPE memo SYSTEM "memo.dtd" [
%%    <!ENTITY % abc "aaaa">
%%    ...]>
%% Here the Entiry and Attributes in the provided section override
%% those given in the supplied DTD

%% p_doctypeDecl(Toks) 
%%   Toks only has the simple tag structure here

p_doctypeDecl([{tag,Line,Str}|Toks]) ->
    Args = tagbody2toks(Line, Str),
    %% io:format("Args=~p~n", [Args]),
    case Args of
	['!', "DOCTYPE", Name, '[' | T1] ->
	    case last(T1) of
		']' ->
		    parse_doctype(Name, first(T1)), 
		    Toks;
		_ ->
		    fatal(Line, ["bad DOCTYPE missing ]"]),
		    Toks
	    end;
	['!', "DOCTYPE", Name, Where, {lit,_,File}] ->
	    Str1 = get_dtd(Line,Where, File),
	    Args1 = tagbody2toks(Line, Str1),
	    parse_doctype(Name, Args1), 
	    Toks;
	['!', "DOCTYPE", Name, Where, File, '['|T1] ->
	    case last(T1) of
		']' ->
		    parse_doctype(Name, first(T1)),
		    Toks;
		_ ->
		    fatal([Line,"bad DOCTYPE missing ]"])
	    end;
	Other ->
	    [{tag,Line,Str}|Toks]
    end;
p_doctypeDecl(Toks) ->
    Toks.

p_XMLDecl([{tag,Line,[$?,$x,$m,$l|T]}|Toks1]) ->
    Args = tagbody2toks(Line, T),
    %% io:format("Args=~p~n", [Args]),
    {Vsn, Args1} = p_VersionInfo(Line, Args),
    {Enc, Args2} = p_EncodingDecl(Line, Args1),
    {RM, Args3}  = p_RMDecl(Line, Args2),
    finally(Line, '?', Args3),
    {{xml,Vsn,Enc,RM}, Toks1};
p_XMLDecl([{tag,Line,[$?,$X,$M,$L|T]}|Toks1]) ->
    Args = tagbody2toks(Line, T),
    %% io:format("Args=~p~n", [Args]),
    {Vsn, Args1} = p_VersionInfo(Line, Args),
    {Enc, Args2} = p_EncodingDecl(Line, Args1),
    {RM, Args3}  = p_RMDecl(Line, Args2),
    finally(Line, '?', Args3),
    {{xml,Vsn,Enc,RM}, Toks1};
p_XMLDecl(Toks) ->
    {{xml,"1.0","",none}, Toks}.

finally(Line, H, [H]) -> true;
finally(Line, H, Args) -> 
    fatal(Line,["expecting a terminating ? found ",Args]).

p_VersionInfo(Line, [H,'=',{lit,_,"1.0"}|Toks]) ->
    case is_const(H, "VERSION") of
	true -> 
	    {"1.0", Toks};
	false -> 
	    warning(Line, [expecing,version]),
	    %% but carry on as if it were ok
	    {"1.0",['?']}
    end;
p_VersionInfo(Line, Toks) ->
    warning(Line, [bad,'XML',version,declaration]),
    %% but carry on as if it were ok
    {"1.0", ['?']}.

p_EncodingDecl(Line, [H,'=',{lit,_,Str}|Toks]) ->
    case is_const(H, "ENCODING") of
	true -> 
	    {Str, Toks};
	false -> 
	    {"", [H,'=',{lit,Str}|Toks]}
    end;
p_EncodingDecl(Line, Toks) ->
    {"", Toks}.

p_RMDecl(Line, [H,'=',{lit,Str}|Toks]) ->
    case is_const(H, "RMD") of
	true -> 
	    case Str of
		"NONE"     -> {none, Toks};
		"INTERNAL" -> {internal, Toks};
		"ALL"      -> {all, Toks};
		_ ->
		    warning(Line, ["BAD RMD value expecting NONE | INTERNAL | ALL",was,Str]),
		    {none, '?'}
	    end;
	false -> 
	    warning(Line, ["Bad XML declaration", Str]),
	    {none, '?'}
    end;
p_RMDecl(Line, Toks) ->
    {none, Toks}.
    
p_MiscStar([H|T]) ->
    case is_Misc(H) of
	true ->
	    {R1, T1} = p_MiscStar(T),
	    {[H|R1], T1};
	false ->
	    {[], [H|T]}
    end;
p_MiscStar([]) ->
    {[], []}.

is_Misc({comment,_,_})  -> true;
is_Misc({space,_,_})    -> true;
is_Misc({tag,_,[$?|_]}) -> true; % PI
is_Misc(_)              -> false.

get_dtd(Line, Type, File) ->
    io:format("[get_dtd:~p ~p]~n", [Type, File]),
    case file:read_file(File) of
	{ok, Bin} ->
	    binary_to_list(Bin);
	_ ->
	    fatal(Line,["cannot read entity ",Type,File]),
	    "<!-- error -->"
    end.

%% parse_doctype(Name, Items)
%%   Items is a list of tagged things (not macro expanded)

parse_doctype(Name, Items) ->
    put(top, tupStr(Name)),
    %% proper_list(Items),
    foreach(fun parse_dtd_element/1, Items),
    case get(errors) of
	0 -> ok;
	N -> {errors, N}
    end.

foreach1(F, [H|T]) ->
    case (catch F(H)) of
	{'EXIT', Why} ->
	    io:format("***** ~p ~p~n", [H, Why]),
	    exit(a);
	Other ->
	    true
    end,
    foreach1(F, T);
foreach1(F, []) -> [].


proper_list([H|T]) ->
    proper_list(T);
proper_list([]) ->
    true;
proper_list(O) ->
    fatal(["improper list",O]).
    
expand_macros(Line, [{pent,Name}|T]) ->
    case get({pent,Name}) of
	undefined ->
	    fatal(Line, ["undefined PE ", Name]),
	    expand_macros(Line, T);
	Str ->
	    Str1 = expand_str(Str),
	    Args1 = tagbody2toks(Line, Str1),
	    Args1 ++ expand_macros(Line, T)
    end;
expand_macros(Line, [H|T]) ->
    [H|expand_macros(Line, T)];
expand_macros(_, []) ->
    [].

parse_dtd_element({tag,Line,Str}) ->
    Args0 = tagbody2toks(Line, Str),
    Args = expand_macros(Line, Args0),
    %% io:format("Parse dtd element:~p~nExpanded:~p~n",[Args0, Args]),
    case Args  of
	['!', "ELEMENT", Name|Args1] ->
	    case xml_element:parse(Line, Args1) of
		error -> true;
		Parse -> add({rule,tupStr(Name)}, Parse)
	    end;
       	['!',"ENTITY",'%',Name,{lit,Line1,Str2}] ->
	    %% Str2 *has* now been expanded
	    add({pent,Name}, Str2);
       	['!',"ENTITY",'%',Name, Where, {lit,Line1,Str2}] ->
	    Str3 = get_dtd(Line,Where, Str2),
	    io:format("defining:~p~n",[Name]),
	    add({pent,Name}, Str3);
	['!',"ENTITY",Name,{lit,Line1,Str2}] ->
	    %% Str2 *has* now been expanded
	    add({gent,Name},Str2);
	['!',"ENTITY",Name,"CDATA", {lit,Line1,Str2}] ->
	    %% Str2 *has* now been expanded
	    add({gent,Name},Str2);
%	['?',"XML",Name|Args2] ->
%	    add({xml,Name},Args2);
	['!',"ATTLIST"|Rest] ->
	    case xml_attributes:parse_definition(Rest) of
		{ok, {Name, Val}} ->
		    add({attr,Name},Val);
		{error, X} ->
		    fatal(Line, [" bad attribute", X])
	    end;
	Args ->
	    fatal(Line, ["unrecognised dtd element",Args])
    end;
parse_dtd_element({pent, Name}) ->
    Str = require({pent,Name}),
    case str2tags(Str) of
	{ok, Args} ->
	    foreach(fun parse_dtd_element/1, Args);
	{error, N, _} ->
	    fatal([bad,Str]);
	OOO ->
	    fatal(["ooooooooo",OOO])
    end;
parse_dtd_element({comment,_,_}) ->
    true;
parse_dtd_element({space,_,_}) ->
    true;
parse_dtd_element({raw,_,Str}) ->
    Str1 = expand_str(Str),
    case str2tags(Str1) of
	{ok, Args} ->
	    foreach(fun parse_dtd_element/1, Args);
	{error, N, _} ->
	    fatal([bad,Str]);
	OOO ->
	    fatal(["ooooooooo",OOO])
    end;
parse_dtd_element(H) ->
    fatal(["NYI parse_dtd_element", H]).

fatal(Line, X) ->
    fatal(["line " ++ integer_to_list(Line) ++ " "|X]).

fatal(X) -> 
    %% io:format("Fatal:~p~n",[X]),
    put(errors, [X|get(errors)]).

warning(X) -> put(warnings, [X|get(warnings)]).

warning(Line, X) ->
    warning(["line " ++ integer_to_list(Line) ++ " "|X]).    

add(Key, Val) ->
    case get(Key) of
	undefined ->
	    put(Key, Val);
	OldVal ->
	    fatal(["redefining ",Key," from ",OldVal," to ",Val]),
	    put(Key, Val)
    end.

require(Key) ->
    case get(Key) of
	undefined -> 
	    fatal(["undefined object", Key]),
	    "*error*";
	Val -> Val
    end.
    
expand_str(Str) ->
    expand_str(Str, true).

expand_str(Str, Bool) ->
    %% io:format("Expand ~p => ", [Str]),
    Str1 = expand_str(Str, Bool, 1),
    %% io:format("~p~n", [Str1]),
    Str1.

%% keep a track of the depth of recursion to avoid run-away expansion
%% of mutually recursive entities

expand_str(_, _, 10) ->
    fatal(["Too many levels of entity expansion"]),
    "**** ERROR ****";
expand_str([H|T], PeAllowed, Level) ->
    case is_Entity_start_char(H) of
	true ->
	    case collect_entity([H|T]) of
		{{pent, Key}, T1} ->
		    case PeAllowed of
			true ->
			    Str  = require({pent,Key}),
			    Str1 = expand_str(Str, PeAllowed, Level+1),
			    Str1 ++ expand_str(T1, PeAllowed, Level);
			false ->
			    fatal(["invalid PE in string ", Key]),
			    "[** error ** " ++ Key ++ expand_str(T1, 
								 PeAllowed, 
								 Level)
		    end;
		{{charent,Int}, T1} ->
		    [Int] ++ expand_str(T1, PeAllowed, Level);
		{{gent,Key}, T1} ->
		    Str  = require({gent,Key}),
		    Str1 = expand_str(Str, PeAllowed, Level+1),
		    Str1 ++ expand_str(T1, PeAllowed, Level);
		fail ->
		    [H|expand_str(T, PeAllowed, Level)]
	    end;
	false ->
	    [H|expand_str(T, PeAllowed, Level)]
    end;
expand_str([], _, _) ->
    [].

is_Entity_start_char($%) -> true;
is_Entity_start_char($&) -> true;
is_Entity_start_char(_)  -> false.

is_const(H, Const) when list(H) ->
    case tupStr(H) of
	Const -> true;
	_ -> false
    end;
is_const(_, _) ->
    false.

first([H]) -> [];
first([H|T]) -> [H|first(T)].
