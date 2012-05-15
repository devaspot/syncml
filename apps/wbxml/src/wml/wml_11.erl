-module(wml_11).
-author('maxim@synrc.com').
-export([create_db/0,lookup_tag/1,lookup_attribute/2]).

%% xmerl element encoding callbacks
-export([wml/3,card/3,
     do/3,onevent/3,
     head/3,template/3,access/3,meta/3,
     go/3,prev/3,refresh/3,noop/3,
     postfield/3,
     setvar/3,
     select/3,optgroup/3,option/3,input/3,fieldset/3,timer/3,
     img/3,
     anchor/3,a/3,
     table/3,tr/3,td/3,
     em/3,strong/3,b/3,i/3,u/3,big/3,small/3,p/3,br/3]).
-export(['#xml-inheritance#'/0]).

%% xmerl attribute encoding callbacks
-export([align/1,content/1,emptyok/1,forua/1,
     href/1,'http-equiv'/1,method/1,mode/1,multiple/1,newcontext/1,
     optional/1,sendreferer/1,src/1,ordered/1,type/1]).

-define(SPACE,32).

-include("../wbxml_bytecodes.hrl").
-include("wml_11_bytecodes.hrl").
-include("../xmerl.hrl").
-include("../wbxml_log.hrl").

%%% Callbacks to to xmerl ------------------------------------------------------
'#xml-inheritance#'() -> [].

%%% ..........................................................................
%%% Decks and Cards
%%% Valid Data = {seq,[{'?',head},{'?',template},{'+',card}]}
%%% This is the Root element!!
wml(Data,Attrs,US) ->
    {wml,encode_tag(?WML_wml,Attrs,Data,US)}.

%%% Valid Data = {seq,[{'*',onevent},{'?',timer},{'*',{choice,[do,p]}}]}
card(Data,Attrs,US) ->
    {card,encode_tag(?WML_card,Attrs,Data,US)}.

%%% ..........................................................................
%%% Event Bindings
%%% Valid Data = {choice,[go,prev,noop,refresh]}
do(Data, Attrs,US) ->
    {do,encode_tag(?WML_do,Attrs,Data,US)}.

%%% Valid Data = {choice,[go,prev,noop,refresh]}
onevent(Data, Attrs,US) ->
    {onevent,encode_tag(?WML_onevent,Attrs,Data,US)}.

%%% ..........................................................................
%%% Deck-level declarations
%%% Valid Data = {'+',{choice,[access,meta]}}
head(Data, Attrs,US) ->
    {head,encode_tag(?WML_head,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,[do,onevent]}}
template(Data, Attrs,US) ->
    {template,encode_tag(?WML_template,Attrs,Data,US)}.

%%% Valid Data = empty
access(Data, Attrs,US) ->
    {access,encode_tag(?WML_access,Attrs,Data,US)}.

%%% Valid Data = empty
meta(Data, Attrs,US) ->
    {meta,encode_tag(?WML_meta,Attrs,Data,US)}.


%%% ..........................................................................
%%% Tasks
%%% Valid Data = {'*',{choice,[postfield,setvar]}}
go(Data, Attrs,US) ->
    {go,encode_tag(?WML_go,Attrs,Data,US)}.

%%% Valid Data = {'*',{seq,[setvar]}}
prev(Data, Attrs,US) ->
    {prev,encode_tag(?WML_prev,Attrs,Data,US)}.

%%% Valid Data = {'*',{seq,[setvar]}}
refresh(Data, Attrs,US) ->
    {refresh,encode_tag(?WML_refresh,Attrs,Data,US)}.

%%% Valid Data = empty
noop(Data, Attrs,US) ->
    {noop,encode_tag(?WML_noop,Attrs,Data,US)}.


%%% ..........................................................................
%%% Postfield
%%% Valid Data = empty
postfield(Data, Attrs,US) ->
    {postfield,encode_tag(?WML_postfield,Attrs,Data,US)}.


%%% ..........................................................................
%%% Variables
%%% Valid Data = empty
setvar(Data, Attrs,US) ->
    {setvar,encode_tag(?WML_setvar,Attrs,Data,US)}.


%%% ..........................................................................
%%% Card Fields
%%% Valid Data = {'+',{choice,[optgroup,option]}}
select(Data, Attrs,US) ->
    {select,encode_tag(?WML_select,Attrs,Data,US)}.

%%% Valid Data = {'+',{choice,[optgroup,option]}}
optgroup(Data, Attrs,US) ->
    {optgroup,encode_tag(?WML_optgroup,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',onevent]}}
option(Data, Attrs,US) ->
    {option,encode_tag(?WML_option,Attrs,Data,US)}.

%%% Valid Data = empty
input(Data, Attrs,US) ->
    {input,encode_tag(?WML_input,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table,input,select,fieldset,do]}}
fieldset(Data, Attrs,US) ->
    {fieldset,encode_tag(?WML_fieldset,Attrs,Data,US)}.

%%% Valid Data = empty
timer(Data, Attrs,US) ->
    {timer,encode_tag(?WML_timer,Attrs,Data,US)}.



%%% ..........................................................................
%%% Images
%%% Valid Data = empty
img(Data, Attrs,US) ->
    {img,encode_tag(?WML_img,Attrs,Data,US)}.


%%% ..........................................................................
%%% Anchor
%%% Valid Data = {'*',{choice,['#PCDATA',br,img,go,prev,refresh]}}
anchor(Data, Attrs,US) ->
    {anchor,encode_tag(?WML_anchor,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',br,img]}}
a(Data, Attrs,US) ->
    {a,encode_tag(?WML_a,Attrs,Data,US)}.


%%% ..........................................................................
%%% Tables
%%% Valid Data = {'+',{seq,[tr]}}
table(Data, Attrs,US) ->
    {table,encode_tag(?WML_table,Attrs,Data,US)}.

%%% Valid Data = {'+',{seq,[td]}}
tr(Data, Attrs,US) ->
    {tr,encode_tag(?WML_tr,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a]}}
td(Data, Attrs,US) ->
    {td,encode_tag(?WML_td,Attrs,Data,US)}.


%%% ..........................................................................
%%% Text layout and line breaks
%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
em(Data, Attrs,US) ->
    {em,encode_tag(?WML_em,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
strong(Data, Attrs,US) ->
    {strong,encode_tag(?WML_strong,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
b(Data, Attrs,US) ->
    {b,encode_tag(?WML_b,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
i(Data, Attrs,US) ->
    {i,encode_tag(?WML_i,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
u(Data, Attrs,US) ->
    {u,encode_tag(?WML_u,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
big(Data, Attrs,US) ->
    {big,encode_tag(?WML_big,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%               anchor,a,table]}}
small(Data, Attrs,US) ->
    {small,encode_tag(?WML_small,Attrs,Data,US)}.

%%% Valid Data = {'*',{choice,['#PCDATA',em,strong,b,i,u,big,small,br,img,
%%%      anchor,a,table,input,select,fieldset,do]}}
p(Data, Attrs, US) ->
    {p,encode_tag(?WML_p,Attrs,Data,US)}.

%%% Valid Data = empty
br(Data, Attrs,US) ->
    {br,encode_tag(?WML_br,Attrs,Data,US)}.

%%% ----------------------------------------------------------------------------
encode_tag(Code,Attrs,Data,US=#wbxml_info{str_table=StrTbl}) ->
    case {Attrs,Data} of
    {[],[]} ->
        {StrTbl,[Code]};
    {[],Data} ->
        {US1,EncV}=encode_data(Data,US),
        {US1#wbxml_info.str_table,[Code+64]++EncV};
    {Attrs,[]} ->
        US1=US#wbxml_info{cur_element=Code},
        {StrTbl2,EncV}=encode_attributes(Attrs,US1),
        {StrTbl2,[Code+128]++EncV};
    {Attrs,Data} ->
        US1=US#wbxml_info{cur_element=Code},
        {StrTbl2,EncV1}=encode_attributes(Attrs,US1),
        {US2,EncV2}=encode_data(Data,US1#wbxml_info{str_table=StrTbl2}),
        {US2#wbxml_info.str_table,[Code+192]++EncV1++EncV2}
    end.



%% Data can be a an encoded element or PCDATA
%% Note:
%% - Uses the fact that xmerl is
%%   + representing numeric references in a sublist.
%%   + always put such entity as the first character in the xmlText value.
%%   The default behaviour is to simply flatten this away when accumulating.
encode_data([],US) ->
    {US,[?WBXML_END]};
encode_data([#xmlText{value=V}|Rest],US) ->
    {US2,EncV1}=encode_text_with_entity(US,V),
    {US3,EncV2}=encode_data(Rest,US2),
    {US3,EncV1++EncV2};
encode_data([{_,_,Data}|Rest],US) ->
    {US2,EncV}=encode_data(Rest,US),
    {US2,Data++EncV}.

%%% Note:
%%% - There can only be one matching attribute_start value, but several
%%%   attribute_value values! 
%%% - The attribute_start value may include the attribute_value as well in a
%%%   single code..
%%% - It may be parts of the value (first part only) or the whole
%%%   attribute_value
%%% **Thus for maximum speed and compression...
%%% -  check if the name of the parameter is a attribute_start token with an
%%%   encoding. Note that some start tokens have several possible encodings,
%%%   each with (the beginning of) an attribute_value token associated to it.
%%% - To a name there might be an integer (the encoding) or an atom 
%%%   (function name that contains clauses with the possible values
%%% - The rest of the value, not encoded, is matched against all the
%%%   attribute_value substrings. This is made by recursively go through all the
%%%   attribute_value encodings and match with string:str(String,Substring), to
%%%   get the start position, until 0 is returned (not found)
encode_attributes([],#wbxml_info{str_table=StrTbl}) ->
    {StrTbl,[?WBXML_END]};
encode_attributes([#xmlAttribute{name=K,value=V}|Attrs],US) ->
    {V1,KCode}=encode_attribute_start(K,V),
    {US2,KValue}=encode_attribute_value(V1,US#wbxml_info{cur_attribute=KCode}),
    {StrTbl3,EncV}=encode_attributes(Attrs,US2),
    {StrTbl3,KCode++KValue++EncV}.

encode_attribute_start(K,V) ->
    case lists:keysearch(K,1,?WML_ATTRIBUTE_START_TOKENS_ENC) of
    {value,{_,Code}} when integer(Code) ->
        {V,[Code]};
    {value,{_,Func}} ->
        apply(?MODULE,Func,[V])
    end.

%%% Attribute encodings of combined name and value encodings.
align("bottom") -> {[],[?WML_align_bottom]};
align("center") -> {[],[?WML_align_center]};
align("left") ->   {[],[?WML_align_left]};
align("middle") -> {[],[?WML_align_middle]};
align("right") ->  {[],[?WML_align_right]};
align("top") ->    {[],[?WML_align_top]};
align(V) ->        {V,[?WML_align]}.

content("application/vnd.wap.wmlc;charset"++V1) ->
    {V1,[?WML_content_application_vndwapwmlc]};
content(V) ->
    {V,[?WML_content]}.

emptyok("false") -> {[],[?WML_emptyok_false]};
emptyok("true") -> {[],[?WML_emptyok_true]}.

forua("false") -> {[],[?WML_forua_false]};
forua("true") -> {[],[?WML_forua_true]}.

href("http://"++V1) -> {V1,[?WML_href_http]};
href("https://"++V1) -> {V1,[?WML_href_https]};
href(V) -> {V,[?WML_href]}.

'http-equiv'("Content-Type"++V1) -> {V1,[?WML_httpequiv_ContentType]};
'http-equiv'("Expires"++V1) -> {V1,[?WML_httpequiv_Expires]};
'http-equiv'(V) -> {V,[?WML_httpequiv]}.

method("get") -> {[],[?WML_method_get]};
method("post") -> {[],[?WML_method_post]}.
    
mode("nowrap") -> {[],[?WML_mode_nowrap]};
mode("wrap") -> {[],[?WML_mode_wrap]}.

multiple("false") -> {[],[?WML_multiple_false]};
multiple("true") -> {[],[?WML_multiple_true]}.

newcontext("false") -> {[],[?WML_newcontext_false]};
newcontext("true") -> {[],[?WML_newcontext_true]}.

optional("false") -> {[],[?WML_optional_false]};
optional("true") -> {[],[?WML_optional_true]}.

sendreferer("false") -> {[],[?WML_sendreferer_false]};
sendreferer("true") -> {[],[?WML_sendreferer_true]}.

src("http://"++V1) -> {V1,[?WML_src_http]};
src("https://"++V1) -> {V1,[?WML_src_https]};
src(V) -> {V,[?WML_src]}.

ordered("false") -> {[],[?WML_ordered_false]};
ordered("true") -> {[],[?WML_ordered_true]}.

type("accept") -> {[],[?WML_type_accept]};
type("delete") -> {[],[?WML_type_delete]};
type("help") -> {[],[?WML_type_help]};
type("password") -> {[],[?WML_type_password]};
type("onpick") -> {[],[?WML_type_onpick]};
type("onenterbackward") -> {[],[?WML_type_onenterbackward]};
type("onenterforward") -> {[],[?WML_type_onenterforward]};
type("ontimer") -> {[],[?WML_type_ontimer]};
type("options") -> {[],[?WML_type_options]};
type("prev") -> {[],[?WML_type_prev]};
type("reset") -> {[],[?WML_type_reset]};
type("vnd."++V1) -> {V1,[?WML_type_vnd]};
type(V) -> {V,[?WML_type]}.


%%% Assumptions to speed up the parsing:
%%% - Encodable strings do only occurs once in a tested substring
%%% - Two types of strings, some occurs in the front, others in the middle
%%% - Only do checks for the above cases
encode_attribute_value("accept",US) -> {US,[?WML_ATTVAL_accept]};
encode_attribute_value("bottom",US) -> {US,[?WML_ATTVAL_bottom]};
encode_attribute_value("clear",US) -> {US,[?WML_ATTVAL_clear]};
encode_attribute_value("delete",US) -> {US,[?WML_ATTVAL_delete]};
encode_attribute_value("help",US) -> {US,[?WML_ATTVAL_help]};
encode_attribute_value("middle",US) -> {US,[?WML_ATTVAL_middle]};
encode_attribute_value("nowrap",US) -> {US,[?WML_ATTVAL_nowrap]};
encode_attribute_value("onenterbackward",US) ->
    {US,[?WML_ATTVAL_onenterbackward]};
encode_attribute_value("onenterforward",US) ->{US,[?WML_ATTVAL_onenterforward]};
encode_attribute_value("onpick",US) -> {US,[?WML_ATTVAL_onpick]};
encode_attribute_value("ontimer",US) -> {US,[?WML_ATTVAL_ontimer]};
encode_attribute_value("options",US) -> {US,[?WML_ATTVAL_options]};
encode_attribute_value("password",US) -> {US,[?WML_ATTVAL_password]};
encode_attribute_value("reset",US) -> {US,[?WML_ATTVAL_reset]};
encode_attribute_value("text",US) -> {US,[?WML_ATTVAL_text]};
encode_attribute_value("top",US) -> {US,[?WML_ATTVAL_top]};
encode_attribute_value("unknown",US) -> {US,[?WML_ATTVAL_unknown]};
encode_attribute_value("wrap",US) -> {US,[?WML_ATTVAL_wrap]};
encode_attribute_value("http://www."++V,US) ->
    {US2,EncV}=encode_attribute_value2(V,US,[]),
    {US2,[?WML_ATTVAL_http_www]++EncV};
encode_attribute_value("http://"++V,US) ->
    {US2,EncV}=encode_attribute_value2(V,US,[]),
    {US2,[?WML_ATTVAL_http]++EncV};
encode_attribute_value("https://www."++V,US) ->
    {US2,EncV}=encode_attribute_value2(V,US,[]),
    {US2,[?WML_ATTVAL_https_www]++EncV};
encode_attribute_value("https://"++V,US) ->
    {US2,EncV}=encode_attribute_value2(V,US,[]),
    {US2,[?WML_ATTVAL_https]++EncV};
encode_attribute_value(V,US) ->
    encode_attribute_value2(V,US,[]).

encode_attribute_value2([],US,Str) ->
    encode_text_with_entity(US,Str);
encode_attribute_value2("www."++V,US,Str) ->
    {US2,EncV1}=encode_text_with_entity(US,Str),
    {US3,EncV2}=encode_attribute_value2(V,US2,Str),
    {US3,EncV1++[?WML_ATTVAL_www]++EncV2};
encode_attribute_value2(".com/"++V,US,Str) ->
    {US2,EncV1}=encode_text_with_entity(US,Str),
    {US3,EncV2}=encode_attribute_value2(V,US2,Str),
    {US3,EncV1++[?WML_ATTVAL_com]++EncV2};
encode_attribute_value2(".edu/"++V,US,Str) ->
    {US2,EncV1}=encode_text_with_entity(US,Str),
    {US3,EncV2}=encode_attribute_value2(V,US2,Str),
    {US3,EncV1++[?WML_ATTVAL_edu]++EncV2};
encode_attribute_value2(".net/"++V,US,Str) ->
    {US2,EncV1}=encode_text_with_entity(US,Str),
    {US3,EncV2}=encode_attribute_value2(V,US2,Str),
    {US3,EncV1++[?WML_ATTVAL_net]++EncV2};
encode_attribute_value2(".org/"++V,US,Str) ->
    {US2,EncV1}=encode_text_with_entity(US,Str),
    {US3,EncV2}=encode_attribute_value2(V,US2,Str),
    {US3,EncV1++[?WML_ATTVAL_org]++EncV2};
encode_attribute_value2([H|V],US,Str) ->
    encode_attribute_value2(V,US,Str++[H]).



%%% FIXME! Does always assume 'noesc' variables, does not validate properly
encode_text_with_entity(US,[]) ->
    {US,[]};
encode_text_with_entity(US=#wbxml_info{cur_element=ECode,
                       cur_attribute=ACode},Text) ->
    case use_variable_scan(ECode,ACode) of
    false ->
        encode_text1(Text,[],[],US);
    true ->
        encode_text2(Text,[],[],US)
    end.

use_variable_scan(?WML_card,A)
  when A==?WML_title;
       A==?WML_onenterforward;A==?WML_onenterbackward;A==?WML_ontimer -> true;
use_variable_scan(?WML_do,?WML_label) -> true;
use_variable_scan(?WML_template,A)
  when A==?WML_onenterforward;A==?WML_onenterbackward;A==?WML_ontimer -> true;
use_variable_scan(?WML_go,?WML_href) -> true;
use_variable_scan(?WML_postfield,A)
  when A==?WML_name;A==?WML_value -> true;
use_variable_scan(?WML_setvar,A) when A==?WML_name;A==?WML_value -> true;
use_variable_scan(?WML_select,A)
  when A==?WML_title;A==?WML_value;A==?WML_ivalue -> true;
use_variable_scan(?WML_optgroup,?WML_title) -> true;
use_variable_scan(?WML_option,A)
  when A==?WML_title;A==?WML_value;A==?WML_onpick -> true;
use_variable_scan(?WML_input,A) when A==?WML_title;A==?WML_value -> true;
use_variable_scan(?WML_fieldset,?WML_title) -> true;
use_variable_scan(?WML_timer,?WML_value) -> true;
use_variable_scan(?WML_img,A)
  when A==?WML_alt;A==?WML_src;A==?WML_localsrc -> true;
use_variable_scan(?WML_anchor,?WML_title) -> true;
use_variable_scan(?WML_a,A) when A==?WML_href;A==?WML_title -> true;
use_variable_scan(?WML_table,?WML_title) -> true;
use_variable_scan(_,_) -> false.

    
%% Note:
%% - All whitespace has been normalised to a single space by xmerl!
%% - Text may include sublists, which indicates a numeric entity
encode_text1([],[],EncV,US) ->
    {US,EncV};
encode_text1([],Acc,EncV1,US) ->
    {US2,EncV2}=encode_string(US,lists:reverse(Acc)),
    {US2,EncV1++EncV2};
encode_text1([[Entity]|Rest],Acc,EncV,US) ->
    {US2,EncV1}=encode_string(US,lists:reverse(Acc)),
    encode_text1(Rest,[],EncV1++encode_numeric_entity(Entity,US2),US2);
encode_text1([H|T],Acc,EncV,US) ->
    encode_text1(T,[H|Acc],EncV,US).


encode_text2([],[],EncV,US) ->
    {US,EncV};
encode_text2([],Acc,EncV1,US) ->
    {US2,EncV2}=encode_string(US,lists:reverse(Acc)),
    {US2,EncV1++EncV2};
encode_text2([[Entity]|Rest],Acc,EncV,US) ->
    {US2,EncV1}=encode_string(US,lists:reverse(Acc)),
    encode_text2(Rest,[],EncV1++encode_numeric_entity(Entity,US2),US2);
encode_text2([$$,H|T],Acc,EncV1,US) ->
    if 
    H==$$ ->
        encode_text2(T,[$$,$$|Acc],EncV1,US);
    H==?SPACE;H==$( ->
        {Var,Esc,T1}=scan_variable(T,'noesc'),
        T3=case {H,T1} of
           {$(," )"++T2} ->
               T2;
           {$(,")"++T2} ->
               T2;
           {?SPACE,[]} ->
               [];
           {?SPACE,[?SPACE|T2]} ->
               T2
           end,
        {US2,EncV2}=encode_variable(US,Esc,Var),
        encode_text2(T3,Acc,EncV1++EncV2,US2);
    true ->
        {Var,Esc,T1}=scan_variable([H|T],'noesc'),
        {US2,EncV2}=encode_variable(US,Esc,Var),
        encode_text2(T1,Acc,EncV1++EncV2,US2)
    end;
encode_text2([H|T],Acc,EncV,US) ->
    encode_text2(T,[H|Acc],EncV,US).


encode_string(US=#wbxml_info{str_table=StrTbl,charset=Charset},Text) ->
    case erlang:length(Text) of
    V when 5<V,V<50 ->
        {StrTbl2,Index}=wbxml:add_string(StrTbl,Text),
        {US#wbxml_info{str_table=StrTbl2},
         [?WBXML_STR_T]++wap_common:pack_uintvar(Index)};
    _ ->
        Str=ucs:from_unicode(Text,Charset),
        {US, [?WBXML_STR_I]++Str++[0]}
    end.

scan_variable([H|T],DefEsc) ->
    case is_var_char1(H) of
    true ->
        {Var,Esc,T1}=scan_variable(T,DefEsc,[]),
        {[H|Var],Esc,T1};
    false ->
        ?error("Illegal variable ~p, with default escaping ~p",
           [T,DefEsc],scan_variable),
        throw({error,illegal_variable})
    end.


scan_variable([],DefEsc,Var) ->
    {lists:reverse(Var),DefEsc,[]};
scan_variable([H|T],DefEsc,Var) ->
    case is_var_charn(H) of
    true ->
        scan_variable(T,DefEsc,[H|Var]);
    false ->
        {lists:reverse(Var),DefEsc,[H|T]};
    conv ->
        scan_conv(T,[H|Var])
    end.

scan_conv("escape"++T,Var) ->
    {lists:reverse(Var),'escape',T};
scan_conv("noesc"++T,Var) ->
    {lists:reverse(Var),'noesc',T};
scan_conv("unesc"++T,Var) ->
    {lists:reverse(Var),'unesc',T};
scan_conv(T,Var) ->
    ?error("Illegal variable ~p, got ~p",[T,Var],scan_conv),
    throw({error,illegal_variable}).

is_var_char1(X) when X >= $a, X =< $z -> true;
is_var_char1(X) when X >= $A, X =< $Z -> true;
is_var_char1($_) -> true;
is_var_char1(_) -> false.

is_var_charn(X) when X >= $0, X =< $9 -> true;
is_var_charn(X) when X >= $0, X =< $9 -> true;
is_var_charn($:) -> conv;
is_var_charn(X) ->
    is_var_char1(X).
    
encode_variable(US,'escape',Var) ->
    encode_variable2(US,erlang:length(Var),Var,?WBXML_EXT_I_0,?WBXML_EXT_T_0);
encode_variable(US,'noesc',Var) ->
    encode_variable2(US,erlang:length(Var),Var,?WBXML_EXT_I_1,?WBXML_EXT_T_1);
encode_variable(US,'unesc',Var) ->
    encode_variable2(US,erlang:length(Var),Var,?WBXML_EXT_I_2,?WBXML_EXT_T_2).


encode_variable2(US=#wbxml_info{str_table=StrTbl},Len,Var,Ext_I,Ext_T)
  when 5<Len,Len<50 ->
    {StrTbl2,Index}=wbxml:add_string(StrTbl,Var),
    {US#wbxml_info{str_table=StrTbl2},[Ext_T]++wap_common:pack_uintvar(Index)};
encode_variable2(US=#wbxml_info{charset=Charset},LenVar,Var,Ext_I,Ext_T) ->
    Str=ucs:from_unicode(Var,Charset),
    {US, [Ext_I]++Str++[0]}.


encode_numeric_entity(Int,#wbxml_info{charset=Charset}) ->
    [?WBXML_ENTITY]++wap_common:pack_uintvar(Int).
%    case ucs:is_incharset(Int,Charset) of
%   false ->
%       [?WBXML_ENTITY]++wap_common:pack_uintvar(Int);
%   true ->
%       [Int]
%    end.

lookup_tag(Code) ->
    case lists:keysearch(Code,1,?WML_TAG_TOKENS) of
    {value,{Code,Val}} ->
        Val;
    _ ->
        throw({error,unknown_tag_code})
    end.

lookup_attribute(Code,First) ->
    case {First,lists:keysearch(Code,1,?WML_ATTRIBUTE_START_TOKENS_DEC)} of
    {true,{value,{Code,Val}}} ->
        {Val,false};
    {false,{value,{Code,Val}}} ->
        {"\""++Val,false};
    _ ->
        case lists:keysearch(Code,1,?WML_ATTRIBUTE_VALUE_TOKENS) of
        {value,{Code,Val}} ->
            {Val,false};
        _ ->
            throw({error,unknown_attribute_code})
        end
    end.

create_db() ->
    WmlTbl=ets:new(wmldb,[set, private]),
    init_db2(?WML_TAG_TOKENS,WmlTbl),
    init_db(?WML_ATTRIBUTE_START_TOKENS_DEC,WmlTbl),
    init_db(?WML_ATTRIBUTE_START_TOKENS_ENC,WmlTbl),
    init_db(?WML_ATTRIBUTE_VALUE_TOKENS,WmlTbl).


init_db([],_) ->
    ok;
init_db([{Code,Val}|TokenList],WmlTbl) ->
    ets:insert(WmlTbl,{Code,Val}),
    init_db(TokenList,WmlTbl).

init_db2([],_) ->
    ok;
init_db2([{Code,Val}|TokenList],WmlTbl) ->
    ets:insert(WmlTbl,{Code,Val}),
    ets:insert(WmlTbl,{Val,Code}),
    init_db2(TokenList,WmlTbl).


