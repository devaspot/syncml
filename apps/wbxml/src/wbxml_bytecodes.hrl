
%% Global Tokens
-define(WBXML_SWITCH_PAGE,      16#00).
-define(WBXML_END,              16#01).
-define(WBXML_ENTITY,           16#02).
-define(WBXML_STR_I,            16#03).
-define(WBXML_LITERAL,          16#04).
-define(WBXML_EXT_I_0,          16#40).
-define(WBXML_EXT_I_1,          16#41).
-define(WBXML_EXT_I_2,          16#42).
-define(WBXML_PI,               16#43).
-define(WBXML_LITERAL_C,        16#44).
-define(WBXML_EXT_T_0,          16#80).
-define(WBXML_EXT_T_1,          16#81).
-define(WBXML_EXT_T_2,          16#82).
-define(WBXML_STR_T,            16#83).
-define(WBXML_LITERAL_A,        16#84).
-define(WBXML_EXT_0,            16#c0).
-define(WBXML_EXT_1,            16#c1).
-define(WBXML_EXT_2,            16#c2).
-define(WBXML_OPAQUE,           16#c3).
-define(WBXML_LITERAL_AC,       16#c4).

%% Public Identifiers
-define(WBXML_string_table,     16#0).
-define(WBXML_unknown,          16#1).
-define(WBXML_wml_10,           16#2).
-define(WBXML_wta_event_10,     16#3). %% Deprecated
-define(WBXML_wml_11,           16#4).
-define(WBXML_si_10,            16#5).
-define(WBXML_sl_10,            16#6).
-define(WBXML_co_10,            16#7).
-define(WBXML_channel_11,       16#8).
-define(WBXML_wml_12,           16#9).
-define(WBXML_wml_13,           16#A).
-define(WBXML_provisioning_10,  16#B).
-define(WBXML_wta_wml_12,       16#C).
-define(WBXML_channel_12,       16#D).
-define(WBXML_syncml_11,        16#FD3).
-define(WBXML_devinf_11,        16#FD4).
-define(WBXML_syncml_12,        16#1201).
-define(WBXML_devinf_12,        16#1201).

%%% WBXML info for encoding call back module 
-record(wbxml_info,{
      str_table,    % (ets) The Global String table
      charset,      % (atom) Charset used to encode all text messages
      cur_element,  % (uint8) Encoding of current element
      cur_attribute % (uint8) Encoding of current attribute, start value
     }).

% Versions
-define(WBXML_VERSION1, 1).
-define(WBXML_VERSION2, 2).
-define(WBXML_VERSION3, 3).

-define(WBXML_ENTITIES,
    [
     {34,"&quot;"},
     {38,"&amp;"},
     {39,"&apos;"},
     {60,"&lt;"},
     {62,"&gt;"},
     {160,"&nbsp;"},
     {173,"&shy;"}
    ]).
