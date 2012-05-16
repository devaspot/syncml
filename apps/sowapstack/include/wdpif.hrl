%%% File    : wdpif.hrl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Generic interface towards WDP
%%% Created : 29 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wdpif.hrl,v 1.1.1.1 2001/07/04 14:51:16 uid56739 Exp $ ').

%% Component external interface
-export([unitdata_req/3,
         unitdata_ind/3,
         error_ind/3]).

%% Downwards (for WSP/WTP/WTLS etc. to use)
unitdata_req(Wdp,Tpar,Data) ->
    gen_server:cast(Wdp,{unitdata_req,Tpar,Data}).


%% Upwards (for WDP to use)
unitdata_ind(App,Tpar,Data) ->
    ?trace("~w",[Data],unitdata_ind),
%%unitdata_ind(App,{{SA,SP,DA,DP},B},Data) ->
%%    Tpar1={{{127,0,0,1},SP,DA,DP},B},
    gen_server:cast(App, {unitdata_ind,Tpar,Data}).
error_ind(App,Tpar,Reason) ->
    gen_server:cast(App, {error_ind,Tpar,Reason}).
