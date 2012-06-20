%%% File    : wtpif.hrl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Generic interface towards WTP
%%% Created : 29 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wtpif.hrl,v 1.1.1.1 2001/07/04 14:51:16 uid56739 Exp $ ').

-export([invoke_req/6,result_req/2,abort_req/2,
	 invoke_res/3,result_res/2,
	 exception/2]).

-export([invoke_ind/3, result_ind/3, invoke_cnf/3]).

-export([invoke_ind/6, abort_ind/3, result_cnf/3]).

%% Downwards (for WSP to use)
invoke_req(WTPman,App,Tpar,Tcl,UAck,Data) ->
    ?debug("WTPini:~w",[WTPman],invoke_req),
    gen_server:call(WTPman,{tr_invoke_req,Tpar,Tcl,UAck,App,Data}).
result_req(WTPres,Data) ->
    ?debug("WTPres:~w",[WTPres],result_req),
    gen_fsm:send_event(WTPres,{tr_result_req,Data}).
abort_req(WTP,Reason) -> % Only abort class 1 or 2 transactions
    ?debug("WTP:~w",[WTP],abort_req),
    gen_fsm:send_event(WTP,{tr_abort_req,Reason}).

invoke_res(WTPres,WSP,ExitInfo) ->
    ?debug("WTPres:~w",[WTPres],invoke_res),
    gen_fsm:send_event(WTPres,{tr_invoke_res,WSP,ExitInfo}).
result_res(WTPini,ExitInfo) ->
    ?debug("WTPini:~w",[WTPini],result_res),
    gen_fsm:send_event(WTPini,{tr_result_res,ExitInfo}).
    

%% Upwards (for WTP to use)
invoke_ind(App,Tpar,Data) -> % Class 0 transactions
    ?debug("class 0",[],invoke_ind),
    gen_server:cast(App,{tr_invoke_ind,undefined,Tpar,?CLASS0,undefined,Data}).
invoke_ind(App,WTPres,Tpar,Class,UAck,Data) -> % Class 1 and 2 transactions
    ?debug("WTPres:~w",[WTPres],invoke_ind),
    gen_server:cast(App,{tr_invoke_ind,WTPres,Tpar,Class,UAck,Data}).

result_ind(App,WTPini,Data) ->
    ?debug("WTPini:~w",[WTPini],result_ind),
    gen_fsm:send_event(App, {tr_result_ind,WTPini,Data}).
abort_ind(App,WTP,Info) ->
    ?debug("WTP:~w",[WTP],abort_ind),
    gen_fsm:send_event(App,{tr_abort_ind,WTP,Info}).

invoke_cnf(App,WTPini,Exitinfo) ->
    ?debug("WTPini:~w",[WTPini],invoke_cnf),
    gen_fsm:send_event(App,{tr_invoke_cnf,WTPini,Exitinfo}).
result_cnf(App,WTPres,Exitinfo) ->
    ?debug("WTPres:~w",[WTPres],result_cnf),
    gen_fsm:send_event(App,{tr_result_cnf,WTPres,Exitinfo}).


exception(App,Reason) ->
    gen_server:cast(App, {exception,Reason}).

