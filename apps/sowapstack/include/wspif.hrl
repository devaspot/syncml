%%% File    : wspif.hrl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Generic interface towards WSP
%%% Created : 29 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wspif.hrl,v 1.1.1.1 2001/07/04 14:51:16 uid56739 Exp $ ').
 
%% Component external interface
-export([connect_req/4,suspend_req/1,resume_req/1,resume_req/2,
	 disconnect_req/1,disconnect_req/2,
         method_invoke_req/3,method_result_req/2,method_abort_req/1,
         push_req/2,confirmed_push_req/2,push_abort_req/1,
         unit_method_invoke_req/5,unit_method_result_req/3,unit_push_req/4
]).
-export([unit_method_invoke_ind/5,unit_method_result_ind/3, unit_push_ind/3, connect_ind/5,
    disconnect_ind/3, suspend_ind/3, resume_ind/2, method_result_ind/3, push_ind/2, confirmed_push_ind/3,
    push_abort_ind/3, connect_cnf/4, resume_cnf/2, method_invoke_cnf/2, confirmed_push_cnf/3]).

-export([method_invoke_ind/4, method_abort_ind/3, method_result_cnf/3]).

-export([
	 connect_res/3,resume_res/1,
	 method_invoke_res/1,method_result_res/2,confirmed_push_res/2
	]).



%% ----------- Downwards (for WAP Application to use) -----------
connect_req(Wsp,Tpar,Headers,Cap) ->
    ?debug("Tpar:~w",[Tpar],connect_req),
    gen_server:call(Wsp, {connect_req,Tpar,Headers,Cap}).
suspend_req(WSPses) ->
    ?debug("WSPses:~w",[WSPses],suspend_req),
    gen_fsm:sync_send_event(WSPses, suspend_req).
resume_req(WSPses) ->
    ?debug("WSPses:~w",[WSPses],resume_req),
    gen_fsm:sync_send_event(WSPses, resume_req).
resume_req(WSPses,Tpar) ->
    ?debug("WSPses:~w",[WSPses],resume_req),
    gen_fsm:sync_send_event(WSPses, {resume_req,Tpar}).
disconnect_req(WSPses) ->
    ?debug("WSPses:~w",[WSPses],disconnect_req),
    gen_fsm:sync_send_event(WSPses, disconnect_req).
disconnect_req(WSPses,Reason) ->
    ?debug("WSPses:~w Reason:~w",[WSPses,Reason],disconnect_req),
    gen_fsm:sync_send_event(WSPses, {disconnect_req,Reason}).


method_invoke_req(WSPses,HTTPReq,HTTPCont) ->
    ?debug("WSPses:~w",[WSPses],method_invoke_req),
    gen_fsm:sync_send_event(WSPses, {method_invoke_req,HTTPReq,HTTPCont}).
method_result_req(WSPmet,HTTPCont) ->
    ?debug("WSPmet:~w",[WSPmet],method_result_req),
    gen_fsm:sync_send_event(WSPmet, {method_result_req,HTTPCont}).
method_abort_req(WSPmet) ->
    ?debug("WSPmet:~w",[WSPmet],method_abort_req),
    gen_fsm:sync_send_event(WSPmet, method_abort_req).
unit_method_invoke_req(WSP,Tpar,Tid,Type,Content) ->
    ?debug("Tid:~w",[Tid],unit_method_invoke_req),
    gen_server:call(WSP, {unit_method_invoke_req,Tpar,Tid,Type,Content}).
unit_method_result_req(WSP,URef,HTTPCont) ->
    ?debug("URef:~w",[URef],unit_method_result_req),
    gen_server:call(WSP,{unit_method_result_req,URef,HTTPCont}).


push_req(WSPses,HTTPCont) ->
    ?debug("WSPses:~w",[WSPses],push_req),
    gen_fsm:sync_send_event(WSPses, {push_req,HTTPCont}).
confirmed_push_req(WSPses,HTTPCont) ->
    ?debug("WSPses:~w",[WSPses],confirmed_push_req),
    gen_fsm:sync_send_event(WSPses, {confirmed_push_req,HTTPCont}).
push_abort_req(WSPpus) ->
    ?debug("WSPpus:~w",[WSPpus],push_abort_req),
    gen_fsm:sync_send_event(WSPpus, push_abort_req).
unit_push_req(WSP,Tpar,Tid,Content) ->
    ?debug("Tid:~w",[Tid],unit_push_req),
    gen_server:call(WSP, {unit_push_req,Tpar,Tid,Content}).


connect_res(WSPses,Headers,Capabilities) ->
    ?debug("WSPses:~w",[WSPses],connect_res),
    gen_fsm:sync_send_event(WSPses,{connect_res,{Headers,Capabilities}}).
resume_res(WSPses) ->
    ?debug("WSPses:~w",[WSPses],resume_res),
    gen_fsm:sync_send_event(WSPses, resume_res).

method_invoke_res(WSPmet) ->
    ?debug("WSPmet:~w",[WSPmet],method_invoke_res),
    gen_fsm:sync_send_event(WSPmet, method_invoke_res).
method_result_res(WSPmet,AckHeaders) ->
    ?debug("WSPmet:~w",[WSPmet],method_result_res),
    gen_fsm:sync_send_event(WSPmet, {method_result_res,AckHeaders}).

confirmed_push_res(WSPpus,AckHeaders) ->
    ?debug("WSPpus:~w",[WSPpus],confirmed_push_res),
    gen_fsm:sync_send_event(WSPpus, {confirmed_push_res,AckHeaders}).


%% ----------- Upwards (for WSP to use) -----------
unit_method_invoke_ind(App,StRef,URef,HTTPReq,HTTPCont) ->
    ?debug("URef:~w",[URef],unit_method_invoke_ind),
    gen_server:cast(App,{unit_method_invoke_ind,StRef,URef,HTTPReq,HTTPCont}).
unit_method_result_ind(App,Tid,HTTPCont) ->
    ?debug("Tid:~w",[Tid],unit_method_result_ind),
    gen_server:cast(App,{unit_method_result_ind,Tid,HTTPCont}).
unit_push_ind(App,Tpar,HTTPCont) ->
    ?debug("",[],unit_push_ind),
    gen_server:cast(App,{unit_push_ind,Tpar,HTTPCont}).

connect_ind(App,WSPses,Vers,Headlist,SerCap) ->
    ?debug("WSPses:~w",[WSPses],connect_ind),
    gen_server:cast(App,{connect_ind,WSPses,Vers,Headlist,SerCap}).
disconnect_ind(App,WSPses,Reason) ->
    ?debug("WSPses:~w",[WSPses],disconnect_ind),
    gen_server:cast(App,{disconnect_ind,WSPses,Reason}).
suspend_ind(App,WSPses,Reason) ->
    ?debug("WSPses:~w",[WSPses],suspend_ind),
    gen_server:cast(App,{suspend_ind,WSPses,Reason}).
resume_ind(App,WSPses) ->
    ?debug("WSPses:~w",[WSPses],resume_ind),
    gen_server:cast(App,{resume_ind,WSPses}).

method_invoke_ind(App,WSPmet,HTTPReq,HTTPCont) ->
    ?debug("WSPmet:~w",[WSPmet],method_invoke_ind),
    gen_server:cast(App,{method_invoke_ind,WSPmet,HTTPReq,HTTPCont}).
method_result_ind(App,WSPmet,HTTPCont) ->
    ?debug("WSPmet:~w",[WSPmet],method_result_ind),
    gen_server:cast(App,{method_result_ind,WSPmet,HTTPCont}).
method_abort_ind(App,WSPmet,Reason) ->
    ?debug("WSPmet:~w",[WSPmet],method_abort_ind),
    gen_server:cast(App,{method_abort_ind,WSPmet,Reason}).

push_ind(App,HTTPCont) ->
    ?debug("",[],push_ind),
    gen_server:cast(App,{push_ind,HTTPCont}).
confirmed_push_ind(App,WSPpus,HTTPCont) ->
    ?debug("WSPpus:~w",[WSPpus],confirmed_push_ind),
    gen_server:cast(App,{confirmed_push_ind,WSPpus,HTTPCont}).
push_abort_ind(App,WSPpus,Reason)->
    ?debug("WSPpus:~w",[WSPpus],push_abort_ind),
    gen_server:cast(App,{push_abort_ind,WSPpus,Reason}).


connect_cnf(App,WSPses,Headlist,Cap) ->
    ?debug("WSPses:~w",[WSPses],connect_cnf),
    gen_server:cast(App,{connect_cnf,WSPses,Headlist,Cap}).
resume_cnf(App,WSPses) ->
    ?debug("WSPses:~w",[WSPses],resume_cnf),
    gen_server:cast(App,{resume_cnf,WSPses}).

method_invoke_cnf(App,WSPmet) ->
    ?debug("WSPmet:~w",[WSPmet],method_invoke_cnf),
    gen_server:cast(App,{method_invoke_cnf,WSPmet}).
method_result_cnf(App,WSPmet,AckHeaders) ->
    ?debug("WSPmet:~w",[WSPmet],method_result_cnf),
    gen_server:cast(App,{method_result_cnf,WSPmet,AckHeaders}).

confirmed_push_cnf(App,WSPpus,AckHeaders)->
    ?debug("WSPpus:~w",[WSPpus],confirmed_push_cnf),
    gen_server:cast(App,{confirmed_push_cnf,WSPpus,AckHeaders}).
