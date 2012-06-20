%%% File    : wtp_common.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Common WTP functions
%%% Created :  9 May 2001 by Johan Blom <johblo@dragon.cellpt.se>

-module(wtp_common).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wtp_common.erl,v 1.1.1.1 2001/07/04 14:51:10 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:10 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([
%%% Timer handling
	 start_ack_timer/3,start_retry_timer/3,start_wait_timer/3,
	 stop_timer/1,
%%% Retransmission
	 max_retransmissions/1,max_ack_expirations/1,
%%% Queue handling
	 in_queue/5,out_queue/1
	]).

-include("wtp.hrl").


%% =============================================================================
%% Timer handling.
%%% FIXME: - Uses a Default value for all bearer types. Should I abort instead?
%%%        - Not using TimerVal at all for the moment
start_retry_timer(Bearer,{_TimerVal,_UAck},Pid) ->
    
    Timeout=1000*get_timerval(Bearer,
			      wap_stack_db:lookup_config(wtp_retry_timer)),
    case timer:apply_after(Timeout,gen_fsm,send_event,[Pid,timerTO_R]) of
	{ok,Ref} ->
	     Ref;
	{error,Reasons} ->
	    throw({error,Reasons})
    end.

start_ack_timer(Bearer,{_TimerVal,_UAck},Pid) ->
    Timeout=1000*get_timerval(Bearer,wap_stack_db:lookup_config(wtp_ack_timer)),
    case timer:apply_after(Timeout,gen_fsm,send_event,[Pid,timerTO_A]) of
	{ok,Ref} ->
	     Ref;
	{error,Reasons} ->
	    throw({error,Reasons})
    end.

start_wait_timer(Bearer,{_TimerVal,_UAck},Pid) ->
    Timeout=1000*get_timerval(Bearer,
			      wap_stack_db:lookup_config(wtp_wait_timer)),
    case timer:apply_after(Timeout,gen_fsm,send_event,[Pid,timerTO_W]) of
	{ok,Ref} ->
	     Ref;
	{error,Reasons} ->
	    throw({error,Reasons})
    end.

get_timerval(BearerType,{ok,Timerlist}) ->
    case lists:keysearch(BearerType,1,Timerlist) of
	{value,{_,TimerVal}} ->
	    TimerVal;
	_ ->
	    10
    end.


stop_timer(Timer) ->
    timer:cancel(Timer).


%%% ============================================================================
%%% FIXME: - Uses a Default value for all bearer types. Should I abort instead?
max_retransmissions(BearerType) ->
    {ok,List}=wap_stack_db:lookup_config(wtp_max_retrans),
    case lists:keysearch(BearerType,1,List) of
	{value,{_,Val}} ->
	    Val;
	_ ->
	    10
    end.

%%% FIXME: - Uses a Default value for all bearer types. Should I abort instead?
max_ack_expirations(BearerType) ->
    {ok,List}=wap_stack_db:lookup_config(wtp_max_ackexp),
    case lists:keysearch(BearerType,1,List) of
	{value,{_,Val}} ->
	    Val;
	_ ->
	    10
    end.


%%% ============================================================================
%% FIXME! Queue handling, for now, just send it right away and don't wait
%% How?
%% - Cache incoming requests and send only when buffer is full, on timeout or
%%   on request (out_queue)
in_queue(Module,Wdp,Tpar,_Type,Data) ->
    Module:unitdata_req(Wdp,Tpar,Data).
out_queue(_Tpar) ->
    ok.

