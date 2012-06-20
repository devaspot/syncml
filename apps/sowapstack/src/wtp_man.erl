%%% File    : wtp_man.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WTP manager process
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wtp_man).
-author('johblo@dragon.cellpt.se').
-behaviour(gen_server).
-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wtp_man.erl,v 1.2 2001/07/10 12:43:26 johblo Exp $ ').
-modified('$Date: 2001/07/10 12:43:26 $ ').
-modified_by('$Author: johblo $ ').
-vsn("1").

-export([start_link/1,stop/1,
	 active_transactions/1
	]).

%% Internal component interface
-export([toggle_tid/1,validate_tid/4,
	 invoke/3,result/3
	]).

%% Internal gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wtp.hrl").
-include("wtpif.hrl").
 
%%% ----------------------------------------------------------------------------
%%% Implementation notes:
%%% Notes on Tid handling:
%%% - The direction bit is always toggled when parsing (and thus _receiving_) a
%%%   message from WDP (or WTLS)
%%% - A Tid returned from an invoke, and perhaps used as a reference, has the
%%%   direction bit set.

-record(state,{tdb,        % (ets) Transaction database
	       wsp,        % (pid) to WSP component above
	       below       % (pid) to component below WTP (WDP or WTLS)
	      }).

%%% ----------------------------------------------------------------------------
%% Maintenance
start_link(Args)->
    gen_server:start_link(wtp_man,Args,?START_OPTIONS).

stop(Wtp) ->
    gen_server:call(Wtp,stop).

active_transactions(Wtp) ->
    gen_server:call(Wtp,active_transactions).
    

%% .............................................................................
init({Sref,Wsp}) ->
    Tdb=case wtp_db:start() of
	    {ok,TransDB} ->
		TransDB;
	    _ ->
		{error,cant_start_transactiondb}
	end,

    %% Initiate all responder and initiator processes
    MaxProc=10,
    start_responder(MaxProc,Tdb),
    start_initiator(MaxProc,Tdb),

    {ok,Wdp}=wdp_man:start_link({Sref,self()}),
    wap_stack_db:update_stack(Sref,{wdp,Wdp}),

    ?trace("Started WTP manager ok",[],init),
    {ok,#state{tdb=Tdb,wsp=Wsp,below=Wdp}}.

start_responder(0,_) ->
    ok;
start_responder(MaxResponder,Tdb) ->
    wtp_db:add_responder(Tdb,wtp_responder:start(Tdb)),
    start_responder(MaxResponder-1,Tdb).

start_initiator(0,_) ->
    ok;
start_initiator(MaxInitiator,Tdb) ->
    wtp_db:add_initiator(Tdb,wtp_initiator:start(Tdb)),
    start_initiator(MaxInitiator-1,Tdb).

terminate(Reason,State) ->
    wtp_db:stop(State#state.tdb),
    ?trace("WTP manager stopped:~p",[Reason],terminate).

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%%% ----------------------------------------------------------------------------
%%% Call back functions
%%% Management functions
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(active_transactions,_,State) ->
    wtp_db:active_transactions(State#state.tdb),
    {reply,ok,State};

%%% Requests from WSP
%% tr_invoke_req
handle_call({tr_invoke_req,Tpar,TCL,Uack,WSP,WSPdata},_,State) ->
    {TID,TIDNew}=give_next_tid(State#state.tdb,Tpar),
    Ans=if 
	    TCL==?CLASS0 ->
		invoke(#invoke_pdu{tid=TID,tidnew=TIDNew,uack=Uack,tcl=TCL,
				   data=WSPdata},
		       State#state.below,Tpar);
	    true ->
		state_null_initiator(Tpar,{TID,TIDNew,Uack,TCL},
				     WSP,WSPdata,State)
	end,
    {reply,Ans,State}.



%% On requests from WDP (with unitdata_ind) do the following:
%% 1. Find the right WTP process and entry from the database
%% 2. Do the initial checkings 
%% 3. Forward the request to the WTP process 
handle_cast({unitdata_ind,Tpar,BinPdu},State) ->
    case catch handle_unitdata_ind(BinPdu,Tpar,State) of
	ok ->
	    ok;
	Error -> % Failed parse unitdata_ind
	    ?error("Cannot parse ~p got ~p Unknown PDU so cannot Abort!!!",
		   [BinPdu,Error],handle_cast)
    end,
    {noreply,State};

handle_cast({error_ind,Tpar,Reason},State) ->
    ?error("Gor error_ind(~p,~p)",[Tpar,Reason],handle_cast),
    {noreply,State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}	      |
%%	    {noreply, State, Timeout} |
%%	    {stop, Reason, State}	     (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%% ============================================================================
%% Checks if an incoming Unitdata.ind has a legal type, parse it and and put
%% the result in a corresponding record. The data block may, however, be
%% segmented or concatenated. If so combine/split first before any further
%% action.
%% Returns: ok or {error,Reason}
handle_unitdata_ind(BinPdu,Tpar,State) ->
    case wtp_pdu:decode_pdu(BinPdu) of
	{concatenated,ConcCont} ->
	    handle_concatenated(ConcCont,Tpar,State);
	Pdu when is_record(Pdu,seginvoke_pdu);is_record(Pdu,segresult_pdu);
		 is_record(Pdu,nack_pdu) ->
	    handle_segmented(Pdu,Tpar,State);
	Pdu when is_record(Pdu,invoke_pdu);is_record(Pdu,result_pdu);
		 is_record(Pdu,ack_pdu);is_record(Pdu,abort_pdu) ->
	    handle_single(Pdu,Tpar,State)
    end.


%%% Parse incoming "concatenated" PDUs from WDP. Parsed packets might be 
%%% - complete single PDU (handle_single/3) or
%%% - parts of a segmented PDU (handle_segmented/3)
handle_concatenated([],_Tpar,_State) ->
    ok;
handle_concatenated([Pdu|Pdulist],Tpar,State) when is_record(Pdu,seginvoke_pdu);
						   is_record(Pdu,segresult_pdu);
						   is_record(Pdu,nack_pdu) ->
    case handle_segmented(Pdu,Tpar,State) of
	ok -> handle_concatenated(Pdulist,Tpar,State);
	{error,Reason} -> {error,Reason}
    end;
handle_concatenated([Pdu|Pdulist],Tpar,State) ->
    case handle_single(Pdu,Tpar,State) of
	ok -> handle_concatenated(Pdulist,Tpar,State);
	{error,Reason} -> {error,Reason}
    end.
    
%%% Handles incoming "single" PDUs (not concatenated or segmented) from WDP.
%%% Concatenated PDUs must first be deconcatenated (handle_concatenated/3) and
%%% segmented PDUs (seginvoke, segresult and nack PDUs) concatenated
%%% (handle_segmented/3)
%%% Note:
%%% - Abort reasons, Error TPIs and Option TPIs  may contain valuable info, to
%%%   be recorded for subsequent transactions to a specific DestAddr.
%%%   Record all such collected info in a database, e.g., emptied on a
%%%   disconnect of a WSP session. Note that Class 0 transactions will never
%%%   receive any information.
%%%   Collectable info includes:
%%%   + Abort Reasons: NOTIMPLEMENTEDSAR, NOTIMPLEMENTEDCL2, NOTIMPLEMENTEDUACK
%%%   + Option TPI: 
%%%   + Error TPI:
%%%
%%% Returns: ok or {error,Reason}
handle_single(Pdu=#invoke_pdu{gtr=?TRUE,ttr=?TRUE,tid=Tid,tcl=TCL,
			      version=Vers,data=Data},
	      Tpar,State) ->
    if
	?WTP_VERSION < Vers ->
	    abort(?WTPVERSIONZERO,Tid,State#state.below,Tpar),
	    throw({error,invalid_incoming_version});
	true -> 
	    ok
    end,
    case TCL of
	?CLASS0 -> % Not segmented, Class 0 transaction
	    invoke_ind(State#state.wsp,Tpar,Data),
	    ok;
	_ -> % Not segmented, Class 1 or 2 transaction
	    forward_single(Pdu,Tpar,State)
    end;
handle_single(Pdu,Tpar,State) when is_record(Pdu,invoke_pdu)->
    handle_segmented(Pdu,Tpar,State);
handle_single(Pdu=#result_pdu{gtr=?TRUE,ttr=?TRUE},Tpar,State) ->
    %% Not segmented, Class 1 or 2 transaction
    forward_single(Pdu,Tpar,State);
handle_single(Pdu,Tpar,State) when is_record(Pdu,result_pdu) ->
    handle_segmented(Pdu,Tpar,State);
handle_single(Pdu,Tpar,State) ->
    forward_single(Pdu,Tpar,State).


%%% FIXME!
%%% Combine incoming "segmenteded" PDUs from WDP and let handle_single/3 take
%%% care of each complete single PDU.
handle_segmented(#invoke_pdu{},_Tpar,_State) -> % Initial segment
    {error,not_implemented};
handle_segmented(#result_pdu{},_Tpar,_State) -> % Initial segment
    {error,not_implemented};
handle_segmented(#seginvoke_pdu{},_Tpar,_State) ->
    {error,not_implemented};
handle_segmented(#segresult_pdu{},_Tpar,_State) ->
    {error,not_implemented};
handle_segmented(#nack_pdu{},_Tpar,_State) ->
    {error,not_implemented}.



%%% Forward a complete, single WTP PDU to the initiator or responder process
forward_single(Pdu,Tpar,State) ->
    {ToggledTid,Trtype,_BareTid}=extract_tid_values(Pdu),
    case wtp_db:lookup_unidata_tid(State#state.tdb,{Tpar,Trtype,ToggledTid}) of
	{ok,WTP,Transtype,TidNewSet} ->
	    case Transtype of
		responder ->
		    wtp_responder:request(WTP,Pdu),
		    update_last_tid(Pdu,TidNewSet,State#state.tdb,Tpar);
		initiator ->
		    wtp_initiator:request(WTP,Pdu),
		    update_last_failed(Pdu,TidNewSet,State#state.tdb,Tpar)
	    end;
	{error,no_transaction} ->
	    handle_new_transaction(Pdu,Trtype,Tpar,State)
    end.

extract_tid_values(Pdu) ->
    ToggledTid=get_tid(Pdu),
    {Trtype,BareTid}=extract_tid(ToggledTid),
    {ToggledTid,Trtype,BareTid}.

%%% Handles an incoming PDU packet from WDP, when this transaction cannot be
%%% found in the current transaction database.
%%% All #invoke_pdu{} are Class 1 or Class 2 transactions, as sorted out in
%%% handle_single/3.
%%% Returns: ok or throw({error,Reason})
handle_new_transaction(Pdu,Trtype,Tpar,State) ->
    case Pdu of
	#invoke_pdu{} ->
	    case get_idle_responder(Tpar,State) of
		{ok,WTPres} -> % (1) Create new transaction
		    wtp_responder:first_rcv_invoke(WTPres,Tpar,Pdu,
						   State#state.wsp,
						   State#state.below),
		    ok;
		{error,_Reason} -> % (10) Ignore
		    ?debug("Cannot find idle responder ==> ignoring",[],
			   handle_new_transaction),
		    ok
	    end;
	#ack_pdu{tidver=?TRUE,tid=Tid} when Trtype==initiator -> % (2) Ignore
	    abort(?INVALIDTID,Tid,State#state.below,Tpar),
	    ok;
	Pdu when is_record(Pdu,ack_pdu);is_record(Pdu,nack_pdu);
		 is_record(Pdu,result_pdu);is_record(Pdu,abort_pdu) -> % (3) Ignore
	    ok
    end.


%% Extracts the Tid field and toggle the direction bit
get_tid(#invoke_pdu{tid=Tid})-> toggle_tid(Tid);
get_tid(#result_pdu{tid=Tid})-> toggle_tid(Tid);
get_tid(#abort_pdu{tid=Tid})-> toggle_tid(Tid);
get_tid(#ack_pdu{tid=Tid})-> toggle_tid(Tid).

%-------------------------------------------------------------------------------
%% Find an idle responder process, if possible.
%% (and initialise with parameters for this particular transaction.)
%% Returns {ok,ResponderPid} or {error,no_idle_responder}
get_idle_responder(_Tpar,State) ->
    wtp_db:get_idle_responder(State#state.tdb).


%% Find an idle initiator process, if possible.
%% (and initialise with parameters for this particular transaction.)
%% If no processes are available return, {error,no_idle_responder}
get_idle_initiator(_Tpar,State) ->
    case wtp_db:get_idle_initiator(State#state.tdb) of
	{ok,RespPid} ->
	    RespPid;
	{error,Reason} ->
	    throw({error,Reason})
    end.

%-------------------------------------------------------------------------------
%% Send data to WDP/WTLS.
%% FIXME! Segmentation still not supported, i.e. always GTR=1,TTR=1
invoke(Pdu,Below,Tpar) ->
    case catch wtp_pdu:encode_pdu(Pdu) of
	BinPdu when is_binary(BinPdu) ->
	    send_segments(Below,Tpar,BinPdu);
	Error ->
	    ?error("Cannot encode Pdu ~p got ~p",[Pdu,Error],invoke),
	    {error,invalid_command}
    end.

result(Pdu,Below,Tpar) ->
    case catch wtp_pdu:encode_pdu(Pdu) of
	BinPdu when is_binary(BinPdu) ->
	    send_segments(Below,Tpar,BinPdu);
	Error ->
	    ?error("Cannot encode Pdu ~p got ~p",[Pdu,Error],result),
	    {error,invalid_command}
    end.

send_segments(Wdp,Tpar,Tpdu) ->
    wdp_man:unitdata_req(Wdp,Tpar,Tpdu).%;
%send_segments(Wdp,Tpar,Tpdu) ->
%    {error,segmentation_not_implemented}.

%reassemble() ->
%    {error,not_implemented}.


abort(AbortReason,Tid,Below,Tpar) ->
    OutTid=toggle_tid(Tid),
    Data=wtp_pdu:encode_pdu(#abort_pdu{type=?PROVIDER,reason=AbortReason,
				       tid=OutTid,tpilist=[]}),
    wdp_man:unitdata_req(Below,Tpar,Data).


%% =============================================================================
%% Send respons back to the application (WSP)

%% TR-Abort.ind
%tr_abort_ind(AbortFrom,AbortReason,State) ->
%    Reason=if
%	       AbortFrom==?USER ->    {wsp,AbortReason};
%	       AbortFrom==?PROVIDER ->{wtp,AbortReason}
%	   end,
%    abort_ind(State#state.wsp,self(),Reason).

%% -----------------------------------------------------------------------------
%% As this is only called at the initial try we know that RID=false, RCR=0
%% Note:
%% - Class 0 transactions and addition are handled separately
%% - The action is independent of Uack
%% - A Tid with the direction bit set is returned. This is because in all
%%   subsequent refernces this transaction this will be the case (tid toggled
%%   in handle_unitdata_ind() )
%% - No additional TPI, ie TpiList always empty
%% - Incoming UserAck already on the right format, so just reuse to Uack
%% TBD: There are probably cases when the invoke *not* should be sent.
state_null_initiator(Tpar,Content,WSP,WSPdata,State) ->
    WTPini=get_idle_initiator(Tpar,State),
    wtp_initiator:tr_invoke_req(WTPini,Tpar,Content,WSP,WSPdata,
				 State#state.below),
    {ok,WTPini}.


%% =============================================================================
%% Transforms to atoms with names as in the state tables in the standard.
%to_abstr(?Invoke) -> rcv_invoke;
%to_abstr(?Ack) ->    rcv_ack;
%to_abstr(?Abort) ->  rcv_abort;
%to_abstr(?Result) -> rcv_result;
%to_abstr(X) -> X. % For any other event, just keep the given integer


%% =============================================================================
%% Tid verification on incoming invoke_ind.
%% Returns a a tuple {Flag,State} where Flag is true if the Tid is valid 
validate_tid(TID,TidNew,Tpar,Tdb) ->
    BareTid=TID band 32767,
    LastTid=wtp_db:get_last_tid(Tdb,Tpar),
    if
	TidNew==?FALSE ->
	    case tid_test(BareTid,LastTid) of
		true ->
		    ok=wtp_db:set_last_tid(Tdb,Tpar,BareTid),
		    true;
		_ ->
		    false
	    end;
	TidNew==?TRUE ->
	    case tid_test(BareTid,LastTid) of
		true ->
		    ?trace("TIDVER:update last_tid:~p",[BareTid],validate_tid),
		    ok=wtp_db:set_last_tid(Tdb,Tpar,BareTid),
		    true;
		_ ->
		    false
	    end;
	true ->
	    false
    end.

-define(WindowSize,16383). % Half the Tid space, ie 2^14-1

%% Tests if an incoming Tid is ok
%% Tids must be inside the window of size WindowSize
%% RcvTid - The newly receive Tid
%% LastTid - Biggest, previously accepted, Tid received 
tid_test(RcvTid,LastTid) ->
    ?trace("TIDVER:invoke_ind RcvTid=~p,LastTid=~p",[RcvTid,LastTid],tid_test),
    if
	LastTid==undefined -> true;
	RcvTid==LastTid ->    false;
	RcvTid>LastTid ->
	    if
		(RcvTid-LastTid)<?WindowSize -> true;
		true -> false
	    end;
	RcvTid<LastTid ->
	    if
		(LastTid-RcvTid)<?WindowSize -> true;
		true -> false
	    end;
	true ->
	    false
    end.	     



%% Transaction Id handling, as described in the Editorial Change Request,
%% 9-July-1998 by Patrik Gustafsson, Unwired Planet
%% Note:
%% -RegisterActiveTid()/DeRegisterActiveTid()
%%   A transaction is registered as active if stored with
%%   wtp_db:add_transaction()
%% -TidActive()
%%   The test if a transaction is active or not is done by
%%   wtp_db:lookup_tid(), if found the it is active.

%% Initiator side
%% Generates a new Transaction Id. If the responder has asked for Tid
%% verification since the last tid was given, set the Tidnew flag.
give_next_tid(Tdb,Tpar) ->
    Tid=wtp_db:get_gentid(Tdb,Tpar),
    TidNew=case wtp_db:get_last_failed(Tdb,Tpar) of
	       undefined -> ?TRUE; % Always sets TidNew on startup
	       false -> ?FALSE;
	       true -> ?TRUE
	   end,
    NextTid=if
		Tid<32768 -> Tid+1;
		true -> 0
	    end,
    wtp_db:set_gentid(Tdb,Tpar,NextTid),
    {Tid,TidNew}.

%% .............................................................................
%% Initiator might need to update last_failed if an Ack is received 
update_last_failed(#ack_pdu{tidver=?TRUE},TidNewSet,Tdb,Tpar) ->
    if
	TidNewSet==?TRUE ->
	    wtp_db:set_last_failed(Tdb,Tpar,false);
	true ->
	    wtp_db:set_last_failed(Tdb,Tpar,true)
    end;
update_last_failed(_,_,_,_) ->
    ok.


%% Responder might need to update last_tid if an Ack is received
update_last_tid(#ack_pdu{tid=Tid,tidver=?TRUE},?TRUE,Tdb,Tpar) ->
    BareTid=Tid band 32767,
    ?trace("TIDVER: updating last_tid:~p",[BareTid],update_last_tid),
    wtp_db:set_last_tid(Tdb,Tpar,BareTid);
update_last_tid(_,_,_,_) ->
    ok.


%% -----------------------------------------------------------------------------
%% Filters outgoing Tids by toggle the direction bit to the
%% high order bit
toggle_tid(RealTid) ->
    RealTid bxor 32768.


%% Filters incoming Tids by extract bit 16 that decides if this transaction is
%% initiated or responded from the lower 15 bits thath decides the Tid
extract_tid(RealTid) ->
    Tid=RealTid band 32767,
    Trtype=case (RealTid band 32768) of
	       0 -> initiator;
	       _ -> responder
	   end,
    {Trtype,Tid}.

