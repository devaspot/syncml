%%% File    : wap_common.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Miscellaneous code for the WAP stack and WAP wae 
%%% Created : 15 Apr 2001 by Johan Blom <johblo@dragon.cellpt.se>

-module(wap_common).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wap_common.erl,v 1.1.1.1 2001/07/04 14:51:16 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:16 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([pack_uint32/1,unpack_uint32/1,
	 pack_uint16/1,unpack_uint16/1,
	 pack_uint8/1,unpack_uint8/1,
	 pack_uintvar/1,unpack_uintvar/1,
	 unpack_data/1,unpack_data/2
	]).


% ------------------------------------------------------------------------------
%% Pack a number into upto 5 bytes, with 7 bits each.
pack_uintvar(Num) when Num<128->
    [Num];
pack_uintvar(Num) ->
    pack_uintvar(Num,[],1).
pack_uintvar(Num,_,6) ->
    throw({error,{too_big_num,Num}});
pack_uintvar(0,Out,_) ->
    Out;
pack_uintvar(Num,Out,S) ->
    Rest=Num div 128,
    Byte=if
	     S==1 ->
		 Num rem 128;
	     true ->
		 (Num rem 128) bor 128
	 end,
    pack_uintvar(Rest,[Byte]++Out,S+1).


unpack_uintvar(Content) ->
    unpack_uintvar(Content,0,1).

unpack_uintvar(_,Num,6) ->
    throw({error,{too_big_num,Num}});
unpack_uintvar([Byte|In],Out,S) ->
    if
	Byte>=128 ->
	    Num=Byte band 127,
	    unpack_uintvar(In,128*(Num+Out),S+1);
	true ->
	    {In,Byte+Out}
    end.

%% .............................................................................
% Pack integer on 1 bytes
pack_uint8(Num) ->
    pack_num(1,Num,[]).

% Pack integer on 2 bytes
pack_uint16	(Num) ->
    pack_num(2,Num,[]).

% Pack integer on 4 bytes
pack_uint32(Num) ->
    pack_num(4,Num,[]).

pack_num(0,_,List) -> List;
pack_num(Num,Sum,List) ->
    Byte=Sum rem 256,
    pack_num(Num-1,Sum div 256,[Byte|List]).

% Unpack integer on 1 bytes
unpack_uint8(Content) ->
    Num=unpack_num(1,0,Content),
    {tl(Content),Num}.

% Unpack integer on 2 bytes
unpack_uint16(Content) ->
    Num=unpack_num(2,0,Content),
    {lists:nthtail(2,Content),Num}.

% Unpack integer on 4 bytes
unpack_uint32(Content) ->
    Num=unpack_num(4,0,Content),
    {lists:nthtail(4,Content),Num}.

unpack_num(0,Sum,_) ->Sum;
unpack_num(Num,Sum,Content) ->
    unpack_num(Num-1,Sum*256+hd(Content),tl(Content)).

% ------------------------------------------------------------------------------
unpack_data(Content) ->
    {[],Content}.
unpack_data(Content,Len) when Len==0 ->
    {Content,[]};
unpack_data(Content,Len) ->
    {lists:nthtail(Len,Content),string:sub_string(Content,1,Len)}.


