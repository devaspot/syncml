%% Submitted to Erlang Mailinglist
%%% Purpose : Encode byte streams in the base 64 encoding as defined by the
%%%         : MIME specification rfc1521. 
%%%         : (http://www.cis.ohio-state.edu/htbin/rfc/rfc1521.html)
%%% TODO: Check in RFC if the ending \n on a line shorter than 76 chars is ok

-module(base64encoder).
-export([pack/1, unpack/1]).

-define(MAX_LINE, 75).

pack(List) when is_list(List) ->
    pack(list_to_binary(List));
pack(Bin) when is_binary(Bin) ->
    acc_pack(Bin, []).

acc_pack(<<Bin:?MAX_LINE/binary, T/binary>>, Acc) ->
    acc_pack(T, [Acc,enc(Bin),$\n]);
acc_pack(Bin, Acc) ->
    list_to_binary([Acc,enc(Bin),$\n]).

unpack(List) when is_list(List) ->
    binary_to_list(list_to_binary(dec(list_to_binary(List))));
unpack(Bin) when is_binary(Bin) ->
    list_to_binary(dec(Bin)).

%% Base-64 encoding: take 6 bits at a time from the head of the binary
%% and emit it as 8 bit characters.
enc(Bin) ->
    list_to_binary(enc1(Bin)).

enc1(<<A:6, B:6, C:6, D:6, T/binary>>) ->
    AA = int_to_b64(A),
    BB = int_to_b64(B),
    CC = int_to_b64(C),
    DD = int_to_b64(D),
    [AA, BB, CC, DD| enc1(T)];

enc1(<<A:6, B:6, C:4>>) -> 
    AA = int_to_b64(A),
    BB = int_to_b64(B),
    CC = int_to_b64(C bsl 2),
    [AA, BB, CC, $=];

enc1(<<A:6, B:2>>) -> 
    AA = int_to_b64(A),
    BB = int_to_b64(B bsl 4),
    [AA, BB, $=, $=];

enc1(<<>>) -> [].

%% Base-64 decoding: Works by consuming groups of 4 input characters to create
%% a group of 3 output characters, with the three special-cases for
%% end-of-input first:

dec(Bin) ->
    dec(512, Bin, [], []).

dec(0, Bin, List, Acc) ->
    dec(512, Bin, List, [list_to_binary(Acc)]);
dec(_N, <<>>, [], Acc) -> Acc;
dec(_N, <<>>, [R,Q,P], Acc) -> [Acc|<<P:6, Q:6, (R bsr 2):4>>];
dec(_N, <<>>, [Q,P], Acc) -> [Acc|<<P:6, (Q bsr 4):2>>];
dec(N, Bin, [S,R,Q,P], Acc) ->
    dec(N-1, Bin, [], [Acc|<<P:6, Q:6, R:6, S:6>>]);
dec(N, <<A:8, T/binary>>, List, Acc) ->
    case b64_to_int(A) of
        ignore -> dec(N, T, List, Acc);
        Sixbits -> dec(N, T, [Sixbits|List], Acc)
    end.

b64_to_int(X) when X >= $A, X =< $Z -> X - $A;
b64_to_int(X) when X >= $a, X =< $z -> X - $a + 26;
b64_to_int(X) when X >= $0, X =< $9 -> X - $0 + 52;
b64_to_int($+) -> 62;
b64_to_int($/) -> 63;
b64_to_int(_) -> ignore.

int_to_b64(X) when X >= 0, X =< 25 -> X + $A;
int_to_b64(X) when X >= 26, X =< 51 -> X - 26 + $a;
int_to_b64(X) when X >= 52, X =< 61 -> X - 52 + $0;
int_to_b64(62) -> $+;
int_to_b64(63) -> $/.



