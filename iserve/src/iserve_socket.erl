
% Authors: 
%
% Sean Hinde, writing the article with the code and all.
% http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features
%
% Torbjörn Törnkvist, building OTP application

-module(iserve_socket).
-export([start_link/5
         ,send_reply/3, send_reply/4
        ]).

-export([init/1]).

-include("../include/iserve.hrl").
-include("iserve_socket.hrl").

-define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").


-define(server_idle_timeout, 30*1000).

start_link(CbMod, CbData, ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, 
                        [{CbMod, CbData, ListenPid, ListenSocket, ListenPort}]).

init({CbMod, CbData, Listen_pid, Listen_socket, ListenPort}) ->
    case catch gen_tcp:accept(Listen_socket) of
	{ok, Socket} ->
            %% Send the cast message to the listener process to create a new acceptor
	    iserve_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock      = Socket,
                   port      = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port,
                   cb_mod    = CbMod,
                   cb_data   = CbData},
	    request(C, #req{}); %% Jump to state 'request'

	Else ->
	    error_logger:error_report([{application, iserve},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.
request(C, Req) ->
    case gen_tcp:recv(C#c.sock, 0, 30000) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(C, Req#req{vsn = Version,
                               method = Method,
                               uri = Path}, []);
        {error, {http_error, "\r\n"}} ->
	    request(C, Req);
	{error, {http_error, "\n"}} ->
            request(C, Req);
	_Other ->
	    exit(normal)
    end.

headers(C, Req, H) ->
    case gen_tcp:recv(C#c.sock, 0, ?server_idle_timeout) of
        {ok, {http_header, _, 'Content-Length', _, Val}} ->
            Len = list_to_integer(Val),
            headers(C, Req#req{content_length = Len}, [{'Content-Length', Len}|H]);
        {ok, {http_header, _, 'Connection', _, Val}} ->
            Keep_alive = keep_alive(Req#req.vsn, Val),
            headers(C, Req#req{connection = Keep_alive}, [{'Connection', Val}|H]);
        {ok, {http_header, _, Header, _, Val}} ->
            headers(C, Req, [{Header, Val}|H]);
        {error, {http_error, "\r\n"}} ->
	    headers(C, Req, H);
	{error, {http_error, "\n"}} ->
            headers(C, Req, H);
        {ok, http_eoh} ->
            body(C, Req#req{headers = lists:reverse(H)});
	_Other ->
	    exit(normal)
    end.

%% Shall we keep the connection alive? 
%% Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
%% Exercise for the reader - finish this so it does case insensitivity properly !
keep_alive({1,1}, "close")      -> close;
keep_alive({1,1}, "Close")      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, _)            -> close;
keep_alive({0,9}, _)            -> close;
keep_alive(Vsn, KA) ->
    io:format("Got = ~p~n",[{Vsn, KA}]),
    close.

body(#c{sock = Sock} = C, Req) ->
    case Req#req.method of
        'GET' ->
            Close = handle_get(C, Req),
            case Close of
                close ->
                    gen_tcp:close(Sock);
                keep_alive ->
                    inet:setopts(Sock, [{packet, http}]),
                    request(C, #req{})
            end;
        'POST' when is_integer(Req#req.content_length) ->
            inet:setopts(Sock, [{packet, raw}]),
            case gen_tcp:recv(Sock, Req#req.content_length, 60000) of
                {ok, Bin} ->
                    Close = handle_post(C, Req#req{body = Bin}),
                    case Close of
                        close ->
                            gen_tcp:close(Sock);
                        keep_alive ->
                            inet:setopts(Sock, [{packet, http}]),
                            request(C, #req{})
                    end;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            send(C, ?not_implemented_501),
            exit(normal)
    end.

handle_get(C, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, http, _Host, _, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, _Other_method, _Host, _, _Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

handle_post(C, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, http, _Host, _, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, _Other_method, _Host, _, _Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

call_mfa(C, Req) ->
    Mod = C#c.cb_mod,
    case catch Mod:iserve_request(C, Req) of
        no_reply ->
            ok;

        {'EXIT', Reason} ->
            io:format("Worker Crash = ~p~n",[Reason]),
            exit(normal);

	%% Responses here should be congruent with the methods
	%% in iserve that 'hide' this internal dependence from 
	%% iserve_server behavior callbacks.

	%% A basic identity http response.
        {respond, StatusCode, Headers0, Body} ->
            Headers = add_content_length(Headers0, Body),
            send_reply(C, StatusCode, Headers, Body);
	
	%% Chunked transfer-encoding for streaming output
	{stream, StatusCode, Headers0, Pid, Subscribe} ->
	    TE = {'Transfer-Encoding', "chunked"},
	    Headers1 = [TE |Headers0],
            send_reply(C, StatusCode, Headers1),
	    send_chunked(C, Pid, Subscribe)
    end.

%%% Part of the exported API.
send_reply(C, StatusCode, Headers) ->
    send_reply(C, StatusCode, Headers, "").

send_reply(C, StatusCode, Headers, Body) ->
    Enc_headers = enc_headers(Headers),
    Enc_status = enc_status(StatusCode),
    Resp = [<<"HTTP/1.1 ">>, Enc_status, <<"\r\n">>,
            Enc_headers,
            <<"\r\n">>,
            Body],
    send(C, Resp).

       
add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', erlang:iolist_size(Body)}|Headers]
    end.


enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
    [].
    
enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.

enc_status(200)  -> "200 OK";
enc_status(404)  -> "404 NOT FOUND";
enc_status(501)  -> "501 INTERNAL SERVER ERROR";
enc_status(Code) -> integer_to_list(Code).
  

send(#c{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.


send_chunked(C, Pid, Subscribe) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {subscribe, self(), MRef, Subscribe},
    receive
	{subscribed, MRef} ->
	    send_chunked0(C, Pid, MRef);
	{'DOWN', MRef, process, Pid, Info} ->
	    throw({chunked_process_crash, Info})
    end,
    erlang:demonitor(MRef, [flush]),
    ok.
    
send_chunked0(C, Pid, MRef) ->
    Pid ! {ready, MRef},
    receive
	{chunk, MRef, ChunkData} ->
	    ChunkSize = erlang:integer_to_list(erlang:iolist_size(ChunkData), 
					       16),
	    Chunk = [ChunkSize, <<"\r\n">>, ChunkData, <<"\r\n">>],
	    send(C, Chunk),
	    send_chunked0(C, Pid, MRef);
	{last_chunk, MRef, Trailers} ->
	    Enc_trailers = enc_headers(Trailers),
	    Chunk = [<<"0\r\n">>,
		     Enc_trailers,
		     <<"\r\n">>],
	    send(C, Chunk);
	{'DOWN', MRef, process, Pid, Info} ->
	    throw({chunked_process_crash, Info})
    end.

	    
	    
