-module(syncserver).
-copyright('Copyright (c) 2010 Synrc Research Center').
-author('Maxim Sokhatsky <maxim@synrc.com>').
-export([do/1,start/0,stop/0]).
-include("iserve/include/iserve.hrl").

-define(PORT, 2500).
-define(BIND, {212,9,249,42}).

start() ->
    application:start(ucs),
    inets:start(),
    {ok, Pid} = inets:start(httpd, [
	{port, ?PORT},
	{bind_address, ?BIND},
	{server_name, "esyncmld"},
	{server_root, "/"},
	{document_root, "/"},
	{modules,[?MODULE]}]),
Pid.

do(Req) ->
    {mod,ID,Data,ST,S,CDB,M,AU,RU,HTTPV,RL,Headers,Body,Conn} = Req,
    io:format("<p>Headers: ~p</p>",[Headers]),
    case M of
    "POST" ->
		{Val,{CTypeC,CType}} = lists:keysearch("content-type",1,Headers),
		io:format("<p>Content Type: ~p</p>",[CType]),
		{ok,DTD} = file:read_file("syncml_12.dtd"),
		case CType of 
		"application/vnd.syncml+xml" ->
	    	XML = Body,
	    	io:format("<p>Incoming Text Content ~p</p>~n<pre>~s</pre>~n",[erlang:now(),xml_print:pp(XML)]);
		"application/vnd.syncml+wbxml" ->
	    	io:format("<p>Incoming Binary Content ~p</p>~n<pre>~p</pre>~n",[erlang:now(),Body]),
	    	{ok,XML} = wbxml:decode(Body)
		end,
		DTDXML = erlang:binary_to_list(DTD) ++ XML,
		{ok,Warnings,Rules,Parse} = xml:bin(erlang:list_to_binary(DTDXML)),
		io:format("<p>Parse tree<p>~n<pre>~s</pre>~n", [xml_print:pp(Parse)]),
		Response = "OK",
		%omads:handle_syncml_message(Parse),
		{proceed,[{response,{response,[{"content-length",0}],Response}}]}
    end.

stop() ->
    ok = inets:stop(httpd, {?BIND, ?PORT}).

