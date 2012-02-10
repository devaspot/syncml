-module(omads).
-copyright('Copyright (c) 2010 Synrc Research Center').
-author('Maxim Sokhatsky <maxim@synrc.com>').
-export([handle_syncml_message/1]).

handle_syncml_message(SyncML) ->
	{"SYNCML",Attr,Children} = SyncML,
	Header = sync_root(Children),
	ok.

sync_root([{"SYNCHDR",Attr,Children}|T]) -> Children, sync_root(T);
sync_root([{"SYNCBODY",Attr,Children}|T]) -> 
	io:format("<p>SYNCBODY</p>",[]),
	dispatch_commands(Children), 
	sync_root(T);
sync_root([]) -> ok.

dispatch_commands(List) -> 
	CmdList = lists:nth(1,List),
	lists:foreach(fun(A) -> dispatch_command(A) end,CmdList);
dispatch_commands([]) -> ok.

dispatch_command({"ALERT",Attr,Children}) -> 
	io:format("<p>Alert</p>",[sync_alert(Children)]),
	ok;
dispatch_command({"PUT",Attr,Children}) -> 
	io:format("<p>Put</p>",[]),
	sync_put(Children), ok;
dispatch_command({"GET",Attr,Children}) -> 
	io:format("<p>Get</p>",[]),
	sync_get(Children), ok;
dispatch_command({CMD,Attr,Children}) -> 
	io:format("<p>Unhandled ~p</p>",[CMD]).

%
%{"ALERT",[],
%	[
%		{"CMDID",[],{pcdata,"2"}},
%		[],
%		[],
%		{"DATA",[],[{pcdata,"200"},[]]},
 %
%		[
%			{"ITEM",[],
 %        		[
%					{"TARGET",[],[{"LOCURI",[],{pcdata,"event"}},[]]},
%					{"SOURCE",[],[{"LOCURI",[],{pcdata,"appointment"}},[]]},
%					{"META",[],
%%%					[],
%					[]
%				]
%			},
%			{"ITEM",[],
 %        		[
%					{"TARGET",[],[{"LOCURI",[],{pcdata,"event"}},[]]},
%					{"SOURCE",[],[{"LOCURI",[],{pcdata,"appointment"}},[]]},
%					{"META",[],
%						[{"ANCHOR",[],
%						[{"LAST",[],{pcdata,"0"}},{"NEXT",[],{pcdata,"1287014141"}}]},[],[],[],[],[],[],[],[],[]]},
%					[],
%					[]
%				]
%			},
%		]
%	]
%},

sync_alert(Children) ->
	CmdId = parse_cmdid(Children),
	io:format("<p>CmdId: ~p</p>",[CmdId]),
	AlertData = parse_data(Children),
	io:format("<p>Data: ~p</p>",[AlertData]),
	Items = parse_items(Children),
	io:format("<p>Items: ~p</p>",[xml_print:pp(Items)]),
	{CmdId,AlertData,Items}.

parse_cmdid([{"CMDID",Attr,{pcdata,CmdId}}|T]) -> CmdId;
parse_cmdid([H|T]) -> parse_cmdid(T);
parse_cmdid([]) -> none.

parse_data([{"DATA",Attr,Children}|T]) -> parse_data_binary_or_xml(Children);
parse_data([H|T]) -> parse_data(T);
parse_data([]) -> none.

parse_items([[{"ITEM",Attr,Children}]|T],Out) -> parse_items(T,Out++[parse_item(Children)]);
parse_items([H|T],Out) -> parse_items(T);
parse_items([],Out) -> Out.

parse_item(Item)
	ItemSource = parse_item_children("SOURCE",Item),
	io:format("<p>Item Source: ~p</p>",[ItemSource]),
	ItemTarget = parse_item_children("TARGET",Item),
	io:format("<p>Item Target: ~p<p>",[ItemTarget]),
	ItemMeta = parse_item_children("META",Item),
	io:format("<p>Item Meta: ~p</p>",[ItemMeta]),
	ItemData = parse_item_children("DATA",Item),
	io:format("<p>Item Data: ~p</p>",[ItemData]),
	{ItemSource,ItemTarget,ItemMeta,ItemData}.

parse_item_children(SourceTarget,[{SourceTarget,Attr,[{"LOCURI",[],{pcdata,LocUri}},LocName]}|T]) ->
	case LocName of
		[] -> {LocUri,none};
		{pcdata,LName} -> {LocUri,LName}
	end;
parse_item_children("META",[{"META",Attr,Children}|T]) -> Children;
parse_item_children("DATA",[{"DATA",Attr,Children}|T]) -> parse_data([{"DATA",Attr,Children}]);
parse_item_children("MOREDATA",[{"MOREDATA",Attr,Children}|T]) -> Children;
parse_item_children(SourceTarget,[H|T]) -> parse_item_children(SourceTarget,T);
parse_item_children(SourceTarget,[]) -> none.

parse_data_binary_or_xml([{pcdata,Data}|T]) -> Data;
parse_data_binary_or_xml([{"DEVINF",Attr,Children}|T]) -> Children;
parse_data_binary_or_xml([H|T]) -> parse_data_binary_or_xml(T);
parse_data_binary_or_xml([]) -> none.

sync_get(Children) -> ok.
sync_put(Children) -> ok.
