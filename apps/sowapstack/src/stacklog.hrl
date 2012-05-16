-ifndef(LOGGING).
-define(LOGGING,true).

-define(TIMESTAMP, erlang:now()).


%%______________________________________________________________________
%% Debug/trace output
%% 

-define(debug(Format,Args,Func),
	sowapstack_log:log(dbg,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(trace(Format,Args,Func),
	sowapstack_log:log(tra,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(info(Format,Args,Func),
	sowapstack_log:log(inf,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(warning(Format,Args,Func),
	sowapstack_log:log(wrn,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(error(Format,Args,Func),
	sowapstack_log:log(err,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(fatal(Format,Args,Func),
	sowapstack_log:log(fat,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).

-endif.



