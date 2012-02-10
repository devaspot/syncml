-ifndef(LOGGING).
-define(LOGGING,true).
-define(TIMESTAMP, erlang:now()).

-define(debug(Format,Args,Func),   log:log(dbg,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(trace(Format,Args,Func),   log:log(tra,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(info(Format,Args,Func),    log:log(inf,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(warning(Format,Args,Func), log:log(wrn,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(error(Format,Args,Func),   log:log(err,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).
-define(fatal(Format,Args,Func),   log:log(fat,Format,Args,?MODULE,Func,?LINE,self(),?TIMESTAMP)).

-endif.
