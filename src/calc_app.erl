-module(calc_app). 
-version("0.2.0").
-behaviour(application). 
-export([ 
	  start/2, 
	  stop/1 
	]). 
-spec start(_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start(_Type, _StartArgs) -> 
    case calc_sup:start_link() of
	{ok, Pid} ->  {ok, Pid}; 
	Error ->  Error
    end. 
-spec stop(_) -> 'ok'.
stop(_State) -> 
	init:stop().