-module(calc_sup). 
-behaviour(supervisor). 
%% API 
-export([start_link/0]).
%% Supervisor callbacks 
-export([init/1]). 
-define(SERVER, ?MODULE). 
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> 
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). 
-spec init([]) -> {'ok',{{'one_for_one',10,1},[{atom(),{atom(),atom(),[]},transient,pos_integer(),worker,[atom()]},...]}}.
init([]) -> 
    %% process_flag(trap_exit, true), 
    io:format("enter init~n"),
    Calc = {calc,{calc_server,start_link, []}, 
	    transient,1000,worker,[calcgui,calc,calc_server]}, 
    {ok,{{one_for_one,10,1},[Calc]}}.
