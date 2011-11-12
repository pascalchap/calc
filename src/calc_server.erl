-module(calc_server).

-behaviour(gen_server).

-record(state,{gui,store}).

-define(SERVER,?MODULE).
%% callback export
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
%% Interface export
-export([start_link/0,display/3,evaluate/2,select_var/2]).
-export([start_app/0]).

start_app() ->
	application:start(calc).

start_link() -> 
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

display(Aff,T,Rep) ->
	gen_server:cast(Aff,{display,T,Rep}).
select_var(Aff,L) ->
	gen_server:call(Aff,{select_var,L}).
evaluate(T,Calc) ->
	gen_server:cast(Calc,{parse_evaluate,T}).
	
%% callback functions	
init([]) -> 
	G = calcgui:start_link(?SERVER),
	S = calc_store:start_link(),
	io:format("Calcgui PId is ~p, Store PID is ~p~n",[G,S]),
	{ok,#state{gui=G,store=S}}.

handle_call({select_var,L}, _From, State = #state{gui = G}) -> 
	Reply = calcgui:selectvar(L,G),
	{reply, Reply, State};
handle_call(Request, From, State) -> 
	io:format("calc_server received call: ~p from ~p~n",[Request,From]),
	Reply = ok, 
    {reply, Reply, State}.

handle_cast({parse_evaluate,T}, State) -> 
	spawn(calc,parse_evaluate,[T,?SERVER]),
	{noreply, State};
handle_cast(Mess = {display,_Input,_Rep}, State = #state{gui = G}) -> 
	G ! Mess,
	{noreply, State};
handle_cast(Msg, State) -> 
	io:format("calc_server received cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info({'EXIT',_P,shutdown},State) -> 
	{stop,State};
handle_info(Msg,State) -> 
	io:format("calc_server received info: ~p~n",[Msg]),
	{noreply,State}.

terminate(Reason, _State) -> 
	io:format("terminate calc server with reason: ~p~n",[Reason]),
	ok.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
