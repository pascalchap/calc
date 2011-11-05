-module(calc_store).

-behaviour(gen_server).

-record(state,{var,func}).
-define(SERVER,?MODULE).

%% gen_server call back
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%% api
-export([start_link/0,storevar/2,storefunc/4,getvalue/1,getfunc/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

storevar(Name,Value) ->
	gen_server:cast(?MODULE,{storevar,Name,Value}).

storefunc(Name,Par,Desc,Text) ->
	gen_server:cast(?MODULE,{storefunc,Name,Par,Desc,Text}).

getvalue(Name) ->
	gen_server:call(?MODULE,{readvar,Name}).
	
getfunc(Name) ->
	gen_server:call(?MODULE,{readfunc,Name}).
	
start_link() -> 
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) -> 
	%% register(?MODULE,self()),
	{ok,#state{var=dict:new(),func=dict:new()}}.

handle_call({readvar,Name}, _From, State = #state{var=D}) -> 
	Reply = dict:find(Name,D), 
    {reply, Reply, State};
handle_call({readfunc,Name}, _From, State = #state{func=F}) -> 
	Reply = dict:find(Name,F) , 
    {reply, Reply, State};
handle_call(Request, From, State) -> 
	io:format("calc_store received call: ~p from ~p~n",[Request,From]),
	Reply = ok, 
    {reply, Reply, State}.

handle_cast({storevar,Name,Value}, State = #state{var=D}) -> 
	NewD= dict:store(Name,Value,D),
	{noreply, State#state{var=NewD}};
handle_cast({storefunc,Name,Par,Desc,Text}, State = #state{func=F}) -> 
	NewF= dict:store(Name,{Par,Desc,Text},F),
	{noreply, State#state{func=NewF}};
handle_cast(Msg, State) -> 
	io:format("calc_store received cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info({'EXIT',_P,shutdown},State) -> 
	{stop,State};
handle_info(Msg,State) -> 
	io:format("calc_state received info: ~p~n",[Msg]),
	{noreply,State}.

terminate(_Reason, _State) -> 
	ok.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
