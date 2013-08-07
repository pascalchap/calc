-module(calc_store).

-behaviour(gen_server).

-record(state,{var,func}).
-define(SERVER,?MODULE).

%% @doc
%% @todo Organiser le stockage en utilisant mnesia.

%% gen_server call back
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%% api
-export([start_link/0,storevar/2,storefunc/4,getvalue/1,getfunc/1,stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec storevar(_,_) -> 'ok'.
storevar(Name,Value) ->
    gen_server:cast(?MODULE,{storevar,Name,Value}).

-spec storefunc(_,_,_,_) -> 'ok'.
storefunc(Name,Par,Desc,Text) ->
    gen_server:cast(?MODULE,{storefunc,Name,Par,Desc,Text}).

-spec getvalue(_) -> any().
getvalue(Name) ->
    gen_server:call(?MODULE,{readvar,Name}).

-spec getfunc(_) -> any().
getfunc(Name) ->
    gen_server:call(?MODULE,{readfunc,Name}).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE,stop).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([]) -> {'ok',#state{var::dict(),func::dict()}}.

init([]) -> 
    %% register(?MODULE,self()),
    {ok,#state{var=dict:new(),func=dict:new()}}.

-spec handle_call(_,_,_) -> {'reply','error' | 'ok' | {'ok',_},_}.
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

-spec handle_cast(_,_) -> {'noreply',_} | {'stop',_}.
handle_cast(stop,State) ->
    {stop,State};
handle_cast({storevar,Name,Value}, State = #state{var=D}) -> 
    NewD= dict:store(Name,Value,D),
    {noreply, State#state{var=NewD}};
handle_cast({storefunc,Name,Par,Desc,Text}, State = #state{func=F}) -> 
    NewF= dict:store(Name,{Par,Desc,Text},F),
    {noreply, State#state{func=NewF}};
handle_cast(Msg, State) -> 
    io:format("calc_store received cast: ~p~n",[Msg]),
    {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_} | {'stop',_}.
handle_info({'EXIT',_P,shutdown},State) -> 
    {stop,State};
handle_info(Msg,State) -> 
    io:format("calc_state received info: ~p~n",[Msg]),
    {noreply,State}.

-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _State) -> 
    ok.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
