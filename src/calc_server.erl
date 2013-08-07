-module(calc_server).

-behaviour(gen_server).

-record(state, {gui, store}).

-define(SERVER, ?MODULE).

%% callback export
-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

%% Interface export
-export([display/3, evaluate/2, parse_eval/2,
     start_link/0]).

-export([start_app/0]).

-spec start_app() -> 'ok' | {'error',_}.
start_app() -> application:start(calc).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
              []).

-spec display(atom() | pid() | {atom(),_} | {'via',_,_},_,_) -> 'ok'.
display(Aff, T, Rep) ->
    gen_server:cast(Aff, {display, T, Rep}).

-spec evaluate(_,atom() | pid() | {atom(),_} | {'via',_,_}) -> 'ok'.
evaluate(T, Calc) ->
    gen_server:cast(Calc, {parse_evaluate, T}).

-spec parse_eval(_,atom() | pid() | {atom(),_} | {'via',_,_}) -> 'ok'.
parse_eval(T, Aff) ->
    Rep = calc:parse_evaluate(T),
    calc_server:display(Aff, T, Rep).

%% callback functions   
-spec init([]) -> {'ok',#state{store::'ignore' | {'error',_} | {'ok',pid()}}}.
init([]) ->
    G = calcgui:start_link(?SERVER),
    S = calc_store:start_link(),
    io:format("Calcgui PId is ~p, Store PID is ~p~n",
          [G, S]),
    {ok, #state{gui = G, store = S}}.

-spec handle_call(_,_,_) -> {'reply','ok',_}.
handle_call(Request, From, State) ->
    io:format("calc_server received call: ~p from ~p~n",
          [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({parse_evaluate, T}, State) ->
    V1 = (?SERVER),
    spawn(fun () -> (?MODULE):parse_eval(T, V1) end),
    {noreply, State};
handle_cast(Mess = {display, _Input, _Rep},
        State = #state{gui = G}) ->
    G ! Mess, {noreply, State};
handle_cast(Msg, State) ->
    io:format("calc_server received cast: ~p~n", [Msg]),
    {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_} | {'stop',_}.
handle_info({'EXIT', _P, shutdown}, State) ->
    {stop, State};
handle_info(Msg, State) ->
    io:format("calc_server received info: ~p~n", [Msg]),
    {noreply, State}.

-spec terminate(_,_) -> 'ok'.
terminate(Reason, _State) ->
    io:format("terminate calc server with reason: ~p~n",
          [Reason]),
    ok.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

