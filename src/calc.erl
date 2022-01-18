-module(calc).

%-compile(export_all).

-export([parse_evaluate/1]).


-include("../include/calc.hrl").

-type print_par() :: 40 | 41.

%% @doc The only exported function of this module is in charge to :
%%  - parse a string representing a math expression
%%  - evaluate as deep as possible the expression, replacing variables
%%    and function by their stored value (if any) and performing all possible opération and simplification.
%%  - return the result in a printable format
-spec parse_evaluate(string()) -> string().
parse_evaluate(T) -> 
    R = (catch evaluate(parse(T))),
    R1 = case R of
       {'EXIT', {Reason, _Info}} ->
           {error, atom_to_list(Reason)};
       {error, _Reason} = Error2 -> Error2;
       {assign, {userfunc, N, A}, B} ->
           case catch checkuserfunc(B, chekleft(A)) of
         ok -> storeuserfunc(N, A, B, T);
         {'EXIT', {{Reason, _}, _Info}} ->
             {error, atom_to_list(Reason)}
           end;
       {assign, {var, N}, A} -> storevar(N, evaluate(A));
       R -> R
     end,
    print(R1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse(string()) -> any().
parse(L) -> 
  L1 = split(L),
  {value,L2} = level(L1),
  priorize(L2).

-spec evaluate(_) -> any().
evaluate(Exp) ->
    A = (catch evaluate1(Exp)),
    evaluate2(A, Exp).

-spec evaluate1({'const',[101 | 105 | 112,...]} | {'minus',{'const',[any(),...]} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'assign',{_,_} | {_,_,_},_} | {'func',{_,_},[any()]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'assign',{'var',_} | {'userfunc',_,_},_} | {'func',{[any()],atom() | tuple()},[{_,_} | {_,_,_} | {_,_,_,_}]} | {'userfunc',_,_} | {'op',[1..255,...],{'const',[any(),...]} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'assign',{_,_} | {_,_,_},_} | {'func',{_,_},[any()]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',[any(),...]} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'assign',{_,_} | {_,_,_},_} | {'func',{_,_},[any()]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}}) -> any().
evaluate1({minus, A}) -> -evaluate1(A);
evaluate1({op, "+", A, B}) ->
    evaluate1(A) + evaluate1(B);
evaluate1({op, "-", A, B}) ->
    evaluate1(A) - evaluate1(B);
evaluate1({op, "*", A, B}) ->
    evaluate1(A) * evaluate1(B);
evaluate1({op, "/", A, B}) ->
    evaluate1(A) / evaluate1(B);
evaluate1({op, "rem", A, B}) ->
    evaluate1(A) rem evaluate1(B);
evaluate1({op, "div", A, B}) ->
    evaluate1(A) div evaluate1(B);
evaluate1({op, "arrg", A, B}) ->
    arrg(evaluate1(A), evaluate1(B));
evaluate1({op, "comb", A, B}) ->
    comb(evaluate1(A), evaluate1(B));
evaluate1({op, "^", A, B}) ->
    pow(evaluate1(A), evaluate1(B));
evaluate1({func, {N, L}, A}) ->
    apply(L, list_to_atom(N), [evaluate1(X) || X <- A]);
evaluate1({userfunc, N, A}) ->
    evalfunc(getuserfunc(N), A);
evaluate1({const, N}) -> getconst(N);
evaluate1({var, N}) -> getvalue(N);
evaluate1(R = {assign, {userfunc, _N, _A}, _B}) -> R;
evaluate1(R = {assign, {var, _N}, _A}) -> R;
evaluate1({num, A}) -> A.

%% evaluate2({num,A},_Exp) -> return_num(A,round(A));
-spec evaluate2(_,_) -> any().
evaluate2(A, _Exp) when is_number(A) ->
    return_num(A, round(A));
evaluate2({'EXIT', _Reason} = Err, _Exp) -> Err;
evaluate2({error, "variable undefined"}, Exp) ->
    simplify(Exp);
evaluate2(A, _Exp) -> A.

-spec return_num(number(),integer()) -> number().
return_num(A, B) when A == B -> B;
return_num(A, _B) -> A.

-spec getuserfunc(_) -> any().
getuserfunc(N) ->
    case calc_store:getfunc(N) of
      {ok, Value} -> Value;
      error -> throw({error, "function undefined"})
    end.

-spec storeuserfunc(_,_,_,_) -> [[any()] | char()].
storeuserfunc(Name, Par, Desc, Text) ->
    Desc1 = simplify(Desc),
    calc_store:storefunc(Name, Par, Desc1, Text),
    io_lib:format("function ~p(~p) = ~p stored",
          [Name, Par, print(Desc1)]).

-spec getconst([101 | 105 | 112,...]) -> float().
getconst("pi") -> math:pi();
getconst("e") -> math:exp(1).

-spec getvalue(_) -> any().
getvalue(N) ->
    case calc_store:getvalue(N) of
      {ok, Value} -> Value;
      error -> throw({error, "variable undefined"})
    end.

-spec storevar(_,_) -> [[any()] | char()].
storevar(Name, Value) ->
    calc_store:storevar(Name, Value),
    io_lib:format("variable ~p = ~p stored", [Name, Value]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% split the string in pieces, identify and translate them

-spec split(string()) -> [split_term()].
split(T) ->
    {ok, M} = (?MS),
    {match, L} = re:run(T, M, [global]),
    First = fun (X) when is_list(X) -> hd(X);
        (X) -> X
        end,
    L1 = [First(X) || X <- L],
    Conv = fun (X = [H])
           when H =:= $+;
            H =:= $-;
            H =:= $*;
            H =:= $/;
            H =:= $(;
            H =:= $);
            H =:= $^;
            H =:= $,;
            H =:= $= ->
           {sep, X};
           (X = [H | _]) when H >= $A, H =< $Z -> {new, X};
           (X = [H | _])
           when H >= $0, H =< $9; H =:= $-; H =:= $+ ->
           {num, getnum(X)};
           (X = [H | _]) when H >= $a, H =< $z -> getsubstitute(X)
       end,
    [Conv(lists:sublist(T, X + 1, Y)) || {X, Y} <- L1].

-spec getsubstitute([any(),...]) -> any().
getsubstitute(X) ->
    F = fun (K = {func, {N, _}}, _) when N =:= X -> K;
        (K = {sep, N}, _) when N =:= X -> K;
        (K = {const, N}, _) when N =:= X -> K;
        (_, A) -> A
    end,
    lists:foldl(F, {new, X}, ?KEYWORDS).

-spec getnum([any(),...]) -> number().
getnum(X) ->
    case {lists:member($#, X), lists:member($., X)} of
      {true, false} ->
      {B1, [_ | X1]} = lists:splitwith(fun (E) -> E =/= $#
                       end,
                       X),
      list_to_integer(X1, list_to_integer(B1));
      {false, true} -> list_to_float(X);
      {false, false} -> list_to_integer(X)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% management of parenthesis

-spec level([split_term()]) -> {value,level_terms()} | {error,string()}.
level(L) -> level(L, [], 0).

-spec level([split_term()], level_terms(), 0|1) -> {value,level_terms()} | {error,string()}.
level([], A, 0) ->
    {value,lists:reverse(A)};              %% end of the analyse
level([], _A, _I) ->
    throw({error,
       "trop de ("});  %% end of the list encountered while analysing a sublevel
%%level(A, [], 0) when is_tuple(A) ->
%%    A;     %% isolated term like {num,15} in 16#F
level([{sep, ")"} | T], A, 1) ->
    {lists:reverse(A), T}; %% close current level
level([{sep, ")"} | _T], _A, 0) ->
    throw({error,
       "trop de )"}); %% missing corresponding "("
level([{new, N}, {sep, "("} | T], A,
      C) ->     %% a new name followed by "(" should be a user defined function
    {L, R} = level(T, [],
           1),                      %% analyse sublevel
    level(R, [L, {userfunc, N} | A],
      C);      %% then continue
level([{sep, "("} | T], A, C) ->    %% at each "("
    {L, R} = level(T, [], 1),       %% analyse a new sublevel
    level(R, [L | A], C);                                   %% then continue
level([{new, N} | T], A, C) ->
    level(T, [{var, N} | A], C);       %% remaining new name should be variables
level([H | T], A, C) when is_tuple(H) ->
    level(T, [H | A], C).
%%level([H | T], A, C) ->
%%    level(T, [level(H, [], 0) | A], C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% management of operator priority

-spec priorize(level_terms()) -> tree_term().
priorize(L) ->
    L0 = prio_assign(L, [], false),
    L1 = prio_func(L0, []),
    La = prio_arg(L1, []),
    L2 = prio_power(La, []),
    L3 = prio_mult(L2, []),
    L4 = prio_add(L3, []),
    totuple(L4).

-spec prio_assign(level_terms(), level_terms(), boolean()) -> level_terms() .
prio_assign([], L, _) -> lists:reverse(L);
prio_assign([{sep, "="} | T], R, false) ->
    Left = lists:reverse(R),
    Right = prio_assign(T, [], true),
    [assign, Left, Right];
prio_assign([{sep, "="} | _T], _R, true) ->
    throw({error, "more than 1 ="});
prio_assign([H | T], R, B) ->
    prio_assign(T, [H | R], B).

%% condition de fin
-spec prio_func(level_terms(), level_terms()) -> level_terms().
prio_func([], L) -> lists:reverse(L);
prio_func(T, []) when not is_list(T) -> T;
%% pour une fonction, identifier la liste de parametre
prio_func([{M, F}, A | T], R)
    when M == func; M == userfunc ->
    prio_func(T, [[M, F, prio_func(A, [])] | R]);
prio_func([H | T], R) ->
    prio_func(T, [prio_func(H, []) | R]).

-spec prio_arg(level_terms(), level_terms()) -> level_terms().
prio_arg([], L) -> lists:reverse(L);
prio_arg(T, []) when not is_list(T) -> T;
prio_arg([[M, F, A] | T], R)
    when M == func; M == userfunc ->
    prio_arg(T,
         [[M, F, [prio_arg(X, []) || X <- liste_arg(A, [], [])]]
          | R]);
prio_arg([H | T], R) ->
    prio_arg(T, [prio_arg(H, []) | R]).

-spec liste_arg([any()],[any()],[[any()]]) -> [[any()]].
liste_arg([], [], D) -> lists:reverse(D);
liste_arg([], C, D) ->
    liste_arg([], [], [lists:reverse(C) | D]);
liste_arg([{sep, ","} | R], C, D) ->
    liste_arg(R, [], [lists:reverse(C) | D]);
liste_arg([H | R], C, D) -> liste_arg(R, [H | C], D).

-spec prio_power(level_terms(), level_terms()) -> level_terms().
prio_power([], L) -> lists:reverse(L);
prio_power(T, []) when not is_list(T) -> T;
prio_power([A, {sep, "^"}, B | T], R) ->
    prio_power([[op, "^", prio_power(A, []),
         prio_power(B, [])]
        | T],
           R);
prio_power([H | T], R) ->
    prio_power(T, [prio_power(H, []) | R]).

-spec prio_mult(level_terms(), level_terms()) -> level_terms().
prio_mult([], L) -> lists:reverse(L);
prio_mult(T, []) when not is_list(T) -> T;
%% les - et + unaire (en début de liste) sont équivalent à une multiplication par + ou - 1
prio_mult([{sep, "-"}, A | T], []) ->
    prio_mult([[minus, prio_mult(A, [])] | T], []);
prio_mult([{sep, "+"}, A | T], []) ->
    prio_mult([prio_mult(A, []) | T], []);
%% dans un niveau de parenthese donné, on execute ces opération de gauche à droite, dans l'ordre ou on les rencontre
prio_mult([A, {sep, Op}, B | T], R)
    when Op == "*";
     Op == "/";
     Op == "div";
     Op == "rem";
     Op == "comb";
     Op == "arrg" ->
    prio_mult([[op, Op, prio_mult(A, []), prio_mult(B, [])]
           | T],
          R);
prio_mult([H | T], R) ->
    prio_mult(T, [prio_mult(H, []) | R]).

-spec prio_add(level_terms(), level_terms()) -> level_terms().
prio_add([], L) -> lists:reverse(L);
prio_add(T, []) when not is_list(T) -> T;
%% prio_add([{sep,","}|T],A) -> prio_add(T,A);
prio_add([A, {sep, Op}, B | T], R)
    when Op == "+"; Op == "-" ->
    prio_add([[op, Op, prio_add(A, []), prio_add(B, [])]
          | T],
         R);
prio_add([H | T], R) ->
    prio_add(T, [prio_add(H, []) | R]).

-spec totuple(level_terms()) -> tree_term().
totuple(X) when not is_list(X) -> X;
totuple([X]) when not is_integer(X) -> totuple(X);
totuple([minus, A]) -> {minus, totuple(A)};
totuple([assign, A, B]) ->
    {assign, totuple(A), totuple(B)};
totuple([func, A, L]) ->
    {func, totuple(A), totuple(L, [])};
totuple([userfunc, A, L]) ->
    {userfunc, totuple(A), totuple(L, [])};
%% totuple([var,A]) -> {var,totuple(A)};
totuple([op, N, A, B]) ->
    {op, N, totuple(A), totuple(B)};
%% totuple([num,N]) -> {num,N};
%% totuple([const,N]) -> {const,N};
totuple(L) -> totuple(L, []).

-spec totuple([any()],[any()]) -> [any()].
totuple([], A) -> lists:reverse(A);
totuple([H | T], A) -> totuple(T, [totuple(H) | A]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec drv({'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}},_) -> {'minus',{'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',0 | 1} | {'func',{_,_},[any(),...]} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',0 | 1} | {'func',{[any(),...],'math'},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'op',[42 | 43 | 45 | 47 | 94,...],{'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',number()} | {'func',{_,_},[any(),...]} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},[any(),...] | {_,_} | {_,_,_} | {_,_,_,_}}}.
drv({userfunc, N, A}, X) ->
    derive(evalfunc(getuserfunc(N), A), X);
drv(F, X) -> derive(F, X).

-spec fact(non_neg_integer()) -> pos_integer().
fact(A) when is_integer(A), A >= 0 -> fact(A, 1);
fact(A) ->
    Err = io_lib:format("evaluation error: fact(~p)~n",
            [A]),
    throw({error, Err}).

-spec fact(non_neg_integer(),pos_integer()) -> pos_integer().
fact(0, R) -> R;
fact(A, R) -> fact(A - 1, A * R).

-spec comb(pos_integer(),pos_integer()) -> integer().
comb(N, K)
    when is_integer(N), is_integer(K), N >= K, K >= 1 ->
    comb2(K, arrg(N, N - K + 1, 1));
comb(N, K) ->
    Err = io_lib:format("evaluation error: ~p comb ~p~n",
            [N, K]),
    throw({error, Err}).

-spec arrg(pos_integer(),pos_integer()) -> integer().
arrg(N, K)
    when is_integer(N), is_integer(K), N >= K, K >= 1 ->
    arrg(N, N - K + 1, 1);
arrg(N, K) ->
    Err = io_lib:format("evaluation error: ~p arrg ~p~n",
            [N, K]),
    throw({error, Err}).

-spec arrg(integer(),integer(),integer()) -> integer().
arrg(M, N, T) when N > M -> T;
arrg(M, N, T) -> arrg(M - 1, N, T * M).

-spec comb2(pos_integer(),integer()) -> integer().
comb2(1, T) -> T;
comb2(K, T) -> comb2(K - 1, T div K).

-spec int(number()) -> integer().
int(N) -> floor(N).

-spec frac(number()) -> number().
frac(N) -> N - int(N).

-spec evalfunc({_,_,_},_) -> any().
evalfunc({Par, Desc, _Text}, A) ->
    evaluate(replace(Desc, Par, A)).

-spec replace(_,_,_) -> any().
replace({var, N}, [{var, N} | _Rn], [O | _Ro]) -> O;
replace({var, _} = N, [_N | Rn], [_O | Ro]) ->
    replace(N, Rn, Ro);
%% replace({minus,B},N,O) -> {minus,replace(B,N,O)}; %% never reached
replace({op, Op, B1, B2}, N, O) ->
    {op, Op, replace(B1, N, O), replace(B2, N, O)};
replace({Tf, F, P}, N, O) ->
    {Tf, F, [replace(X, N, O) || X <- P]};
replace(B, _N, _O) -> B.

%% A must be a list of {var,N} not empty
-spec chekleft([any()]) -> nonempty_maybe_improper_list().
chekleft(A) ->
    [_ | _] = lists:foldl(fun (X, Acc) ->
                  {var, N} = X, [N | Acc]
              end,
              [], A).

%% The function definition must only include variables defined in Listvar
-spec checkuserfunc(_,[any(),...]) -> 'ok'.
checkuserfunc(Def, Listvar) ->
    All = lists:sort(Listvar),
    Part = getvarlist(Def),
    %% Check inclusion, lists:substract should be OK because lists must be very small.
    Rest = All -- Part,
    Part = All -- Rest,
    ok.

-spec getvarlist(_) -> [any()].
getvarlist(S) -> lists:usort(getvarlist(S, [])).

-spec getvarlist(_,_) -> any().
getvarlist({var, N}, Acc) -> [N | Acc];
getvarlist({minus, B}, Acc) -> getvarlist(B, Acc);
getvarlist({op, _Op, B1, B2}, Acc) ->
    NewAcc = getvarlist(B1, Acc), getvarlist(B2, NewAcc);
getvarlist({_Tf, _F, P}, Acc) ->
    lists:foldl(fun (X, A) -> getvarlist(X, A) end, Acc, P);
getvarlist(_B, Acc) -> Acc.

-spec derive({'const',_} | {'minus',{'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}}} | {'num',_} | {'var',_} | {'func',{[1..255,...],_},[{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}},{'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}}},_) -> {'minus',{'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',0 | 1} | {'func',{_,_},[any(),...]} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',0 | 1} | {'func',{[any(),...],'math'},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'op',[42 | 43 | 45 | 47 | 94,...],{'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',number()} | {'func',{_,_},[any(),...]} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},[any(),...] | {_,_} | {_,_,_} | {_,_,_,_}}}.
derive({num, _}, _) -> {num, 0};
derive({const, _}, _) -> {num, 0};
derive({var, N}, N) -> {num, 1};
derive({var, _}, _) -> {num, 0};
derive({minus, A}, V) -> {minus, derive(A, V)};
derive({op, "+", A, B}, V) ->
    {op, "+", derive(A, V), derive(B, V)};
derive({op, "-", A, B}, V) ->
    {op, "-", derive(A, V), derive(B, V)};
derive({op, "*", A, B}, V) ->
    {op, "+", {op, "*", derive(A, V), B},
     {op, "*", A, derive(B, V)}};
derive({op, "/", A, B}, V) ->
    {op, "/",
     {op, "-", {op, "*", derive(A, V), B},
      {op, "*", A, derive(B, V)}},
     {op, "^", B, {num, 2}}};
derive({op, "^", {var, V} = Var, {num, N} = Num1}, V) ->
    {op, "*", Num1, {op, "^", Var, {num, N - 1}}};
derive({op, "^", {var, _}, {num, _N}}, _V) -> {num, 0};
derive({op, "^", {var, V} = Var, A} = Op1, V) ->
    {op, "*",
     {op, "+",
      {op, "*", derive(A, V), {func, {"log", math}, [Var]}},
      {op, "/", A, Var}},
     Op1};
derive({op, "^", A, B} = Op, V) ->
    {op, "*",
     {op, "+",
      {op, "*", derive(B, V), {func, {"log", math}, [A]}},
      {op, "/", {op, "*", B, derive(A, V)}, A}},
     Op};
derive({userfunc, N, A}, V) ->
    {Sa, D, _} = getuserfunc(N),
    derive(replace(D, Sa, A), V);
derive({func, {N, _}, [{var, V}]}, V) ->
    getderive(N, {var, V});
derive({func, {N, _}, [A]}, V) ->
    {op, "*", derive(A, V), getderive(N, A)}.

-spec getderive([1..255,...],{'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}}) -> {'minus',{'func',{[any(),...],'math'},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'op',[94,...],{'op',[any(),...],{_,_},{_,_,_,_}},{'num',float()}}} | {'func',{[99 | 101 | 104 | 105 | 110 | 111 | 112 | 115 | 120,...],'math'},[{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},...]} | {'op',[42 | 47 | 94,...],{'num',number()} | {'op',[43 | 45,...],{'num',1} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_}},{'num',1} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_}}},{'const',_} | {'minus',{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',_} | {'func',{[any(),...],_},[{_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',_,_} | {'op',[42 | 43 | 45 | 47 | 94,...],{'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}},[{_,_} | {_,_,_} | {_,_,_,_},...] | {'const',_} | {'minus',{_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',_} | {'func',{_,_},[any(),...]} | {'userfunc',_,_} | {'op',[any(),...],{_,_} | {_,_,_} | {_,_,_,_},{_,_} | {_,_,_} | {_,_,_,_}}}}.
getderive("sin", A) -> {func, {"cos", math}, [A]};
getderive("cos", A) ->
    {minus, {func, {"sin", math}, [A]}};
getderive("sinh", A) -> {func, {"cosh", math}, [A]};
getderive("cosh", A) -> {func, {"sinh", math}, [A]};
getderive("tan", A) ->
    {op, "/", {num, 1},
     {op, "^", {func, {"cos", math}, [A]}, {num, 2}}};
getderive("tanh", A) ->
    {op, "/", {num, 1},
     {op, "^", {func, {"cosh", math}, [A]}, {num, 2}}};
getderive("exp", A) -> {func, {"exp", math}, [A]};
getderive("log", A) -> {op, "/", {num, 1}, A};
getderive("log10", A) ->
    {op, "/", {num, 1},
     {op, "*", {num, math:log(10)}, [A]}};
getderive("acos", A) ->
    {minus,
     {op, "^", {op, "-", {num, 1}, {op, "^", A, {num, 2}}},
      {num, -5.0e-1}}};
getderive("asin", A) ->
    {op, "^", {op, "-", {num, 1}, {op, "^", A, {num, 2}}},
     {num, -5.0e-1}};
getderive("atan", A) ->
    {op, "/", {num, 1},
     {op, "+", {num, 1}, {op, "^", A, {num, 2}}}};
getderive("acosh", A) ->
    {op, "^", {op, "-", {op, "^", A, {num, 2}}, {num, 1}},
     {num, -5.0e-1}};
getderive("asinh", A) ->
    {op, "^", {op, "+", {num, 1}, {op, "^", A, {num, 2}}},
     {num, -5.0e-1}};
getderive("atanh", A) ->
    {op, "/",  {num, 1},
     {op, "-", {num, 1}, {op, "^", A, {num, 2}}}};
getderive("sqrt", A) ->
    {op, "*", {num, 5.0e-1}, {op, "^", A, {num, -5.0e-1}}}.

-spec simplify(_) -> any().
simplify({func, {"drv", _}, [F, {var, X}]}) ->
    simplify(drv(F, X));
simplify({func, N, [A]}) -> {func, N, [simplify(A)]};
%% simplify({userfunc,N,A}) -> {Sa,D,_} = getuserfunc(N), simplify(replace(D,Sa,A)); %% never reached
simplify({minus, A}) -> reduce({minus, simplify(A)});
simplify({op, Op, A, B}) ->
    reduce({op, Op, simplify(A), simplify(B)});
simplify(A) -> reduce(A).

%% reduce(Exp) return equivalent expression with some simplification that should lighten the printing
%% -.- = + and similar rules
-spec reduce(_) -> any().
reduce({minus, {minus, A}}) -> reduce(A);
reduce({op, Op, {minus, A}, {minus, B}})
    when Op == "*"; Op == "/" ->
    reduce({op, Op, A, B});
reduce({op, Op, {minus, A}, {num, N}})
    when Op == "*"; Op == "/" ->
    reduce({op, Op, A, {num, -N}});
reduce({minus, {num, N}}) -> {num, -N};
reduce({minus, A}) -> {minus, reduce(A)};
reduce({op, "-", A, {minus, B}}) ->
    reduce({op, "+", A, B});
reduce({op, "+", A, {minus, B}}) ->
    reduce({op, "-", A, B});
reduce({op, Op, A, {minus, B}})
    when Op == "*"; Op == "/" ->
    {minus, reduce({op, Op, A, B})};
reduce({op, Op, {minus, A}, B})
    when Op == "*"; Op == "/" ->
    {minus, reduce({op, Op, A, B})};
%% simplify multiple qotient, trying to wite a ratio of products
reduce({op, "/", A, {op, "/", B, C}}) ->
    reduce({op, "/", {op, "*", A, C}, B});
reduce({op, "/", {op, "/", A, B}, C}) ->
    reduce({op, "/", A, {op, "*", C, B}});
reduce({op, "*", {num, N1}, {op, Op, {num, N2}, A}})
    when Op == "*"; Op == "/" ->
    reduce({op, Op, {num, N1 * N2}, A});
reduce({op, "/", {op, Op, {num, _N1} = Num1, A},
    {num, _N2} = Num2})
    when Op == "*"; Op == "/" ->
    reduce({op, Op, reduce({op, "/", Num1, Num2}), A});
reduce({op, "/", {num, _N1} = Num1,
    {op, "*", {num, _N2} = Num2, A}}) ->
    reduce({op, "/", reduce({op, "/", Num1, Num2}), A});
%% try to gather the numerical value to the left of op when it is possible
%% operation with 2 numbers
reduce({op, "*", {num, N1}, {num, N2}}) ->
    {num, N1 * N2};
reduce({op, "/", {num, N1}, {num, N2}})
    when is_float(N1); is_float(N2) ->
    {num, N1 / N2};
reduce({op, "/", {num, N1}, {num, N2}})
    when N1 rem N2 =:= 0 ->
    {num, N1 div N2};
reduce({op, "/", {num, _N1}, {num, _N2}} = A) -> A;
reduce({op, "+", {num, N1}, {num, N2}}) ->
    {num, N1 + N2};
reduce({op, "-", {num, N1}, {num, N2}}) ->
    {num, N1 - N2};
reduce({op, "^", {num, N1}, {num, N2}}) ->
    {num, pow(N1, N2)};
reduce({op, Op, {num, _N1}, {num, _N2}} = A)
    when Op =/= "*", Op =/= "/", Op =/= "+", Op =/= "-",
     Op =/= "^" ->
    {num, evaluate(A)};
reduce({op, Op, A, {num, _N} = Num})
    when Op == "+"; Op == "*" ->
    reduce({op, Op, Num, A});
reduce({op, "-", A, {num, N}}) ->
    reduce({op, "+", {num, -N}, A});
reduce({op, "/", A, {num, N}}) when is_float(N) ->
    reduce({op, "*", {num, 1 / N}, A});
reduce({op, "/", A, {num, _N} = Num1}) ->
    reduce({op, "*", reduce({op, "/", {num, 1}, Num1}), A});
%% trivial numerical expression (not really accurate regarding fundamental maths)
reduce({op, "*", {num, N}, _A}) when N == 0 -> {num, 0};
reduce({op, "+", {num, N}, A}) when N == 0 -> A;
reduce({op, "-", {num, N}, A}) when N == 0 ->
    {minus, A};
reduce({op, "/", {num, N}, _A}) when N == 0 -> {num, 0};
reduce({op, "^", {num, N}, _A}) when N == 0 -> {num, 0};
reduce({op, "^", _A, {num, N}}) when N == 0 -> {num, 1};
reduce({op, "*", {num, N}, A}) when N == 1 -> A;
reduce({op, "^", {num, N}, _A}) when N == 1 -> {num, 1};
reduce({op, "^", A, {num, N}}) when N == 1 -> A;
%% recognize some other patterns
reduce({op, "*", A, A}) ->
    {op, "^", reduce(A), {num, 2}};
reduce({op, "/", A, A}) -> {num, 1};
reduce({op, "*", A, {op, "^", A, B}}) ->
    reduce({op, "^", reduce(A),
        reduce({op, "+", {num, 1}, B})});
reduce({op, "*", {op, "^", A, B}, A}) ->
    reduce({op, "^", reduce(A),
        reduce({op, "+", {num, 1}, B})});
reduce({op, "/", A, {op, "^", A, B}}) ->
    reduce({op, "^", reduce(A),
        reduce({op, "-", {num, 1}, B})});
reduce({op, "/", {op, "^", A, B}, A}) ->
    reduce({op, "^", reduce(A),
        reduce({op, "-", B, {num, 1}})});
reduce({op, Op, {op, Op, A, B}, {op, Op, _C, _D} = Op3})
    when Op == "+"; Op == "*" ->
    reduce({op, Op, A, reduce({op, Op, B, reduce(Op3)})});
reduce({op, Op, {num, _N1} = Num1,
    {op, Op, {num, _N2} = Num2, A}})
    when Op == "+"; Op == "*" ->
    reduce({op, Op, {num, evaluate({op, Op, Num1, Num2})},
        A});
reduce({op, Op, A, {op, Op, {num, _N} = Num, B}})
    when Op == "+"; Op == "*" ->
    reduce({op, Op, Num, {op, Op, A, B}});
reduce({op, Op, A, B}) ->
    {op, Op, reduce(A), reduce(B)};
reduce(A) -> A.

-spec pow(_,number()) -> number().
pow(_, 0) -> 1;
pow(X, N) when is_integer(N), N > 0, is_integer(X) ->
    pow(X, N, 1);
pow(X, N) -> math:pow(X, N).

-spec pow(integer(),non_neg_integer(),integer()) -> integer().
pow(_, 0, R) -> R;
pow(X, N, R) -> pow(X, N - 1, X * R).

-spec print(nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',[any()]} | {'func',{[any()],_},[nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',[any()],[nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}},nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}}}) -> any().
print(A) -> print(A, 10).

%% priority level
%% 0 top
%% 1 func, userfunc
%% 2 ^
%% 3 / rem div
%% 4 minus *
%% 5 + -

-spec print(nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}}} | {'num',_} | {'var',[any()]} | {'func',{[any()],_},[nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},...]} | {'userfunc',[any()],[nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}},nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}}},2 | 3 | 4 | 5 | 10) -> any().
print({func, {N, _}, [A]}, _P) ->
    N ++ "(" ++ print(A, 5) ++ ")";
print({userfunc,N,Par}, _P) ->
    N ++ "(" ++ printpar(Par) ++ ")";
print({minus, A}, P) ->
    par(4 > P, o) ++ "-" ++ print(A, 4) ++ par(4 > P, f);
print({var, A}, P) ->
    par(2 > P, o) ++ A ++ par(2 > P, f);
print(A, _P) when is_number(A) -> A;
print({num, A}, _P) when is_integer(A) ->
    integer_to_list(A);
print({num, A}, _P) ->
    [H] = io_lib:format("~w", [A]),
    H; %% float_to_list(A);
print({const, A}, _P) -> A;
print({op, Op, A, B}, 2) when Op == "^" ->
    par(true, o) ++
      print(A, 2) ++ Op ++ print(B, 2) ++ par(true, f);
print({op, Op, A, B}, P) when Op == "^" ->
    par(2 > P, o) ++
      print(A, 2) ++ Op ++ print(B, 2) ++ par(2 > P, f);
print({op, Op, A, B}, 3)
    when Op == "/"; Op == "rem"; Op == "div" ->
    par(true, o) ++
      print(A, 3) ++
    printOp(Op) ++ print(B, 3) ++ par(true, f);
print({op, Op, A, B}, P)
    when Op == "/"; Op == "rem"; Op == "div" ->
    par(3 > P, o) ++
      print(A, 3) ++
    printOp(Op) ++ print(B, 3) ++ par(3 > P, f);
print({op, Op, A, B}, P) when Op == "*" ->
    par(4 > P, o) ++
      print(A, 4) ++ Op ++ print(B, 4) ++ par(4 > P, f);
print({op, Op, A, B}, P) ->
    par(5 > P, o) ++
      print(A, 5) ++
    printOp(Op) ++ print(B, 5) ++ par(5 > P, f);
print([L], _P) -> print(L);
print([_ | _] = L, _P) -> L;
print({error, _R} = Err, _P) -> Err.

-spec printpar([nonempty_maybe_improper_list() | number() | {'const',_} | {'error',_} | {'minus',nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}} | {'num',_} | {'var',[any()]} | {'func',{_,_},[any(),...]} | {'userfunc',[any()],[any(),...]} | {'op',[any()],nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_},nonempty_maybe_improper_list() | number() | {_,_} | {_,_,_} | {_,_,_,_}},...]) -> any().
printpar([H]) -> print(H);
printpar([H|Q]) -> print(H) ++ " , " ++ printpar(Q).

%% print(A,_P) -> " " ++ io_lib:format("#~p#",[A]) ++ " ". %% never reached

-spec printOp([any()]) -> [any(),...].
printOp([_] = Op) -> Op;
printOp(Op) -> " " ++ Op ++ " ".

-spec par(boolean(),'f' | 'o') -> [print_par()].
par(true, o) -> "(";
par(true, f) -> ")";
par(_, _) -> "".
