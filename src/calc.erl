-module(calc).

%% -compile(export_all).

-export([parse_evaluate/2, parse/1,evaluate/1,fact/1,int/1,frac/1,drv/2,
	 derive/2,simplify/1,print/1,getvarfunc/1,testparse/0,test/1]).

-include("../include/calc.hrl").

parse_evaluate(T,Aff) ->
    R = (catch evaluate(parse(T))),
    Rep = case R of
	      {'EXIT',{Reason,_Info}} -> {error,atom_to_list(Reason)};
	      {error,Reason} -> {error,Reason};
	      {assign,{userfunc,N,A},B} -> 
		  case catch(checkuserfunc(B,chekleft(A))) of
		      ok -> storeuserfunc(N,A,B,T);
		      {'EXIT',{{Reason,_},_Info}} -> {error,atom_to_list(Reason)}
		  end;
	      {assign,{var,N},A} -> storevar(N,evaluate(A));
	      R -> R
	  end,
    calc_server:display(Aff,T,Rep).

getvarfunc(Exp) ->
    R = (catch parse(Exp)),
    case R of
	{'EXIT',{Reason,_Info}} -> {error,atom_to_list(Reason)};
	{error,Reason} -> {error,Reason};
	{assign,{userfunc,_N,A},B} ->  
	    {A,B};
	Other -> {getvarlist(Other),Other}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(L) ->
    L1 = split(L),
    L2 = level(L1),
    priorize(L2).

evaluate({minus,A}) -> -evaluate(A);
evaluate({op,"+",A,B}) -> evaluate(A)+evaluate(B);
evaluate({op,"-",A,B}) -> evaluate(A)-evaluate(B);
evaluate({op,"*",A,B}) -> evaluate(A)*evaluate(B);
evaluate({op,"/",A,B}) -> evaluate(A)/evaluate(B);
evaluate({op,"rem",A,B}) -> evaluate(A) rem evaluate(B);
evaluate({op,"div",A,B}) -> evaluate(A) div evaluate(B);
evaluate({op,"arrg",A,B}) -> arrg(evaluate(A),evaluate(B));
evaluate({op,"comb",A,B}) -> comb(evaluate(A),evaluate(B));
evaluate({op,"^",A,B}) -> math:pow(evaluate(A),evaluate(B));
evaluate({func,{N,L},A}) -> apply(L,list_to_atom(N),lists:map(fun(X) -> evaluate(X) end,A));
evaluate({userfunc,N,A}) ->	evaluate(N,A);
evaluate({const,N}) -> getconst(N);
evaluate({var,N}) -> getvalue(N);
evaluate(R= {assign,{userfunc,_N,_A},_B}) -> R;
evaluate(R = {assign,{var,_N},_A}) -> R;
evaluate({num,A}) -> A.

evaluate(F,A) -> evalfunc(getuserfunc(F),A).

getuserfunc(N) -> 
    case calc_store:getfunc(N) of
	{ok,Value} -> Value;
	error -> throw({error,"function undefined"})
    end.
storeuserfunc(Name,Par,Desc,Text) -> 
    Desc1 = simplify(Desc),
    calc_store:storefunc(Name,Par,Desc1,Text),
    io_lib:format("function ~p(~p) = ~p stored",[Name,Par,print(Desc1)]).

getconst("pi") -> math:pi();
getconst("e") -> math:exp(1).

getvalue(N) -> 
    case calc_store:getvalue(N) of
	{ok,Value} -> Value;
	error -> throw({error,"variable undefined"})
    end.
storevar(Name,Value) -> 
    calc_store:storevar(Name,Value),
    io_lib:format("variable ~p = ~p stored",[Name,Value]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% split the string in pieces, identify and translate them

split(T) -> 
    {ok,M} = ?MS,
    {match,L} = re:run(T,M,[global]),
    First = fun(X) when is_list(X) -> hd(X); (X) -> X end,
    L1 = [First(X) || X <- L],
    Conv = fun	(X = [H]) when H=:=$+; H=:=$-; H=:=$*; H=:=$/; H=:=$(; H=:=$); H=:=$^; H=:=$,; H=:=$= -> {sep,X};
		(X = [H|_]) when H >= $A , H =< $Z -> {new,X};
		(X = [H|_]) when H >= $0 , H =< $9; H =:= $- ; H =:= $+ -> {num,getnum(X)};
		(X = [H|_]) when H >= $a , H =< $z -> getsubstitute(X)
	   end,
    [Conv(lists:sublist(T,X+1,Y)) || {X,Y} <- L1].

getsubstitute(X) ->
    F = fun (K={_,{N,_}},_) when N==X-> K;
	    (K={_,N},_) when N==X-> K;
	    (_,A) -> A
	end,
    lists:foldl(F,{new,X},?KEYWORDS).

getnum(X) ->
    case {lists:member($#,X),lists:member($.,X)} of
	{true,false} -> {B1,[_|X1]} = lists:splitwith(fun (E) -> E =/= $# end,X),list_to_integer(X1,list_to_integer(B1));
	{false,true} -> list_to_float(X);
	{false,false} -> list_to_integer(X)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% management of parenthesis

level(L) -> level(L,[],0).

level([],A,0) -> lists:reverse(A);		%% end of the analyse
level([],_A,_I) -> throw({error,"trop de ("});	%% end of the list encountered while analysing a sublevel
level(A,[],0) when is_tuple(A)-> A;	%% isolated term like {num,15} in 16#F
level([{sep,")"}|T],A,1) -> {lists:reverse(A),T}; %% close current level
level([{sep,")"}|_T],_A,0) -> throw({error,"trop de )"}); %% missing corresponding "("
level([{new,N},{sep,"("}|T],A,C) -> 	%% a new name followed by "(" should be a user defined function
    {L,R} = level(T,[],1),			%% analyse sublevel
    level(R,[L,{userfunc,N}|A],C);	%% then continue
level([{sep,"("}|T],A,C) ->				%% at each "("
    {L,R} = level(T,[],1),			%% analyse a new sublevel
    level(R,[L|A],C);					%% then continue
level([{new,N}|T],A,C) -> level(T,[{var,N}|A],C);	%% remaining new name should be variables
level([H|T],A,C) -> level(T,[level(H,[],0)|A],C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% management of operator priority

priorize(L) ->
    L0 = prio_assign(L,[],false),
    L1 = prio_func(L0,[]),
    L2 = prio_power(L1,[]),
    L3 = prio_mult(L2,[]),
    L4 = prio_add(L3,[]),
    totuple(L4).

prio_assign([],L,_) ->
    lists:reverse(L);
prio_assign([{sep,"="}|T],R,false) ->
    Left = lists:reverse(R),
    Right = prio_assign(T,[],true),
    [assign,Left,Right];
prio_assign([{sep,"="}|_T],_R,true) ->
    throw({error,"more than 1 ="});
prio_assign([H|T],R,B) ->
    prio_assign(T,[H|R],B).

%% condition de fin
prio_func([],L) ->
    lists:reverse(L);
prio_func(T,[]) when not(is_list(T)) -> T;
%% pour une fonction, identifier la liste de parametre
prio_func([{M,F},A|T],R) when M == func; M == userfunc ->
    prio_func(T,[[M,F,prio_func(A,[])]|R]);
%% il ne doit plus rester de "new"
prio_func([{new,_H}|_T],_R) ->
    throw({error,"unexpected term"});
prio_func([H|T],R) ->
    prio_func(T,[prio_func(H,[])|R]).

prio_power([],L) ->
    lists:reverse(L);
prio_power(T,[]) when not(is_list(T)) -> T;
prio_power([A,{sep,"^"},B|T],R) ->
    prio_power([[op,"^",prio_power(A,[]),prio_power(B,[])]|T],R);
prio_power([H|T],R) ->
    prio_power(T,[prio_power(H,[])|R]).

prio_mult([],L) ->
    lists:reverse(L);
prio_mult(T,[]) when not(is_list(T)) -> T;
%% les - et + unaire (en début de liste) sont équivalent à une multiplication par + ou - 1
prio_mult([{sep,"-"},A|T],[]) -> prio_mult([[minus,prio_mult(A,[])]|T],[]);
prio_mult([{sep,"+"},A|T],[]) -> prio_mult([prio_mult(A,[])|T],[]);
%% dans un niveau de parenthese donné, on execute ces opération de gauche à droite, dans l'ordre ou on les rencontre
prio_mult([A,{sep,Op},B|T],R) when 	Op == "*";
					Op == "/";
					Op == "div";
					Op == "rem";
					Op == "comb";
					Op == "arrg" ->
    prio_mult([[op,Op,prio_mult(A,[]),prio_mult(B,[])]|T],R);
prio_mult([H|T],R) ->
    prio_mult(T,[prio_mult(H,[])|R]).

prio_add([],L) ->
    lists:reverse(L);
prio_add(T,[]) when not(is_list(T)) -> T;
prio_add([A,{sep,Op},B|T],R) when 		Op == "+";
						Op == "-" ->
    prio_add([[op,Op,prio_add(A,[]),prio_add(B,[])]|T],R);
prio_add([{sep,","}|T],A) -> prio_add(T,A);
prio_add([H|T],R) ->
    prio_add(T,[prio_add(H,[])|R]).

totuple(X) when not(is_list(X)) -> X;
totuple([X]) when not(is_integer(X)) -> totuple(X);
totuple([minus,A]) -> {minus,totuple(A)};
totuple([assign,A,B]) -> {assign,totuple(A),totuple(B)};
totuple([func,A,L]) -> {func,totuple(A),totuple(L,[])};
totuple([userfunc,A,L]) -> {userfunc,totuple(A),totuple(L,[])};
totuple([var,A]) -> {var,totuple(A)};
totuple([op,N,A,B]) -> {op,N,totuple(A),totuple(B)};
totuple([num,N]) -> {num,N};
totuple([const,N]) -> {const,N};
totuple(L) -> totuple(L,[]).

totuple([],A) -> lists:reverse(A);
totuple([H|T],A) -> totuple(T,[totuple(H)|A]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drv({userfunc,N,A},[{var,X}]) ->
    derive(evaluate(N,A),X);
drv(F,[{var,X}]) ->
    derive(F,X).

fact(A) when is_integer(A), A >= 0 ->
    fact(A,1);
fact(A) ->
    Err = io_lib:format("evaluation error: fact(~p)~n",[A]),
    throw({error,Err}),
    ok.

fact(0,R) -> R;
fact(A,R) -> fact(A-1,A*R).

comb(N,K) when is_integer(N), is_integer(K), N>=K, K >=1 ->
    comb2(K,arrg(N,N-K+1,1));
comb(N,K) ->
    Err = io_lib:format("evaluation error: ~p comb ~p~n",[N,K]),
    throw({evaluation_error,Err}),
    ok.

arrg(N,K) when is_integer(N), is_integer(K), N>=K, K >=1 ->
    arrg(N,N-K+1,1);
arrg(N,K) ->
    Err = io_lib:format("error: ~p arrg ~p~n",[N,K]),
    throw({evaluation_error,Err}),
    ok.

arrg(M,N,T) when N > M -> T;
arrg(M,N,T) -> arrg(M-1,N,T*M).

comb2(1,T) -> T;
comb2(K,T) -> comb2(K-1,T div K).	

int(N) -> round(N-0.5).

frac(N) -> N-int(N).

evalfunc({Par,Desc,_Text},A) -> evaluate(replace(Desc,Par,A)).

replace({var,N},[{var,N}|_Rn],[O|_Ro]) -> O;
replace({var,_}=N,[_N|Rn],[_O|Ro]) -> replace(N,Rn,Ro);
replace({minus,B},N,O) -> {minus,replace(B,N,O)};
replace({op,Op,B1,B2},N,O) -> {op,Op,replace(B1,N,O),replace(B2,N,O)};
replace({Tf,F,P},N,O) -> {Tf,F,lists:map(fun(X) -> replace(X,N,O) end,P )};
replace(B,_N,_O) -> B.


%% A must be a list of {var,N} not empty
chekleft(A) -> 
    [_|_] = lists:foldl(fun (X,Acc) -> {var,N} = X, [N|Acc] end, [],A).

%% The function definition must only include variables defined in Listvar 
checkuserfunc(Def,Listvar) ->
    All = lists:sort(Listvar),
    Part = getvarlist(Def),
%% Check inclusion, lists:substract should be OK because lists must be very small.
	Rest = All -- Part,
	Part = All -- Rest,
    ok.

getvarlist(S) -> lists:usort(getvarlist(S,[])).

getvarlist({var,N},Acc) -> 
    [N|Acc];
getvarlist({minus,B},Acc) -> getvarlist(B,Acc);
getvarlist({op,_Op,B1,B2},Acc) ->
    NewAcc = getvarlist(B1,Acc),
    getvarlist(B2,NewAcc);
getvarlist({_Tf,_F,P},Acc) -> lists:foldl(fun(X,A) -> getvarlist(X,A) end,Acc,P);
getvarlist(_B,Acc) -> Acc.

derive({num,_},_) -> {num,0};
derive({const,_},_) -> {num,0};
derive({var,N},N) -> {num,1};
derive({var,_},_) -> {num,0};
derive({minus,A},V) -> {minus,derive(A,V)}; 
derive({op,"+",A,B},V) -> {op,"+",derive(A,V),derive(B,V)}; 
derive({op,"-",A,B},V) -> {op,"-",derive(A,V),derive(B,V)}; 
derive({op,"*",A,B},V) -> {op,"+",{op,"*",derive(A,V),B},{op,"*",A,derive(B,V)}}; 
derive({op,"/",A,B},V) -> {op,"/",{op,"-",{op,"*",derive(A,V),B},{op,"*",A,derive(B,V)}},
			   {op,"^",B,{num,2}}};
derive({op,"^",{var,V},{num,N}},V) -> {op,"*",{num,N},{op,"^",{var,V},{num,N-1}}};
derive({op,"^",{var,_},{num,_N}},_V) -> {num,0};
derive({op,"^",{var,V},A},V) -> {op,"*",
				 {op,"+",{op,"*",derive(A,V),{func,{"log",math},[{var,V}]}},{op,"/",A,{var,V}}},
				 {op,"^",{var,V},A}};
derive({op,"^",A,B},V) -> {op,"*",
			   {op,"+",{op,"*",derive(B,V),{func,{"log",math},[A]}},
			    {op,"/",{op,"*",B,derive(A,V)},A}},
			   {op,"^",A,B}};
derive({userfunc,N,A},V) -> {Sa,D,_} = getuserfunc(N), derive(replace(D,Sa,A),V);
derive({func,{N,_},[{var,V}]},V) -> getderive(N,{var,V});
derive({func,{N,_},[A]},V) -> {op,"*",derive(A,V),getderive(N,A)}.

getderive("sin",A) -> {func,{"cos",math},[A]};
getderive("cos",A) -> {minus,{func,{"sin",math},[A]}};
getderive("sinh",A) -> {func,{"cosh",math},[A]};
getderive("cosh",A) -> {func,{"sinh",math},[A]};
getderive("tan",A) -> {op,"/",{num,1},{op,"^",{func,{"cos",math},[A]},{num,2}}};
getderive("tanh",A) -> {op,"/",{num,1},{op,"^",{func,{"cosh",math},[A]},{num,2}}};
getderive("exp",A) -> {func,{"exp",math},[A]};
getderive("log",A) -> {op,"/",{num,1},A};
getderive("log10",A) -> {op,"/",{num,1},{op,"*",{num,math:log(10)},[A]}};
getderive("acos",A) -> {minus,{op,"^",{op,"-",{num,1},{op,"^",A,{num,2}}},{num,-0.5}}};
getderive("asin",A) -> {op,"^",{op,"-",{num,1},{op,"^",A,{num,2}}},{num,-0.5}};
getderive("atan",A) -> {op,"/",{num,1},{op,"+",{num,1},{op,"^",A,{num,2}}}};
getderive("acosh",A) -> {op,"^",{op,"-",{op,"^",A,{num,2}},{num,1}},{num,-0.5}};
getderive("asinh",A) -> {op,"^",{op,"+",{num,1},{op,"^",A,{num,2}}},{num,-0.5}};
getderive("atanh",A) -> {op,"/",{num,1},{op,"-",{num,1},{op,"^",A,{num,2}}}};
getderive("sqrt",A) -> {op,"*",{num,0.5},{op,"^",A,{num,-0.5}}}.

simplify({func,{"drv",_},[F,{var,X}]}) -> simplify(derive(F,X));
simplify({func,N,[A]}) -> {func,N,[simplify(A)]};
simplify({userfunc,N,A}) -> {Sa,D,_} = getuserfunc(N), simplify(replace(D,Sa,A));
simplify({minus,A}) -> reduce({minus,simplify(A)});
simplify({op,Op,A,B}) -> reduce({op,Op,simplify(A),simplify(B)});
simplify({assign,A,B}) -> {assign,A,reduce(simplify(B))};
simplify(A) -> reduce(A).

%% reduce(Exp) return equivalent expression with some simplification that should lighten the printing
%% -.- = + and similar rules
reduce({minus,{minus,A}}) -> reduce(A);
reduce({minus,{num,N}}) -> {num,-N};
reduce({minus,A}) -> {minus,reduce(A)};
reduce({op,"-",A,{minus,B}}) -> reduce({op,"+",A,B});
reduce({op,"+",A,{minus,B}}) -> reduce({op,"-",A,B});
reduce({op,Op,{minus,A},{minus,B}}) when Op == "*"; Op == "/"  -> reduce({op,Op,A,B});
reduce({op,Op,A,{minus,B}}) when Op == "*"; Op == "/"  -> {minus,reduce({op,Op,A,B})};
reduce({op,Op,{minus,A},B}) when Op == "*"; Op == "/"  -> {minus,reduce({op,Op,A,B})};
%% try to gather the numerical value to the left of op when it is possible
reduce({op,_Op,{num,_N1},{num,_N2}}=A) -> {num,evaluate(A)};
reduce({op,Op,A,{num,N}}) when Op == "+" ; Op == "*" -> reduce({op,Op,{num,N},A});
reduce({op,"-",A,{num,N}}) -> reduce({op,"+",{num,-N},A});
reduce({op,"/",A,{num,N}}) -> reduce({op,"*",{num,1/N},A});
%% simplify multiple qotient, trying to wite a ratio of products
reduce({op,"/",A,{op,"/",B,C}}) -> reduce({op,"/",{op,"*",A,C},B});
reduce({op,"/",{op,"/",A,B},C}) -> reduce({op,"/",A,{op,"*",C,B}});
%% trivial numerical expression (not really accurate regarding fundamental maths)
reduce({op,"*",{num,N},_A}) when N == 0 -> {num,0};
reduce({op,"+",{num,N},A}) when N == 0 -> A;
reduce({op,"-",{num,N},A}) when N == 0 -> {minus,A};
reduce({op,"/",{num,N},_A}) when N == 0 -> {num,0};
reduce({op,"^",{num,N},_A}) when N == 0 -> {num,0};
reduce({op,"^",_A,{num,N}}) when N == 0 -> {num,1};
reduce({op,"*",{num,N},A}) when N == 1 -> A;
reduce({op,"^",{num,N},_A}) when N == 1 -> {num,1};
reduce({op,"^",A,{num,N}}) when N == 1 -> A;
%% operation with 2 numbers
reduce({op,"*",{num,N1},{num,N2}}) -> {num,N1*N2};
reduce({op,"/",{num,N1},{num,N2}}) when is_float(N1); is_float(N2)-> {num,N1/N2};
reduce({op,"/",{num,N1},{num,N2}}) when (N1 rem N2) == 0 -> {num,N1 div N2};
reduce({op,"+",{num,N1},{num,N2}}) -> {num,N1+N2};
reduce({op,"-",{num,N1},{num,N2}}) -> {num,N1-N2};
reduce({op,"^",{num,N1},{num,N2}}) -> {num,pow(N1,N2)};
%% recognize some other patterns
reduce({op,"*",A,A}) -> {op,"^",reduce(A),{num,2}};
reduce({op,"/",A,A}) -> {num,1};
reduce({op,"*",A,{op,"^",A,B}}) -> {op,"^",reduce(A),reduce({op,"+",{num,1},B})};
reduce({op,"*",{op,"^",A,B},A}) -> {op,"^",reduce(A),reduce({op,"+",{num,1},B})};
reduce({op,"/",A,{op,"^",A,B}}) -> {op,"^",reduce(A),reduce({op,"-",{num,1},B})};
reduce({op,"/",{op,"^",A,B},A}) -> {op,"^",reduce(A),reduce({op,"-",B,{num,1}})};
reduce({op,"*",{num,N1},{op,Op,{num,N2},A}}) when Op == "*"; Op == "/" ->
    reduce({op,Op,{num,N1*N2},A});
reduce({op,"/",{op,Op,{num,N2},A},{num,N1}}) when Op == "*"; Op == "/" ->
    reduce({op,Op,{num,N1/N2},A});
reduce({op,Op,{op,Op,A,B},{op,Op,C,D}}) when Op == "+"; Op == "*" ->
    reduce({op,Op,A,{op,Op,B,{op,Op,C,D}}});
reduce({op,Op,A,{op,Op,{num,N},B}}) when Op == "+"; Op == "*" -> reduce({op,Op,{num,N},{op,Op,A,B}});
reduce({op,Op,A,B}) -> {op,Op,reduce(A),reduce(B)};
reduce(A) -> A.

pow(_,0) -> 1;
pow(X,N) when is_integer(N), N>0, is_integer(X) ->
    pow(X,N,1);
pow(X,N) -> math:pow(X,N).

pow(_,0,R) -> R;
pow(X,N,R) -> pow(X,N-1,X*R).

print(A) -> print(A,10).

%% priority level
%% 0 top
%% 1 func, userfunc 
%% 2 ^
%% 3 / rem div
%% 4 minus *
%% 5 + -

print({func,{N,_},[A]},_P) -> N ++ "(" ++ print(A, 5) ++ ")";
print({minus,A},P) -> par(4>P,o) ++ "-" ++ print(A,4) ++ par(4>P,f);
print({var,A},P) -> par(2>P,o) ++ A ++ par(2>P,f);
print({num,A},_P) when is_integer(A) -> integer_to_list(A);
print({num,A},_P) -> [H] = io_lib:format("~w",[A]), H; %% float_to_list(A);
print({const,A},_P) -> A;
print({op,Op,A,B},2) when Op == "^" -> par(true,o) ++ print(A,2) ++ Op ++ print(B,2) ++ par(true,f);
print({op,Op,A,B},P) when Op == "^" -> par(2>P,o) ++ print(A,2) ++ Op ++ print(B,2) ++ par(2>P,f);
print({op,Op,A,B},3) when Op == "/"; Op == "rem"; Op == "div" -> 
    par(true,o) ++ print(A,3) ++ Op ++ print(B,3) ++ par(true,f);
print({op,Op,A,B},P) when Op == "/"; Op == "rem"; Op == "div" -> 
    par(3>P,o) ++ print(A,3) ++ Op ++ print(B,3) ++ par(3>P,f);
print({op,Op,A,B},P) when Op == "*" -> 
    par(4>P,o) ++ print(A,4) ++ Op ++ print(B,4) ++ par(4>P,f);
print({op,Op,A,B},P) -> par(5>P,o) ++ print(A,5) ++ Op ++ print(B,5) ++ par(5>P,f);
print(A,_P) -> " " ++ io_lib:format("#~p#",[A]) ++ " ".

par(true,o) -> "(";
par(true,f) -> ")";
par(_,_) -> "".

testparse() ->
    F = fun(X) -> 
		{Test,Expect} = X,
		case catch(parse(Test)) of
		    Expect -> ok;
		    Other -> io:format("test ~p --> ~p~n",[Test,Other]), ko
		end
	end,
    Res = lists:map(F,?TESTBENCH),
    lists:foldl(fun (ok,Acc) -> Acc;(_,_) -> ko end,ok,Res).
test(G) ->
    F = fun(X) -> 
		{Test,_Expect} = X,  
		io:format("~p -->~n    ~p~n",[Test,catch(G(Test))]) 
	end,
    lists:map(F,?TESTBENCH).

