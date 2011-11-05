%%
-module(calc_save).


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse/1,evaluate/1,print/1,parse_evaluate/2]).

%%
%% API Functions
%%

parse(L) ->
		S = split(L,[],new,0),
		io:format("split result: ~p~n",[S]),
		S1 = deep_reverse(analyse(S,[]),[]),
		io:format("analyse result: ~p~n",[S1]),
		S2 = implicit(S1,[]),
		io:format("implicit result: ~p~n",[S2]),
		S3 = tupleize(S2),
		io:format("tupleize result: ~p~n",[S3]),
		S3.

evaluate({A,plus,B}) ->
	evaluate(A) + evaluate(B);
evaluate({A,minus,B}) ->
	evaluate(A) - evaluate(B);
evaluate({minus,unary,A}) ->
	- evaluate(A);
evaluate({A,mult,B}) ->
	evaluate(A) * evaluate(B);
evaluate({A,divide,B}) ->
	evaluate(A) / evaluate(B);
evaluate({A,power,B}) ->
	math:pow(evaluate(A),evaluate(B));
evaluate({A,dive,B}) ->
	evaluate(A) div evaluate(B);
evaluate({A,reme,B}) ->
	evaluate(A) rem evaluate(B);
evaluate({A,comb,B}) ->
	comb(evaluate(A),evaluate(B));
evaluate({A,arrg,B}) ->
	arrg(evaluate(A),evaluate(B));
evaluate({{math,fact},A})->
	fact(evaluate(A));
evaluate({{math,F},A}) ->
	apply(math,F,[evaluate(A)]);
evaluate({const,pi}) ->
	math:pi();
evaluate({num,N}) ->
	N;
evaluate(E) ->
	Rep = lists:flatten(io_lib:format("cannot evaluate~p~n",[E])),
	io:format(Rep),
	throw({evaluate_error,Rep}),
	ok.


%%
%% Local Functions
%%

%% split(term,rep) -> [[unary_op], term, op,term], [[unary_op],term] 
%%                 [-] a + b , [-] a
%% split identifie et sépare les elements entres par l'utilisateur et
%% fourni une liste image de l'entree.
   
split([],R,_,0)						-> lists:reverse(R);
split([$(|T],R,_,I)				-> split(T,[open|R],new,I+1);
split([$)|T],R,_,I) when I>0		-> split(T,[close|R],cont,I-1);
split([$+|T],R,new,I) 				-> split(T,R,cont,I);
split([$+|T],R,cont,I)				-> split(T,[{binary,plus}|R],cont,I);
split([$-|T],R,new,I) 				-> split(T,[minus|R],cont,I);
split([$-|T],R,cont,I)				-> split(T,[{binary,minus}|R],cont,I);
split([$*|T],R,_,I)				-> split(T,[{binary,mult}|R],new,I);
split([$/|T],R,_,I)				-> split(T,[{binary,divide}|R],new,I);
split([$^|T],R,_,I)				-> split(T,[{binary,power}|R],new,I);
split([$=|T],R,_,I)				-> split(T,[{binary,equal}|R],new,I);
split([$d,$i,$v|T],R,_,I)			-> split(T,[{binary,dive}|R],cont,I);
split([$r,$e,$m|T],R,_,I)			-> split(T,[{binary,reme}|R],cont,I);
split([$c,$o,$m,$b|T],R,_,I)		-> split(T,[{binary,comb}|R],cont,I);
split([$a,$r,$r,$g|T],R,_,I)		-> split(T,[{binary,arrg}|R],cont,I);
split([$ |T],R,S,I) 				-> split(T,R,S,I);
split([$s,$q,$r,$t|T],R,_,I)		-> split(T,[{func,sqrt}|R],cont,I);
split([$s,$i,$n,$h|T],R,_,I)		-> split(T,[{func,sinh}|R],cont,I);
split([$a,$s,$i,$n,$h|T],R,_,I)	-> split(T,[{func,asinh}|R],cont,I);
split([$c,$o,$s,$h|T],R,_,I)		-> split(T,[{func,cosh}|R],cont,I);
split([$a,$c,$o,$s,$h|T],R,_,I)	-> split(T,[{func,acosh}|R],cont,I);
split([$t,$a,$n,$h|T],R,_,I)		-> split(T,[{func,tanh}|R],cont,I);
split([$a,$t,$a,$n,$h|T],R,_,I)	-> split(T,[{func,atanh}|R],cont,I);
split([$s,$i,$n|T],R,_,I)			-> split(T,[{func,sin}|R],cont,I);
split([$a,$s,$i,$n|T],R,_,I)		-> split(T,[{func,asin}|R],cont,I);
split([$c,$o,$s|T],R,_,I)			-> split(T,[{func,cos}|R],cont,I);
split([$a,$c,$o,$s|T],R,_,I)		-> split(T,[{func,acos}|R],cont,I);
split([$t,$a,$n|T],R,_,I)			-> split(T,[{func,tan}|R],cont,I);
split([$a,$t,$a,$n|T],R,_,I)		-> split(T,[{func,atan}|R],cont,I);
split([$e,$x,$p|T],R,_,I)			-> split(T,[{func,exp}|R],cont,I);
split([$l,$o,$g|T],R,_,I)			-> split(T,[{func,log10}|R],cont,I);
split([$l,$n|T],R,_,I)				-> split(T,[{func,log}|R],cont,I);
split([$f,$a,$c,$t|T],R,_,I)		-> split(T,[{func,fact}|R],cont,I);
split([$p,$i,$#|T],R,_,I)			-> split(T,[{const,pi}|R],cont,I);
split([$1,$6,$#|T],R,_,I)			-> {Int,End,Type} = readhex(T,0),
							   split(End,[{Type,Int}|R],cont,I);
split([$8,$#|T],R,_,I)		-> {Int,End,Type} = readoct(T,0),
							   split(End,[{Type,Int}|R],cont,I);
split([$2,$#|T],R,_,I)		-> {Int,End,Type} = readbin(T,0),
							   split(End,[{Type,Int}|R],cont,I);
split(L=[H|_T],R,_,I) when H >= $0, H =< $9 ->
							   {Int,End,Type} = readnum(L,0),
							   split(End,[{Type,Int}|R],cont,I);
split(L=[H|_T],R,_,I) when H >= $A, H =< $Z ->
							   {V,End} = readvar(L,[]),
							   split(End,[{var,V}|R],cont,I);
split(L,R,_,_) 						-> 
	Rep = lists:flatten("parse error: " ++ print(lists:reverse(R)) ++ "-->" ++ L ++ "\n"),
	io:format(Rep),
	throw({parse_error,Rep}),
	ok.

%% format supportés:
%% integer	1234567890
%% Hexa		16#123456789AaBbCcDdEeFf0
%% Octal	8#1234567
%% binaire	2#10
%% float	[-]1234567890.1234567890[Ee[-]0123456789]
readnum([H|T],I) when H >= $0, H =< $9  ->
	readnum(T,10*I+H-$0);
readnum([$.|T],I) ->
	readfrac(T,I,0,0);
readnum(L,I) ->
	{I,L,num}.

readfrac([H|T],E,I,R) when H >= $0, H =< $9  ->
	readfrac(T,E,10*I+H-$0,R+1);
readfrac([$E|T],E,I,R)  ->
	readexp(T,E,I,R,0,nosign);
readfrac([$e|T],E,I,R)  ->
	readexp(T,E,I,R,0,nosign);
readfrac(L,E,I,R) ->
	{makefloat(E,I,R,0),L,num}.

readexp([H|T],E,I,R,D,nosign) when H >= $0, H =< $9  ->
	readexp(T,E,I,R,10*D+H-$0,plus);
readexp([$-|T],E,I,R,0,nosign)  ->
	readexp(T,E,I,R,0,minus);
readexp([H|T],E,I,R,D,S) when H >= $0, H =< $9  ->
	readexp(T,E,I,R,10*D+H-$0,S);
readexp(L,E,I,R,D,plus) ->
	{makefloat(E,I,R,D),L,num};
readexp(L,E,I,R,D,minus) ->
	{makefloat(E,I,R,-D),L,num}.



%% E = entier
%% F = fraction
%% R = range - pour la partie fractionaire
%% D = puissance de 10
makefloat(E,F,R,D) ->
	(E + F * math:pow(10,-R))*math:pow(10,D).

readhex([H|T],I) when H >= $0, H =< $9 ->
	readhex(T,16*I+H-$0);
readhex([H|T],I) when H >= $a, H =< $f ->
	readhex(T,16*I+H-$a+10);
readhex([H|T],I) when H >= $A, H =< $F ->
	readhex(T,16*I+H-$A+10);
readhex(L,I) ->
	{I,L,num}.

readoct([H|T],I) when H >= $0, H =< $7  ->
	readoct(T,8*I+H-$0);
readoct(L,I) ->
	{I,L,num}.

readbin([H|T],I) when H == $0;
				      H == $1  ->
	readbin(T,2*I+H-$0);
readbin(L,I) ->
	{I,L,num}.



readvar([H|T],I) when H >= $0, H =< $9;
					  H >= $a, H =< $z;
					  H >= $A, H =< $Z;
					  H =:= $_   ->
	readvar(T,[H|I]);
readvar(L,I) ->
	{lists:reverse(I),L}.

%% analyse l'imbrication des parentheses et les operateurs unaires

analyse([],R) ->
	R;
	%	lists:reverse(R);
analyse([open|T],R) ->
	{P,Rest} = analyse(T,[]),
	analyse(Rest,[P|R]);
analyse([close|T],R) ->
	{R,T};
analyse([minus|T],[]) ->
	analyse(T,[unary,minus]);
%analyse([plus|T],[]) ->
%	analyse(T,[]);
analyse([H|T],R) ->
	analyse(T,[H|R]).

deep_reverse([],L) ->
	L;
deep_reverse([H|T],A) when is_list(H) ->
	deep_reverse(T,[deep_reverse(H,[])|A]);
deep_reverse([H2,H1 = {unary,_}|T],A) ->
	deep_reverse(T,[[H1,H2]|A]);
deep_reverse([H|T],A) ->
	deep_reverse(T,[H|A]).

%% gestion de la preseance des operateurs
	
implicit([],L) ->
	lists:reverse(L);
implicit(E,[]) when not(is_list(E)) ->
	E;
implicit(L,R) ->
%	io:format("enter implicit with :~n~p~n~p~n",[L,R]),
	[H|T] = priorize(L),	
	implicit(T,[implicit(H,[])|R]).

priorize(L) ->
	L1 = prio_func(L,[]),
	L2 = prio_power(L1,[]),
	L3 = prio_mult(L2,[]),
	prio_add(L3,[]).

prio_func([],L) ->
	lists:reverse(L);
prio_func([{func,F},A|T],R) ->
	prio_func([[{math,F},A]|T],R);
prio_func([{var,F},A|T],R) ->
	prio_func([[{userfunc,F},A]|T],R);
prio_func([H|T],R) ->
	prio_func(T,[H|R]).

prio_power([],L) ->
	lists:reverse(L);
prio_power([A,{binary,power},B|T],R) ->
	prio_power([[A,power,B]|T],R);
prio_power([H|T],R) ->
	prio_power(T,[H|R]).

prio_mult([],L) ->
	lists:reverse(L);
prio_mult([A,{binary,Op},B|T],R) when 	Op == mult;
												Op == divide;
												Op == dive;
												Op == reme;
												Op == comb;
												Op == arrg ->
	prio_mult([[A,Op,B]|T],R);
prio_mult([H|T],R) ->
	prio_mult(T,[H|R]).

prio_add([],L) ->
	lists:reverse(L);
prio_add([A,{binary,Op},B|T],R) when Op == plus;
									 Op == minus->
	prio_add([[A,Op,B]|T],R);
prio_add([H|T],R) ->
	prio_add(T,[H|R]).


%% restitution sous forme de tuple
%% {minus,T} , {op,Operateur,T1,T2}
%% {num,Nombre}, {const,ConstName}
%% {func,FuncName}, {userfunc,FuncName},
%% {var,VarName},
%% {assign,VarName,T},{assign,UserfuncName,T}

tupleize([A]) ->
	tupleize(A);
tupleize([A,B]) ->
	{tupleize(A),tupleize(B)};
tupleize([A,B,C]) ->
	{tupleize(A),tupleize(B),tupleize(C)};
tupleize(E) ->
	E.

fact(A) when is_integer(A), A >= 0 ->
	fact(A,1);
fact(A) ->
	Err = io_lib:format("evaluation error: fact(~p)~n",[A]),
	throw({evaluation_error,Err}),
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
	Err = io_lib:format("evaluation error: ~p arrg ~p~n",[N,K]),
	throw({evaluation_error,Err}),
	ok.
	
arrg(M,N,T) when N > M -> T;
arrg(M,N,T) -> arrg(M-1,N,T*M).

comb2(1,T) -> T;
comb2(K,T) -> comb2(K-1,T div K).	
	
print([]) -> [];
print([{func,F}|R]) -> atom_to_list(F)++print(R);
print([open|R]) -> "("++print(R);
print([close|R]) -> ")"++print(R);
print([{num,N}|R]) -> io_lib:format("~p",[N])++print(R);
print([{const,N}|R]) -> io_lib:format("~p",[N])++print(R);
print([{var,V}|R]) -> " "++V++" "++print(R);
print([{binary,plus}|R]) -> " + "++print(R);
print([{binary,minus}|R]) -> " - "++print(R);
print([{binary,mult}|R]) -> " * "++print(R);
print([{binary,divide}|R]) -> " / "++print(R);
print([{binary,power}|R]) -> " ^ "++print(R);
print([{binary,dive}|R]) -> " div "++print(R);
print([{binary,reme}|R]) -> " rem "++print(R);
print([{binary,comb}|R]) -> " comb "++print(R);
print([{binary,arrg}|R]) -> " arrg "++print(R);
print([{binary,equal}|R]) -> "="++print(R);
print([minus|R]) -> "-"++print(R).

parse_evaluate(T,Aff) ->
	io:format("enter parse_evaluate (~p,~p)~n",[T,Aff]),
	Rep = try
		Parsed = parse(T),
		evaluate(Parsed)
	catch
		error:badarith ->
			{error,"arithmetic error"};
		throw:Type ->
			{_,Err} = Type,
			{error,Err}
	end,
	calc_server:display(Aff,T,Rep).
