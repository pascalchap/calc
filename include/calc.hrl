-include_lib("wx/include/wx.hrl").

%% GUI Id and definitions
-define(QUIT,1).
-define(ABOUT,2).



-define(BIN,50).
-define(OCT,51).
-define(DEC,52).
-define(HEX,53).

-define(CLEARDISP,62).
-define(TST2,63).

-define(ERR,60).

-type button_int() :: 100..146.

-define(ZERO,100).
-define(UN,101).
-define(DEUX,102).
-define(TROIS,103).
-define(QUATRE,104).
-define(CINQ,105).
-define(SIX,106).
-define(SEPT,107).
-define(HUIT,108).
-define(NEUF,109).
-define(POINT,110).
-define(CLEAR,111).
-define(EGAL,112).
-define(RACINE,113).
-define(SIN,114).
-define(COS,115).
-define(TAN,116).
-define(DIVE,117).
-define(DIV,118).
-define(PI,119).
-define(EXP,120).
-define(LOG,121).
-define(DERIV,122).
-define(PARO,123).
-define(PARF,124).
-define(ARRG,125).
-define(MULT,126).
-define(PLUS,127).
-define(MOINS,128).
-define(LOG10,129).
-define(COSH,130).
-define(SINH,131).
-define(TANH,132).
-define(FACT,133).
-define(COMB,134).
-define(REM,135).
-define(ABS,136).
-define(FRAC,137).
-define(INT,138).
-define(ASIN,139).
-define(ASINH,140).
-define(ACOS,141).
-define(ACOSH,142).
-define(ATAN,143).
-define(ATANH,144).
-define(ENTER,145).
-define(RES,146).


-type button_def() :: {button_int(),string(),string()}.

-define(B_ZERO,{?ZERO,"0","0"}).
-define(B_UN,{?UN,"1","1"}).
-define(B_DEUX,{?DEUX,"2","2"}).
-define(B_TROIS,{?TROIS,"3","3"}).
-define(B_QUATRE,{?QUATRE,"4","4"}).
-define(B_CINQ,{?CINQ,"5","5"}).
-define(B_SIX,{?SIX,"6","6"}).
-define(B_SEPT,{?SEPT,"7","7"}).
-define(B_HUIT,{?HUIT,"8","8"}).
-define(B_NEUF,{?NEUF,"9","9"}).
-define(B_POINT,{?POINT,".","."}).
-define(B_CLEAR,{?CLEAR,"C",null}).
-define(B_EGAL,{?EGAL,"="," = "}).
-define(B_RACINE,{?RACINE,"sqrt","sqrt("}).
-define(B_SIN,{?SIN,"sin","sin("}).
-define(B_COS,{?COS,"cos","cos("}).
-define(B_TAN,{?TAN,"tan","tan("}).
-define(B_DIVE,{?DIVE,"div"," div "}).
-define(B_DIV,{?DIV,"/"," / "}).
-define(B_PI,{?PI,"pi"," pi "}).
-define(B_EXP,{?EXP,"^","^"}).
-define(B_LOG,{?LOG,"log","log("}).
-define(B_DERIV,{?DERIV,"f'","drv("}).
-define(B_PARO,{?PARO,"(","("}).
-define(B_PARF,{?PARF,")",")"}).
-define(B_ARRG,{?ARRG,"arrg"," arrg "}).
-define(B_MULT,{?MULT,"*"," * "}).
-define(B_PLUS,{?PLUS,"+"," + "}).
-define(B_MOINS,{?MOINS,"-"," - "}).
-define(B_LOG10,{?LOG10,"log10","log10("}).
-define(B_COSH,{?COSH,"cosh","cosh("}).
-define(B_SINH,{?SINH,"sinh","sinh("}).
-define(B_TANH,{?TANH,"tanh","tanh("}).
-define(B_FACT,{?FACT,"!","fact("}).
-define(B_COMB,{?COMB,"comb"," comb "}).
-define(B_REM,{?REM,"rem"," rem "}).
-define(B_ABS,{?ABS,"abs","abs("}).
-define(B_FRAC,{?FRAC,"frac","frac("}).
-define(B_INT,{?INT,"int","int("}).
-define(B_ACOS,{?ACOS,"acos","acos("}).
-define(B_ACOSH,{?ACOSH,"acosh","acosh("}).
-define(B_ASIN,{?ASIN,"asin","asin("}).
-define(B_ASINH,{?ASINH,"asinh","asinh("}).
-define(B_ATAN,{?ATAN,"atan","atan("}).
-define(B_ATANH,{?ATANH,"atanh","atanh("}).
-define(B_ENTER,{?ENTER,"enter",null}).
-define(B_RES,{?RES," ",null}).

-type split_term() :: {func, {string(), atom()}} | {sep | const, string()} | {num, number()} | {new, string()} .
-type level_term() :: {func, {string(), atom()}} | {sep | const, string()} | {num, number()} | {var, string()} | {userfunc, string()} | assign .
-type level_terms() :: [level_term() | level_terms()].
-type tree_term() :: {op, string(), tree_term(), tree_term()} | {minus, tree_term()} | {func, {string(), atom()}, [tree_term(), ...]} |
                     {userfunc, string(), [tree_term(), ...]} | {num, number()} | {var, string()}.

%% KEYWORDS
-define(KEYWORDS,[
		  {func,{"acos",math}},
		  {func,{"acosh",math}},
		  {sep,"arrg"},
		  {func,{"asin",math}},
		  {func,{"asinh",math}},
		  {func,{"atan",math}},
		  {func,{"atanh",math}},
		  {sep,"comb"},
		  {func,{"cos",math}},
		  {func,{"cosh",math}},
		  {sep,"div"},
		  {func,{"drv",calc}},
		  {const,"e"},
		  {sep,"eq"},
		  {func,{"exp",math}},
		  {func,{"fact",calc}},
		  {sep,"ge"},
		  {sep,"gt"},
		  {sep,"le"},
		  {func,{"log",math}},
		  {func,{"log10",math}},
		  {sep,"lt"},
		  {sep,"neq"},
		  {const,"pi"},
		  {sep,"rem"},
		  {func,{"sin",math}},
		  {func,{"sinh",math}},
		  {func,{"sqrt",math}},
		  {func,{"tan",math}},
		  {func,{"tanh",math}},
		  {func,{"abs",erlang}},
		  {func,{"int",calc}},
		  {func,{"frac",calc}}
		 ]).

%% Pattern definition
-define(NOM,"[A-Za-z][A-Za-z0-9_]*").
-define(SIGN,"[\\(\\)\\+\\-\\*\\/\\^=,]").
-define(NUM,"[0-9]+(\\.[0-9]+)?([eE][\\+-]?[0-9]+)?").
-define(BASE,"[2-9]#[0-8]+|[12][0-9]#[0-9a-sA-S]+|3[0-6]#[0-9a-zA-Z]+").
-define(MATCH,?NOM++"|"++?SIGN++"|"++?BASE++"|"++?NUM).
-define(MS,re:compile(?MATCH)).

-define(TESTBENCH,[
		   {"G(x)",{userfunc,"G",[{var,"x"}]}},
		   {"Fct(x)",{userfunc,"Fct",[{var,"x"}]}},
		   {"123456789",{num,123456789}},
		   {"123456.789",{num,123456.789}},
		   {"-12.5",{minus,{num,12.5}}},
		   {"+56.0e3",{num,56.0e3}},
		   {"+89.002E-5",{num,89.002E-5}},
		   {"16#0123456789ABCDEF",{num,81985529216486895}},
		   {"16#abcdef",{num,11259375}},
		   {"8#12345670",{num,2739128}},
		   {"2#1000101011",{num,555}},
		   {"X=-5",{assign,{var,"X"},{minus,{num,5}}}},
		   {"sin(6)",{func,{"sin",math},[{num,6}]}},
		   {"sin(tan(X))",{func,{"sin",math},[{func,{"tan",math},[{var,"X"}]}]}},
		   {"F(x,y)=sin(x*y)^cos(x+y)",{assign,	{userfunc,"F",[{var,"x"},{var,"y"}]},
						{op,"^",{func,{"sin",math},[{op,"*",{var,"x"},{var,"y"}}]},
						{func,{"cos",math},[{op,"+",{var,"x"},{var,"y"}}]}}}},
		   {"1+2*3^4",{op,"+",{num,1},{op,"*",{num,2},{op,"^",{num,3},{num,4}}}}},
		   {"exp(-2*t)",{func,{"exp",math},[{op,"*",{minus,{num,2}},{var,"t"}}]}},
		   {"8*(-tan(x)+3)",{op,"*",{num,8},{op,"+",{minus,{func,{"tan",math},[{var,"x"}]}},{num,3}}}},
		   {"X=7=Y",{error,"more than 1 ="}},
		   {"sin((3)",{error,"trop de ("}},
		   {"test(5))",{error,"trop de )"}},
		   {"5*/2",[{op,"*",{num,5},{sep,"/"}},{num,2}]},
		   {"fact((6))",{func,{"fact",calc},[{num,6}]}},
		   {"2*3*4/5 div 2",{op,"div",{op,"/",{op,"*",{op,"*",{num,2},{num,3}},{num,4}},{num,5}},{num,2}}},
		   {"F(X) = exp(log(sin(X)^2)*cos(X))",{assign,{userfunc,"F",[{var,"X"}]},
							{func,{"exp",math},[{op,"*",{func,{"log",math},[{op,"^",
							{func,{"sin",math},[{var,"X"}]},{num,2}}]},{func,{"cos",math},[{var,"X"}]}}]}}}
		  ]).
