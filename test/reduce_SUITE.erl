-module(reduce_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(simplify, Config) ->
%% check that cac_store is not started on this VM
%% if calc_store is started, the if statement will crash and the test case skipped
	Started = whereis(calc_store),
	io:format("################ Whereis(calc_store) = ~p~n",[Started]),
	if ( Started == undefined) ->
		calc_store:start_link()
		%% When this test case will finish the link property will kill the calc_store process
	end,
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
%% end_per_testcase(simplify, _Config) ->
%%	{'EXIT',ok,shutdown} ! calc_store;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [evaluationok,evaluationerror,simplify].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
evaluationok() -> [].
evaluationerror() -> [].
simplify() -> [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
evaluationok(_Config) ->
	?line 12 = calc:parse_evaluate("6+8+log(e^2)-4"),
	?line 13 = calc:parse_evaluate("5+3*4/6*2+(-2+12) div 5 + 23 rem 7"),
	?line 2.5 = calc:parse_evaluate("10/4"),
	?line 0 = calc:parse_evaluate("sin(0)"),
	?line R1 = (15+16*(14+16*(13+16*(12+16*(11+16*10))))),
	?line R1 = calc:parse_evaluate("16#abcdef"),
	?line -1 = calc:parse_evaluate("1-(+ 4/2)"),
	?line 120 = calc:parse_evaluate("fact(5)"),
	?line 220 = calc:parse_evaluate("12 comb 3"),
	?line 1320 = calc:parse_evaluate("12 arrg 3"),
	?line -1 = calc:parse_evaluate("cos(pi)"),
	?line 0 = calc:parse_evaluate("exp(1) - e"),
	?line 0 = calc:parse_evaluate("tan(0)"),
%% for frac test use only numbers that have an exact representation in floating point coding
	?line 0 = calc:parse_evaluate("frac(2.25)-frac(4.25)"),
	?line 2 = calc:parse_evaluate("int(2.44)"),
    ok.
evaluationerror(_Config) ->
	?line {error,"badarith"} = (catch(calc:parse_evaluate("log(0)"))),
	?line {error,"trop de ("} = (catch(calc:parse_evaluate("3*(5+2"))),
	?line {error,"trop de )"} = (catch(calc:parse_evaluate("3*(5+2))"))),
	?line {error,"more than 1 ="} = (catch(calc:parse_evaluate("x = 12 = 12"))),
	?line {error,"function_clause"} = (catch(calc:parse_evaluate("2 + 4*(X X)"))),
	?line {error,"badmatch"} = calc:parse_evaluate("f(x) = 2*z"),
	?line {error,[$e,$v,$a,$l,$u,$a,$t,$i,$o,$n,$ ,$e,$r,$r,$o,$r|_]} = (catch(calc:parse_evaluate("fact(1.3)"))),
	?line {error,[$e,$v,$a,$l,$u,$a,$t,$i,$o,$n,$ ,$e,$r,$r,$o,$r|_]} = (catch(calc:parse_evaluate("2.5 comb 3"))),
	?line {error,[$e,$v,$a,$l,$u,$a,$t,$i,$o,$n,$ ,$e,$r,$r,$o,$r|_]} = (catch(calc:parse_evaluate("8 arrg 9"))),
    ok.
simplify(_Config) ->
	?line "2+16*log(x)" = calc:parse_evaluate("6+8*log(x)*2-4"),
	?line "cos(x)" = calc:parse_evaluate("drv(sin(x),x)"),
	calc:parse_evaluate("Y=12"),
	?line 12 = calc:parse_evaluate("Y"),
	calc:parse_evaluate("f(x) = 2*x"),
	?line 24 = calc:parse_evaluate("f(Y)"),
	?line "-2*u" = calc:parse_evaluate("f(-u)"),
	?line "-2*x" = calc:parse_evaluate("-x*2"),
	?line "x/y" = calc:parse_evaluate("-x/(-y)"),
	?line "x+y" = calc:parse_evaluate("x-(-y)"),
	?line "-x/y" = calc:parse_evaluate("-x/y"),
	?line "(x*z)/y" = calc:parse_evaluate("x/(y/z)"),
	?line "x/(z*y)" = calc:parse_evaluate("x/y/z"),
	?line "x" = calc:parse_evaluate("-x/(3-4)"),
	?line "4/x" = calc:parse_evaluate("12/x/3"),
	?line "12/(5*x)" = calc:parse_evaluate("12/x/5"),
	?line "6.4/x" = calc:parse_evaluate("16/x/2.5"),
	?line "6*x" = calc:parse_evaluate("2*3*x"),
	?line "2*x" = calc:parse_evaluate("(4*x)/2"),
	?line "6*x*y" = calc:parse_evaluate("(2*x)*(y*3)"),
	?line "1+x" = calc:parse_evaluate("x + 4 rem 3"),
	?line "2/3*x" = calc:parse_evaluate("2/3*x"),
	?line "8*x" = calc:parse_evaluate("2^3*x"),
	?line "0.25*x" = calc:parse_evaluate("0.5^2*x"),
	?line "x" = calc:parse_evaluate("0.5^0*x"),
	?line "2.0*x" = calc:parse_evaluate("x/0.5"),
	?line "-x" = calc:parse_evaluate("0-x"),
	?line "0" = calc:parse_evaluate("0/x"),
	?line "0" = calc:parse_evaluate("0^x"),
	?line "1" = calc:parse_evaluate("x^0"),
	?line "1" = calc:parse_evaluate("1^x"),
	?line "x^2" = calc:parse_evaluate("x*x"),
	?line "x^3" = calc:parse_evaluate("x*x^2"),
	?line "x^3" = calc:parse_evaluate("x^2*x"),
	?line "x" = calc:parse_evaluate("x^2/x"),
	?line "x^-1" = calc:parse_evaluate("x/x^2"),
	?line "(x^2)^(1+x)" = calc:parse_evaluate("(x^2)^(1+x)"),
	?line "(x div y)/z" = calc:parse_evaluate("(x div y) / z"),
	?line "3+2*u" = calc:parse_evaluate("drv(u-f(-u)+u^2,u)"),
	?line "4*u" = calc:parse_evaluate("drv(f(u^2),u)"),
	calc:parse_evaluate("h(x) = -x+3"),
	?line "3-u^2" = calc:parse_evaluate("h(u^2)"),
	?line "pi*sin(pi*x)" = calc:parse_evaluate("drv(-cos(pi*x),x)"),
	?line {error,"function undefined"} = (catch(calc:parse_evaluate("g(2)"))),
	?line calc:parse_evaluate("g(x,y) = tan(x)*exp(y)"),
	?line "tan(x)*exp(y)"=calc:parse_evaluate("drv(g(x,y),y)"),
	?line "tan(u)*exp(-v)"=calc:parse_evaluate("g(u,-v)"),
	?line "1" = calc:parse_evaluate("drv(x+5,x)"),
	?line "-1" = calc:parse_evaluate("drv(5-x,x)"),
	?line "-1/x^2" = calc:parse_evaluate("drv(1/x,x)"),
	?line "2.5*x^1.5"=calc:parse_evaluate("drv(x^2.5,x)"),
	?line "1"=calc:parse_evaluate("drv(x+y^2,x)"),
	?line "cosh(x)" = calc:parse_evaluate("drv(sinh(x),x)"),
	?line "sinh(x)" = calc:parse_evaluate("drv(cosh(x),x)"),
	?line "1/cos(x)^2" = calc:parse_evaluate("drv(tan(x),x)"),
	?line "1/cosh(x)^2" = calc:parse_evaluate("drv(tanh(x),x)"),
 	?line "1/x" = calc:parse_evaluate("drv(log(x),x)"),
 	?line "0.43429448190325176/x" = calc:parse_evaluate("drv(log10(x),x)"),
	?line "(1-x^2)^-0.5" = calc:parse_evaluate("drv(asin(x),x)"),
	?line "-(1-x^2)^-0.5" = calc:parse_evaluate("drv(acos(x),x)"),
	?line "1/(1+x^2)" = calc:parse_evaluate("drv(atan(x),x)"),
	?line "(1+x^2)^-0.5" = calc:parse_evaluate("drv(asinh(x),x)"),
	?line "(-1+x^2)^-0.5" = calc:parse_evaluate("drv(acosh(x),x)"),
	?line "1/(1-x^2)" = calc:parse_evaluate("drv(atanh(x),x)"),
	?line "(1+log(x))*x^x" = calc:parse_evaluate("drv(x^x,x)"),
	?line "cos(x)*cos(sin(x))" = calc:parse_evaluate("drv(sin(sin(x)),x)"),
	?line "(log(sin(x))+(x*cos(x))/sin(x))*sin(x)^x" = calc:parse_evaluate("drv(sin(x)^x,x)"),
	?line "0.5*x^-0.5" = calc:parse_evaluate("drv(sqrt(x),x)"),
 ok.
	