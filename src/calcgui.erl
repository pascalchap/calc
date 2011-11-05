-module(calcgui).

-vsn("v0.1 alpha --").

-export([init/1, handle_info/2, handle_call/3, handle_event/2, 
	 terminate/2, code_change/3]).

-export([start_link/1,selectvar/2]).

-behaviour(wx_object).

-include("../include/calc.hrl").

-record(state,{board,frame,screen,input,calc}).

start_link(Server) ->
    wx:new(),
    {wx_ref,_Id,_WxType,Pid} = wx_object:start_link(?MODULE, [Server], []),
	Pid.

selectvar(L,G) ->
	gen_server:call(G,{selectVar,L}).
	
%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

init([Server]) ->
    {Frame, Board,Screen,Input} = wx:batch(fun() -> create_window() end),
    {Frame, #state{board=Board,frame=Frame,screen=Screen,input=Input,calc=Server}}.

handle_info(quit, S=#state{frame=F}) ->
    wxWindow:close(F),
    %% wx_core:quit(), 
    {stop, shutdown, S};

handle_info(needrestart, S=#state{frame=F}) ->
	io:format("Received  message needrestart~n",[]),
    wxWindow:close(F),
    %% wx_core:quit(), 
    {stop, die, S};

handle_info({display,Input,Res},S = #state{screen=Scr}) -> 
	io:format("received display command Input = ~p~nRes = ~p~n",[Input,Res]), 
	affiche(Scr,Input,Res),
    {noreply, S};
	
handle_info(M,S = #state{frame=F}) -> 
	M1 = io_lib:format("Received unexpected message ~p",[M]),
	wxFrame:setStatusText(F,M1,[]),
    {noreply, S}.
	
handle_event(#wx{event=#wxClose{}},S = #state{frame=F}) ->
    catch wxWindow:'Destroy'(F),
    {stop, shutdown, S};

handle_event(#wx{event=#wxCommand{type=command_button_clicked},id=Id},S) ->
	keypress(Id,S),
    {noreply,S};

handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?CLEARDISP},S = #state{screen=Scr}) ->
	wxTextCtrl:clear(Scr),
    {noreply,S};

handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?QUIT},S = #state{frame=F}) ->
    catch wxWindow:'Destroy'(F),
    {stop, shutdown, S};
	
handle_event(E,S = #state{frame=F}) ->
	M = io_lib:format("Received Event ~p from Id ~p",[E#wx.event,E#wx.id]),
	wxFrame:setStatusText(F,M,[]),
    {noreply,S}.

handle_call({selectVar,L}, _From, State=#state{frame=F}) ->
	Reply = dialog_choosevar(L,F),
    {reply, Reply, State};
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(die, _State) ->
	io:format("terminate calcgui with reason = die~n"),
    die;
terminate(Reason, _State) ->
	io:format("terminate calcgui with reason = ~p~n",[Reason]),
	application:stop(calc),
    normal.
	
%%%%%%%%%%%%%%%%%%%%% local helpers %%%%%%%%%%%%%

create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "La Calculette à Chapi", [{size,{444,485}},
							{style,?wxFULL_REPAINT_ON_RESIZE bor ?wxDEFAULT_FRAME_STYLE}]),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),	
	
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Aff     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?QUIT, "&Quit the marvelous calculette"),

    wxMenu:append(Help, ?ABOUT, "About"), 

    wxMenu:appendRadioItem(Aff, ?BIN, "Binaire"),
    wxMenu:appendRadioItem(Aff, ?OCT, "Octal"),
    LItem = wxMenu:appendRadioItem(Aff, ?DEC, "Decimal"),
    wxMenu:appendRadioItem(Aff, ?HEX, "Hexadecimal"),
    wxMenu:appendSeparator(Aff),
    wxMenu:append(Aff, ?CLEARDISP, "Clear display"),
    wxMenu:appendSeparator(Aff),
    EItem = wxMenu:appendCheckItem(Aff, ?ERR, "Show errors"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Aff, "&Affichage"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

	MainSz = wxBoxSizer:new(?wxVERTICAL),

    Board = wxPanel:new(Frame),
	wxWindow:setSizer(Board,MainSz),
	
	ScrSz = wxStaticBoxSizer:new(?wxVERTICAL, Board,[{label, "Display"}]),
	InSz = wxStaticBoxSizer:new(?wxVERTICAL, Board,[{label, "Input"}]),
	KeySz = wxStaticBoxSizer:new(?wxVERTICAL, Board,[{label, "Keyboard"}]),
	KeyGrSz = wxGridSizer:new(0, 10,2,2),
	
	Screen = wxTextCtrl:new(Board, 110, [{size, {300,200}},
			{style, ?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxHSCROLL bor ? wxVSCROLL bor ?wxTE_READONLY}]),
	Input = wxTextCtrl:new(Board,111,[{style, ?wxDEFAULT}]),

	
 	wxSizer:addSpacer(MainSz,2),
    wxSizer:add(ScrSz, Screen, [{flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:add(MainSz, ScrSz, [{proportion, 1}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(InSz, Input, [{flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:add(MainSz, InSz, [{proportion, 0}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(KeySz, KeyGrSz, [{flag, ?wxALL bor ?wxEXPAND}]), 
    [wxSizer:add(KeyGrSz, wxButton:new(Board,Id,[{label,Txt},{size,{40,20}}]), [{flag, ?wxALL}]) || 
	 {Id,Txt} <- buttonlist()], 
    wxSizer:add(MainSz, KeySz, [{proportion, 0}, {border, 4}, {flag, ?wxALL}]), 
    wxSizer:addSpacer(MainSz,2),


    wxWindow:connect(Board, command_button_clicked),
    wxWindow:show(Frame),
    wxMenuItem:check(LItem),
    wxMenuItem:check(EItem),
    {Frame, Board,Screen,Input}.
	
buttonlist() ->
	[?B_SEPT,?B_HUIT,?B_NEUF,?B_DIV,?B_CLEAR,?B_RACINE,?B_PI,?B_EXP,?B_LOG,?B_ABS,
	?B_QUATRE,?B_CINQ,?B_SIX,?B_MULT,?B_DIVE,?B_REM,?B_DERIV,?B_SIN,?B_SINH,?B_FRAC,
	?B_UN,?B_DEUX,?B_TROIS,?B_MOINS,?B_PARO,?B_PARF,?B_LOG10,?B_COS,?B_COSH,?B_INT,
	?B_POINT,?B_ZERO,?B_EGAL,?B_PLUS,?B_FACT,?B_ARRG,?B_COMB,?B_TAN,?B_TANH,?B_RES].
	
keypress(Id,#state{input = I}) when Id >= ?ZERO, Id =< ?NEUF -> 
	addinput(I,integer_to_list(Id - ?ZERO));
keypress(?POINT,#state{input = I}) ->
	addinput(I,".");
	
keypress(?PLUS,#state{input = I}) ->
	addinput(I," + ");
keypress(?MOINS,#state{input = I}) ->
	addinput(I," - ");
keypress(?MULT,#state{input = I}) ->
	addinput(I," * ");
keypress(?DIV,#state{input = I}) ->
	addinput(I," / ");
keypress(?PARO,#state{input = I}) ->
	addinput(I,"(");
keypress(?PARF,#state{input = I}) ->
	addinput(I,")");
keypress(?RACINE,#state{input = I}) ->
	addinput(I,"sqrt(");
keypress(?EXP,#state{input = I}) ->
	addinput(I,"^");
keypress(?LOG,#state{input = I}) ->
	addinput(I,"log(");
keypress(?SIN,#state{input = I}) ->
	addinput(I,"sin(");
keypress(?SINH,#state{input = I}) ->
	addinput(I,"sinh(");
keypress(?LOG10,#state{input = I}) ->
	addinput(I,"log10(");
keypress(?COS,#state{input = I}) ->
	addinput(I,"cos(");
keypress(?COSH,#state{input = I}) ->
	addinput(I,"cosh(");
keypress(?TAN,#state{input = I}) ->
	addinput(I,"tan(");
keypress(?TANH,#state{input = I}) ->
	addinput(I,"tanh(");
keypress(?ABS,#state{input = I}) ->
	addinput(I,"abs(");
keypress(?FRAC,#state{input = I}) ->
	addinput(I,"frac(");
keypress(?INT,#state{input = I}) ->
	addinput(I,"int(");
keypress(?RES,_) -> ok;
keypress(?FACT,#state{input = I}) ->
	addinput(I,"fact(");
keypress(?DIVE,#state{input = I}) ->
	addinput(I," div ");
keypress(?REM,#state{input = I}) ->
	addinput(I," rem ");
keypress(?COMB,#state{input = I}) ->
	addinput(I," comb ");
keypress(?ARRG,#state{input = I}) ->
	addinput(I," arrg ");
keypress(?PI,#state{input = I}) ->
	addinput(I," pi# ");
	
keypress(?CLEAR,#state{input=I}) -> 
	wxTextCtrl:clear(I);

keypress(?EGAL,#state{input=I,calc=Calc}) -> 
	T = wxTextCtrl:getLineText(I,0),
	evaluate(T,Calc);
keypress(?DERIV,#state{input=I,calc=Calc}) -> 
	T = wxTextCtrl:getLineText(I,0),
	derive(T,Calc);
	
keypress(Id,#state{input=_I})  -> 
	io:format("traiter Id ~p~n",[Id]).
	
addinput(I,S) ->
	wxTextCtrl:writeText(I,S).

evaluate(T,Calc) ->
	calc_server:evaluate(T,Calc).

derive(T,Calc) ->
	calc_server:derive(T,Calc).

affiche(S,T,{error,R}) ->
	Mess = T ++ " --> Warning \n    " ++ R ++ "\n",
	wxTextCtrl:appendText(S,Mess);
affiche(S,T,R) when is_list(R)->
	Mess = T ++ "\n    " ++ R ++ "\n",
	wxTextCtrl:appendText(S,Mess);
affiche(S,T,R) ->
	Mess = io_lib:format(T ++ " = ~n    ~p~n",[R]),
	wxTextCtrl:appendText(S,Mess).

dialog_choosevar(L,F) ->
    Str = "select variable",
    MD = wxFrame:new(F, 1000, Str, [{size,{250,150}},
							{style,?wxFULL_REPAINT_ON_RESIZE bor ?wxDEFAULT_FRAME_STYLE}]),
%%    Choice = wxChoice:new(MD, 4, [{choices, L}]),
%%    wxChoice:setToolTip(Choice, "select one variable for derive operation"),
%%    wxChoice:connect(Choice,command_choice_selected),

    Choice = wxListBox:new(MD, 2, [{size, {-1,100}},
					{choices, L},
					{style, ?wxLB_SINGLE}]),
    wxListBox:setToolTip(Choice, "select one variable for derive operation"),
    wxFrame:connect(MD, close_window),	
    wxFrame:makeModal(MD),
	wxFrame:show(MD),
	["x"].