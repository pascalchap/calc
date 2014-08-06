-module(calcgui).

%% @doc
%% @todo Utiliser un server pour stocker les informations d'affichage et permettre la gestion base 2,8,10,16.
%% @todo Coriger le probl�me d'affichage tronqu� des grand nombre (fact(1000))
-vsn("v0.1 alpha --").

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2, 
     terminate/2, code_change/3]).

-export([start_link/1]).


-behaviour(wx_object).

-include("../include/calc.hrl").

-record(state,{board,frame,screen,input,calc,button=buttonlist()}).

-spec start_link(_) -> any().
start_link(Server) ->
    wx:new(),
    {wx_ref,_Id,_WxType,Pid} = wx_object:start_link(?MODULE, [Server], []),
    Pid.

%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

-spec init([any(),...]) -> {_,#state{button::[any(),...]}}.
init([Server]) ->
    {Frame, Board,Screen,Input} = wx:batch(fun() -> create_window() end),
    io:format("input = ~p~n",[Input]),
    {Frame, #state{board=Board,frame=Frame,screen=Screen,input=Input,calc=Server}}.

-spec handle_info(_,#state{}) -> {'noreply',#state{}} | {'stop','die' | 'shutdown',#state{frame::{'wx_ref',integer(),_,_}}}.
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

-spec handle_event(#wx{id::'undefined' | integer(),obj::'undefined' | {'wx_ref',_,_,_},event::'undefined' | tuple()},_) -> {'noreply',_} | {'stop','shutdown',#state{}}.
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

handle_event(#wx{event=#wxKey{type=char,keyCode=13}},S) ->
    %% theb user pressed enter
    keyenter(S),
    {noreply,S};

handle_event(#wx{event=#wxKey{type=char}},S) ->
    %% for any other key, use the standard behaviour (option skip)
    {noreply,S};

handle_event(E,S = #state{frame=F}) ->
    M = io_lib:format("Received Event ~p from Id ~p",[E#wx.event,E#wx.id]),
    wxFrame:setStatusText(F,M,[]),
    {noreply,S}.

-spec handle_call(_,_,_) -> {'stop',{'call',_},_}.
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(_What, State) ->
    {noreply, State}.
    
-spec code_change(_,_,_) -> {'stop','not_yet_implemented',_}.
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

-spec terminate(_,_) -> 'die' | 'normal'.
terminate(die, _State) ->
    io:format("terminate calcgui with reason = die~n"),
    die;
terminate(Reason, _State) ->
    io:format("terminate calcgui with reason = ~p~n",[Reason]),
    application:stop(calc),
    normal.

%%%%%%%%%%%%%%%%%%%%% local helpers %%%%%%%%%%%%%

-spec create_window() -> {{'wx_ref',integer(),_,_},{'wx_ref',integer(),_,_},{'wx_ref',integer(),_,_},{'wx_ref',integer(),_,_}}.
create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "La Calculette � Chapi", [{size,{444,510}},
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
    Input = wxTextCtrl:new(Board,111,[{style, ?wxDEFAULT bor ?wxTE_PROCESS_ENTER}]),


    wxSizer:addSpacer(MainSz,2),
    wxSizer:add(ScrSz, Screen, [{flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:add(MainSz, ScrSz, [{proportion, 1}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(InSz, Input, [{flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:add(MainSz, InSz, [{proportion, 0}, {border, 4}, {flag, ?wxALL bor ?wxEXPAND}]), 
    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(KeySz, KeyGrSz, [{flag, ?wxALL bor ?wxEXPAND}]), 
    [wxSizer:add(KeyGrSz, wxButton:new(Board,Id,[{label,Txt},{size,{40,20}}]), [{flag, ?wxALL}]) || 
    {Id,Txt,_Print} <- buttonlist()], 
    wxSizer:add(MainSz, KeySz, [{proportion, 0}, {border, 4}, {flag, ?wxALL}]), 
    wxSizer:addSpacer(MainSz,2),


    wxWindow:connect(Board, command_button_clicked),
    wxWindow:show(Frame),
    wxMenuItem:check(LItem),
    wxMenuItem:check(EItem),
    wxTextCtrl:connect(Input,char,[{skip,true}]),
    wxWindow:setFocus(Input),
    {Frame, Board,Screen,Input}.

-spec buttonlist() -> [{1..255,[1..255,...],'null' | [1..255,...]},...].
buttonlist() ->
    [?B_SEPT,?B_HUIT,?B_NEUF,?B_DIV,?B_CLEAR,?B_RACINE,?B_PI,?B_EXP,?B_LOG,?B_ABS,
     ?B_QUATRE,?B_CINQ,?B_SIX,?B_MULT,?B_DIVE,?B_REM,?B_DERIV,?B_SIN,?B_SINH,?B_FRAC,
     ?B_UN,?B_DEUX,?B_TROIS,?B_MOINS,?B_PARO,?B_PARF,?B_LOG10,?B_COS,?B_COSH,?B_INT,
     ?B_POINT,?B_ZERO,?B_EGAL,?B_PLUS,?B_FACT,?B_ARRG,?B_COMB,?B_TAN,?B_TANH,?B_RES,
     ?B_ASIN,?B_ASINH,?B_ACOS,?B_ACOSH,?B_ATAN,?B_ATANH,?B_RES,?B_RES,?B_RES,?B_ENTER].


-spec keypress('undefined' | integer(),#state{input::{'wx_ref',integer(),_,_}}) -> 'ok'.
keypress(?CLEAR,#state{input=I}) -> 
    wxTextCtrl:clear(I),
    wxWindow:setFocus(I);

keypress(?ENTER,#state{input=I,calc=Calc}) -> 
    T = wxTextCtrl:getLineText(I,0),
    wxWindow:setFocus(I),
    evaluate(T,Calc);

keypress(Id,#state{input=I,button=B})  -> 
    {Id,_Text,Print} = lists:keyfind(Id,1,B),
    addinput(I,Print),
    wxWindow:setFocus(I).

-spec keyenter(#state{input::{'wx_ref',integer(),_,_},calc::atom() | pid() | {atom(),_} | {'via',_,_}}) -> 'ok'.
keyenter(#state{input=I,calc=Calc}) -> %% Enter
    T = wxTextCtrl:getLineText(I,0),
    wxWindow:setFocus(I),
    evaluate(T,Calc).

-spec addinput({'wx_ref',integer(),_,_},[binary() | [any()] | non_neg_integer()]) -> 'ok'.
addinput(I,S) ->
    wxTextCtrl:writeText(I,S),
    wxWindow:setFocus(I).

-spec evaluate([binary() | [binary() | [any()] | non_neg_integer()] | non_neg_integer()],atom() | pid() | {atom(),_} | {'via',_,_}) -> 'ok'.
evaluate(T,Calc) ->
    calc_server:evaluate(T,Calc).

-spec affiche({'wx_ref',integer(),_,_},[any()],_) -> 'ok'.
affiche(S,T,{error,R}) ->
    Mess = T ++ " --> Warning \n    " ++ R ++ "\n",
    wxTextCtrl:appendText(S,Mess);
affiche(S,T,R) when is_list(R)->
    Mess = T ++ "\n    " ++ R ++ "\n",
    wxTextCtrl:appendText(S,Mess);
affiche(S,T,R) ->
    Mess = io_lib:format(T ++ " = ~n    ~p~n",[R]),
    wxTextCtrl:appendText(S,Mess).
