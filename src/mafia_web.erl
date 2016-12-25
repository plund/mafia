%%%-------------------------------------------------------------------
%%% @author Peter Lund <peter@liber>
%%% @copyright (C) 2016, Peter Lund
%%% @doc Updates the DB every 10 mins, 5 min last hour and 1 min last
%%% 7 min to day deadline.
%%% @end
%%% Created : 10 Dec 2016 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(mafia_web).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         stop/0,

         start_polling/0,
         stop_polling/0,

         start_web/0,
         stop_httpd/1,

         get_state/0,
         regenerate_history/1,  % used to print txt files when refresh_votes
         regenerate_history/2
        ]).

%% web
-export([msg_search_result/3, vote_tracker/3, stats/3]).

%% deprecated
-export([set_interval_minutes/1]).

%% test
%% -export([is_word/2, allpos/2, find_word_searches/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(mafia, [l2u/1, b2ul/1, getv/1]).

-include("mafia.hrl").

-define(SERVER, ?MODULE).
-define(WEBPORT, 50666).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").
%% ServerRoot is relative to running path.
-define(SERVER_ROOT, "/Users/peter/httpd/mafia.peterlund.se").
%% DocumentRoot is relative to running path
-define(DOC_ROOT, "/Users/peter/httpd/mafia.peterlund.se/html").
-define(LOG_ROOT, "/Users/peter/httpd/mafia.peterlund.se/logs").

-record(state,
        {timer :: reference(),
         timer_minutes :: integer(),
         web_pid :: pid(),
         game_key :: thread_id()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_web() ->
    gen_server:call(?SERVER, start_web).

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?SERVER, 'stop').

start_polling() -> gen_server:call(?SERVER, 'start_polling').
stop_polling() -> gen_server:call(?SERVER, 'stop_polling').

stop_httpd(a) ->
    io:format("Stopping webserver at ~p, port ~p\n",
              [{192,168,0,100}, ?WEBPORT]),
    inets:stop(httpd, {{192,168,0,100}, ?WEBPORT});
stop_httpd(b) ->
    io:format("Stopping webserver at ~p, port ~p\n",
              [{192,168,0,3}, ?WEBPORT]),
    inets:stop(httpd, {{192,168,0,3}, ?WEBPORT}).

%%--------------------------------------------------------------------
%% @doc Show gen_server internal state
%% @end
%%--------------------------------------------------------------------
get_state()  ->
    gen_server:call(?SERVER, get_state).

%%--------------------------------------------------------------------
%% @doc Change the poll timer
%%      Old do not work since we go on schedule now.
%% @end
%%--------------------------------------------------------------------
set_interval_minutes(N) when is_integer(N)  ->
    gen_server:call(?SERVER, {set_timer_interval, N}).

%% rewrite one history txt file
regenerate_history(Time, Phase) ->
    ?dbg(Time, {"REGENERATE_HISTORY", Phase}),
    regenerate_historyI(Phase).

regenerate_history(Phase) ->
    ?dbg({"REGENERATE_HISTORY", Phase}),
    regenerate_historyI(Phase).

regenerate_historyI({DNum, DoN, _}) ->
    regenerate_historyI({DNum, DoN});
regenerate_historyI(Phase) ->
    gen_server:cast(?SERVER, {regenerate_history, Phase}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    mafia:setup_mnesia(),
    GameKey = getv(?game_key),
    State = start_web(#state{game_key = GameKey}),
    {_Reply, S2} = set_timer_interval(State, 10),
    self() ! do_polling,
    {ok, S2}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_state, _From, State) ->
    Fields = record_info(fields, state),
    Values = tl(tuple_to_list(State)),
    {reply, lists:zip(Fields, Values), State};
handle_call({set_timer_interval, N}, _From, State) ->
    {Reply, S2} = set_timer_interval(State, N),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call('stop', _From, State) ->
    timer:cancel(State#state.timer),
    inets:stop(httpd, State#state.web_pid),
    {stop, stopped, stop_reply, State#state{timer = ?undefined,
                                            web_pid = ?undefined}};
handle_call('start_polling', _From, State) ->
    {Reply, S2} = maybe_change_timer(State),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call('stop_polling', _From, State) ->
    timer:cancel(State#state.timer),
    {reply, {ok, polling_stopped}, State#state{timer = ?undefined}};

handle_call(start_web, _From, State) ->
    S = start_web(State),
    {reply, ok, S};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% rewrite one history txt file
handle_cast(Ev = {regenerate_history, {DNum, DoN}}, State) ->
    timer:sleep(300),
    ?dbg({"HC REGENERATE_HISTORY", DNum, DoN}),
    case mafia:rgame(State#state.game_key) of
        [] -> ok;
        [G] ->
            {GameDir, FilePrefix} = mafia_data:game_prefixes(G),
            PhStr = case DoN of
                        ?day -> "d";
                        ?night -> "n"
                    end ++ ?i2l(DNum),
            %% calculate "m25_d1.txt"
            PhaseFN = FilePrefix ++ PhStr ++ ".txt",
            FileName = filename:join([?DOC_ROOT, GameDir, PhaseFN]),
            {ok, Fd} = file:open(FileName, [write]),
            mafia_print:print_votes([{?game_key, State#state.game_key},
                                     {?phase, {DNum, DoN}},
                                     {?dev, Fd}]),
            file:close(Fd),
            flush({'$gen_cast', Ev})
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(do_polling, State) ->
    TimeStr = mafia_print:print_time(current_time, short),
    io:format("~s poll for new messages\n", [TimeStr]),
    mafia_data:downl_web(State#state.game_key),
    FileName = filename:join(?DOC_ROOT, "current_vote.txt"),
    {_Reply, S2} = maybe_change_timer(State),
    {ok, Fd} = file:open(FileName, [write]),
    Phase = mafia_time:calculate_phase(S2#state.game_key),
    mafia_print:print_votes([{?game_key, S2#state.game_key},
                             {?phase, Phase},
                             {?dev, Fd},
                             {?period, S2#state.timer_minutes}
                            ]),
    file:close(Fd),
    {noreply, S2};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_web(S) ->
    inets:start(),
    maybe_create_dir(?SERVER_ROOT),
    maybe_create_dir(?DOC_ROOT),
    maybe_create_dir(?LOG_ROOT),
    os:cmd("cp ../priv/search_form.html " ++ ?DOC_ROOT),
    os:cmd("cp ../priv/index.html " ++ ?DOC_ROOT),
    IP_en1 =
        lists:nth(2, lists:dropwhile(
                       fun("inet") -> false; (_) -> true end,
                       string:tokens(os:cmd("ifconfig en1"), "\t\n\s"))),
    io:format("Starting up a webserver listening on ~s\n", [IP_en1]),
    case inets:start(httpd,
                     [{port, ?WEBPORT},
                      {server_name, "mafia_test.peterlund.se"},
                      {server_root, ?SERVER_ROOT},
                      {document_root, ?DOC_ROOT},
                      {directory_index, ["index.html"]},
                      {bind_address, IP_en1},
%%% specifying modules removes the default list in where
%%% mod_esi and mod_dir already are included by default
%%                       {modules, [
%%                                  %% mod_alias,
%%                                  %% mod_auth,
%% %%% mod_esi - http://mafia_test.peterlund.se/esi/mafia_web/msg_search_result
%%                                  mod_esi,
%%                                  %% mod_actions,
%%                                  %% mod_cgi, %mod_include,
%% %%% mod_dir - browse directories
%%                                  mod_dir
%%                                  %% mod_get,
%%                                  %% mod_head, mod_log, mod_disk_log
%%                                 ]},
                      {erl_script_alias, {"/e", [web]}},

                      {error_log, "logs/error_log.txt"},
                      {security_log, "logs/security_log.txt"},
                      {transfer_log, "logs/transfer_log.txt"}
                     ]) of
        {ok, Pid} ->
            S#state{web_pid = Pid};
        Else ->
            io:format("Else ~p\n", [Else]),
            S
    end.

%% must create dirs first.
%% mkdir -p /Users/peter/httpd/mafia.peterlund.se/html
maybe_create_dir(Dir) ->
    case file:read_file_info(Dir) of
        {error,enoent} ->
            os:cmd("mkdir -p " ++ Dir),
            io:format("Created ~s\n", [Dir]);
        _ -> ok
    end.

-spec maybe_change_timer(#state{}) -> {Reply::term(), #state{}}.
maybe_change_timer(S = #state{timer = TRef,
                              timer_minutes = TMins,
                              game_key = ThId}) ->
    case mafia_time:timer_minutes(ThId) of
        Mins when is_integer(Mins), Mins /= TMins ->
            set_timer_interval(S, Mins);
        Mins when TRef == ?undefined ->
            set_timer_interval(S, Mins);
        _ -> {no_change, S}
    end.

-spec set_timer_interval(#state{}, integer()) -> {Reply :: term(), #state{}}.
set_timer_interval(S, N) when is_integer(N), N >= 1 ->
    if S#state.timer /= ?undefined ->
            timer:cancel(S#state.timer),
            flush(do_polling);
       true -> ok
    end,
    {ok, TRef} = timer:send_interval(N * ?MINUTE_MS, do_polling),
    Reply = {interval_changed,
             {old, S#state.timer_minutes},
             {new, N}},
    {Reply, S#state{timer = TRef,
                    timer_minutes = N}}.

flush(Msg) ->
    receive Msg -> flush(Msg)
    after 0 -> ok
    end.

%% http://mafia_test.peterlund.se/e/web/msg_search_result
msg_search_result(Sid, _Env, In) ->
    ThId = getv(?game_key),
    PQ = httpd:parse_query(In),
    mod_esi:deliver(Sid, ["Content-type: text/html\r\n",
                          "Transfer-Encoding: chunked\r\n", %% maybe not needed
                          "\r\n"]),
    {_, UsersText} = lists:keyfind("user names", 1, PQ),
    {_, WordsText} = lists:keyfind("contained words", 1, PQ),
    {_, DayNumText} = lists:keyfind("day numbers", 1, PQ),
    DayCond =
        try
            DayNumU = ?l2u(DayNumText),
            case DayNumU of
                "END" ++ _ -> ?game_ended;
                _ ->
                    case string:tokens(DayNumU, "-") of
                        [LoStr, "END" ++ _] ->
                            {list_to_integer(string:strip(LoStr)),
                             ?game_ended};
                        [LoStr, HiStr] ->
                            {list_to_integer(string:strip(LoStr)),
                             list_to_integer(string:strip(HiStr))};
                        [Str] ->
                            Num = list_to_integer(string:strip(Str)),
                            {Num, Num};
                        _ -> all
                    end
            end
        catch _:_ -> all
        end,
    UsersU = find_word_searches(UsersText),
    WordsU = find_word_searches(WordsText),
    IsDayCondSingle = case DayCond of
                          {DNumC, DNumC} -> true;
                          ?game_ended -> true;
                          _ -> false
                      end,
    IsWordCond = WordsU /= [],
    IsUserCond = UsersU /= [],
    DoCont = IsUserCond orelse IsWordCond orelse IsDayCondSingle,
    Fun =
        fun(acc, init) -> 0;
           (#message{user_name = MsgUserB,
                     page_num = PageNum,
                     time = Time,
                     message = MsgB},
            Acc) ->
                MsgPhase = mafia_time:calculate_phase(ThId, Time),
                Msg = ?b2l(MsgB),
                B2U = fun(B) -> string:to_upper(binary_to_list(B)) end,
                MsgUserU = B2U(MsgUserB),
                TestFuns =
                    [
                     %% 1. Test if any of Users in form matches MsgUser
                     fun() ->
                             UsersU == [] orelse
                                 lists:any(
                                   fun(UserU) ->
                                           0 /= string:str(MsgUserU, UserU)
                                   end,
                                   UsersU)
                     end,

                     %% 2. Test Words with ANY instead of all
                     fun() ->
                             MsgU = ?l2u(Msg),
                             lists:all(
                               fun(WordU) ->
                                       lists:any(
                                         fun(OrWordU) ->
                                                 is_word(MsgU, OrWordU)
                                         end,
                                         string:tokens(WordU, "|"))
                               end,
                               WordsU)
                     end,

                     %% 3. Test Day
                     fun() ->
                             case {DayCond, MsgPhase} of
                                 {all, _} -> true;
                                 {?game_ended, ?game_ended} -> true;
                                 %% Next condition works as expected also
                                 %% if NHi is ?game_ended
                                 {{NLo, NHi}, {DNum, _DoN}}
                                   when NLo =< DNum,
                                        DNum =< NHi -> true;
                                 {{_NLo, ?game_ended}, ?game_ended} -> true;
                                 _ -> false
                             end
                     end],
                AllTestsOk = lists:all(fun(F) -> F() end, TestFuns),
                if AllTestsOk ->
                        DayStr = case MsgPhase of
                                     {DNum, ?day} -> "Day-" ++ ?i2l(DNum);
                                     {DNum, ?night} -> "Night-" ++ ?i2l(DNum);
                                     ?game_ended -> "Game End "
                                 end,
                        Hash = erlang:phash2(MsgUserB, 16#1000000),
                        Color = Hash bor 16#C0C0C0,
                        ColorStr = integer_to_list(Color, 16),
                        {HH, MM} = mafia_time:hh_mm_to_deadline(ThId, Time),
                        OutB = ?l2b(["<tr bgcolor=\"#", ColorStr,
                                    "\"><td valign=\"top\"><b>", MsgUserB,
                                    "</b><br>",
                                    DayStr, " ", p(HH), ":", p(MM),
                                    "<br> page ", ?i2l(PageNum),
                                    "</td><td valign=\"top\">", Msg,
                                    "</td></tr>\r\n"]),
                        mod_esi:deliver(Sid, OutB),
                        Acc + size(OutB);
                   true -> Acc
                end;
           (_, Acc) ->
                Acc
        end,
    TimeA = erlang:monotonic_time(millisecond),
    A = del_start(Sid, "Mafia Search Result"),
    B = if DoCont ->
                mafia_data:iterate_all_msgs(ThId, Fun);
           true ->
                MsgB = ?l2b(["<tr><td valign=\"top\">",
                            "Error: Minimum one condition needs to be "
                            "specified: User name, Word or a "
                            "single Day number!",
                            "</td></tr>"]),
                mod_esi:deliver(Sid, MsgB),
                size(MsgB)
        end,
    C = del_end(Sid),
    TimeB = erlang:monotonic_time(millisecond),
    NumBytes = A + B + C,
    MilliSecs = TimeB - TimeA,
    TimeStr = mafia_print:print_time(current_time, short),
    io:format("~s Sent ~p bytes in ~p millisecs, search =*~s*=*~s*=*~s*=\n",
              [TimeStr, NumBytes, MilliSecs, UsersText, WordsText, DayNumText]).

find_word_searches(WordText) ->
    [?l2u(Str) || Str <- fws(WordText,
                            _InQuotes = false,
                            _QStrs = [],
                            _CharAcc = "")].

fws("", _IsInQ, QStrs, Acc) ->
    QStrs2 = add_cond(QStrs, Acc),
    ?lrev(QStrs2);
fws("\""++T, true, QStrs, Acc) ->
    fws(T, false, QStrs, Acc);
fws("\""++T, false, QStrs, Acc) ->
    fws(T, true, QStrs, Acc);
fws(" "++T, false, QStrs, Acc) ->
    QStr2 = add_cond(QStrs, Acc),
    fws(T, false, QStr2, "");
fws([H|T], IsInQ, QStrs, Acc) ->
    fws(T, IsInQ, QStrs, [H|Acc]).

add_cond(QStrs, Acc) ->
    if Acc /= "" -> [?lrev(Acc)|QStrs];
       true -> QStrs
    end.

is_word(MsgU, Search) ->
    AllPos = allpos(MsgU, Search),
    LenMsg = length(MsgU),
    LenSea = length(Search),
    lists:any(fun(P) -> is_word(MsgU, P, LenMsg, LenSea) end,
              AllPos).

allpos(MsgU, Search) -> allpos(MsgU, Search, 0, []).

allpos(MsgU, Search, Offset, Acc) ->
    case string:str(MsgU, Search) of
        0 ->
            ?lrev(Acc);
        P ->
            MsgU2 = lists:nthtail(P, MsgU),
            FoundAt = P + Offset,
            allpos(MsgU2, Search, FoundAt, [FoundAt | Acc])
    end.

-define(BoundaryChars, " !\"@#€$%&/\\|()[]{}=≈≠´`^*'™’-_.:…·,;‚„<>≥≤").

is_word(MsgU, Pos, LenMsg, LenSea) ->
    IsAtBeg = Pos == 1,
    NextPosAfterSearch = Pos + LenSea,
    LastPosInSearch = NextPosAfterSearch - 1,
    IsAtEnd = LenMsg == LastPosInSearch,
    IsBoundA = IsAtBeg orelse
        lists:member(lists:nth(Pos - 1, MsgU), ?BoundaryChars),
    IsBoundB = IsAtEnd orelse
        lists:member(lists:nth(NextPosAfterSearch, MsgU), ?BoundaryChars),
    IsBoundA and IsBoundB.

p(I) when I > 9 -> ?i2l(I);
p(I) -> string:right(?i2l(I), 2, $0).

-define(RES_START(Title, Border),
 "<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>" ++ Title ++ "</title>
  </head>
  <body bgcolor=\"#cfffaf\">
    <center>
      <h3>" ++ Title ++ "</h3>
      <table" ++ Border ++ ">").

-define(RES_END, "
      </table>
    </center>
  </body>
</html>").

del_start(Sid, Title) ->
    del_start(Sid, Title, 1).

del_start(Sid, Title, 0) ->
    Border = "",
    del_start(Sid, Title, Border);
del_start(Sid, Title, BordInt) when is_integer(BordInt) ->
    Border = " border=\"" ++ ?i2l(BordInt) ++ "\"",
    del_start(Sid, Title, Border);
del_start(Sid, Title, Border) ->
    Start = ?RES_START(Title, Border),
    mod_esi:deliver(Sid, Start),
    size(?l2b(Start)).

del_end(Sid) ->
    mod_esi:deliver(Sid, ?RES_END),
    size(?l2b(?RES_END)).

%% http://mafia_test.peterlund.se/e/web/vote_tracker?day=1
%% http://mafia_test.peterlund.se/e/web/vote_tracker?msg_id=1420335
vote_tracker(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    case vote_tracker2(lists:keyfind("day", 1,  PQ),
                       lists:keyfind("msg_id", 1,  PQ)) of
        {tracker, Out} ->
            _A = del_start(Sid, "Vote Tracker", 0);
        {error, Out} ->
            _A = del_start(Sid, "Vote Tracker Error", 1);
        Out ->
            _A = del_start(Sid, "Vote Message", 1)
    end,
    mod_esi:deliver(Sid, Out),
    _C = del_end(Sid).

vote_tracker2({"day", Str},
              _) ->
    try
        DayNum = list_to_integer(Str),
        [RK, VT] = mafia_print:web_vote_tracker(DayNum),
        {tracker, ?l2b(["<tr><td>", RK, "</td></tr>",
                        "<tr><td>", VT, "</td></tr>"
                       ])}
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert day value to integer"
             "</td></tr>"}
    end;
vote_tracker2(?false,
              {"msg_id", Str}) ->
    try
        MsgId = list_to_integer(Str),
        show_msg(MsgId)
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert msg_id value to integer"
             "</td></tr>"}
    end;
vote_tracker2(?false,
              ?false) ->
    {error,
     "<tr><td>bad_request</td></tr>"}.

show_msg(MsgId) when is_integer(MsgId) ->
    show_msg(mafia:rmess(MsgId));

show_msg([]) -> "<tr><td>No message found with this id</td></tr>";
show_msg([#message{user_name = MsgUserB,
                   page_num = PageNum,
                   time = Time,
                   message = MsgB}]) ->
    GameKey = getv(?game_key),
    MsgPhase = mafia_time:calculate_phase(GameKey, Time),
    DayStr = case MsgPhase of
                 {DNum, ?day} -> "Day-" ++ ?i2l(DNum);
                 {DNum, ?night} -> "Night-" ++ ?i2l(DNum);
                 ?game_ended -> "Game End "
             end,
    Hash = erlang:phash2(MsgUserB, 16#1000000),
    Color = Hash bor 16#C0C0C0,
    ColorStr = integer_to_list(Color, 16),
    {HH, MM} = mafia_time:hh_mm_to_deadline(GameKey, Time),
    ?l2b(["<tr bgcolor=\"#", ColorStr,
         "\"><td valign=\"top\"><b>", MsgUserB,
         "</b><br>",
         DayStr, " ", p(HH), ":", p(MM),
         "<br> page ", ?i2l(PageNum),
         "</td><td valign=\"top\">", MsgB,
         "</td></tr>\r\n"]).

%% http://mafia_test.peterlund.se/e/web/stats?phase=day&num=1
%% http://mafia_test.peterlund.se/e/web/stats?phase=night&num=1
%% http://mafia_test.peterlund.se/e/web/stats?phase=end
%% http://mafia_test.peterlund.se/e/web/stats?phase=total
stats(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    Html =
        (catch case stats2(lists:keyfind("phase", 1,  PQ),
                           lists:keyfind("num", 1,  PQ)) of
                   {ok, Phase} ->
                       mafia_print:print_stats([{game_key, getv(game_key)},
                                                {phase, Phase},
                                                {mode, ?html}
                                               ]);
                   {error, ErrorHtml} -> ErrorHtml
               end),
    Html2 =
        case Html of
            {'EXIT', Term} ->
                Str = lists:flatten(io_lib:format("~p\n", [Term])),
                ?dbg_str(Str),
                ["<tr><td>", Str, "</td></tr>"];
            _ -> Html
        end,
    _A = del_start(Sid, "Posting Stats", 1),
    mod_esi:deliver(Sid, Html2),
    _C = del_end(Sid).

stats2({"phase", "total"},
       _) ->
    {ok, ?total_stats};
stats2({"phase", "end"},
       _) ->
    {ok, ?game_ended};
stats2({"phase", "day"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, {Num, ?day}};
        {error, _HtmlErr} = E -> E
    end;
stats2({"phase", "night"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, {Num, ?night}};
        {error, _HtmlErr} = E -> E
    end;
stats2(_, _) ->
    {error, "<tr><td>"
     "You need to end url with .../stats?phase=day&num=1, "
     "?phase=night&num=2 or ?phase=end"
     "</td></tr>"}.

conv_to_num(Str) ->
    try
        Num = list_to_integer(Str),
        {ok, Num}
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert day value to integer"
             "</td></tr>"}
    end.
