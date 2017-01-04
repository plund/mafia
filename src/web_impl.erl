-module(web_impl).

%% web
-export([msg_search_result/3,
         game_status/3,
         vote_tracker/3,
         stats/3,

         show_msg/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

%% http://mafia_test.peterlund.se/e/web/msg_search_result
msg_search_result(Sid, _Env, In) ->
    ThId = ?getv(?game_key),
    PQ = httpd:parse_query(In),
    {_, UsersText} = lists:keyfind("user names", 1, PQ),
    {_, WordsText} = lists:keyfind("contained words", 1, PQ),
    {_, DayNumText} = lists:keyfind("day numbers", 1, PQ),
    DayCond =
        try
            DayNumU = ?l2u(DayNumText),
            case DayNumU of
                "END" ++ _ -> ?game_ended;
                %% "CURRENT" -> %% the current phase only.
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
                        MsgBoldMarked = bold_mark_words(Msg, WordsU),
                        Hash = erlang:phash2(MsgUserB, 16#1000000),
                        Color = Hash bor 16#C0C0C0,
                        ColorStr = integer_to_list(Color, 16),
                        {HH, MM} = mafia_time:hh_mm_to_deadline(ThId, Time),
                        OutB = ?l2b(["<tr bgcolor=\"#", ColorStr,
                                    "\"><td valign=\"top\"><b>", MsgUserB,
                                    "</b><br>",
                                    DayStr, " ", p(HH), ":", p(MM),
                                    "<br> page ", ?i2l(PageNum),
                                    "</td><td valign=\"top\">", MsgBoldMarked,
                                    "</td></tr>\r\n"]),
                        SizeOut = web:deliver(Sid, OutB),
                        Acc + SizeOut;
                   true -> Acc
                end;
           (_, Acc) ->
                Acc
        end,
    A = del_start(Sid, "Mafia Search Result", 0),
    B = if DoCont ->
                TabStart = "<tr><td><table cellpadding=6 cellspacing=3>",
                TabEnd = "</table></td></tr>",
                web:deliver(Sid, TabStart),
                mafia_data:iterate_all_msgs(ThId, Fun),
                web:deliver(Sid, TabEnd);
           true ->
                MsgB = ?l2b(["<tr><td valign=\"top\">",
                            "Error: Minimum one condition needs to be "
                            "specified: User name, Word or a "
                            "single Day number!",
                            "</td></tr>"]),
                web:deliver(Sid, MsgB)
        end,
    C = del_end(Sid),
    A + B + C.

find_word_searches(WordText) ->
    [?l2u(Str) || Str <- fws(WordText,
                            _InQuotes = out,
                            _QStrs = [],
                            _CharAcc = "")].

fws("", _IsInQ, QStrs, Acc) ->
    QStrs2 = add_cond(QStrs, Acc),
    ?lrev(QStrs2);
fws("\""++T, out, QStrs, Acc) ->
    fws(T, in, QStrs, Acc);
fws("\""++T, in, QStrs, Acc) ->
    fws(T, out, QStrs, Acc);
fws(" "++T, out, QStrs, Acc) ->
    QStr2 = add_cond(QStrs, Acc),
    fws(T, out, QStr2, "");
fws([H|T], IsInQ, QStrs, Acc) ->
    fws(T, IsInQ, QStrs, [H|Acc]).

add_cond(QStrs, Acc) ->
    if Acc /= "" -> [?lrev(Acc)|QStrs];
       true -> QStrs
    end.

bold_mark_words(Msg, WordsU) ->
    MsgU = ?l2u(Msg),
    WordsU2 = get_all_words_to_mark(WordsU),
    Intervals =
        mafia_lib:merge_intervals(
          lists:sort(
            lists:foldl(
              fun(SearchU, Acc) ->
                      %% ?dbg({bold_mark_words_l, }).
                      Len = length(SearchU),
                      Acc ++ [{P, P+Len-1} || P <- allpos(MsgU, SearchU)]
              end,
              [],
              WordsU2))),
    Start = "<b><font size=\"+1\">",
    Stop = "</font></b>",
    {_P, In2, Out2} =
        lists:foldl(fun({A, B}, {P, In, Out}) ->
                            P2 = A - P,
                            {S1, In2} = lists:split(P2, In),
                            P3 = B - A + 1,
                            {S2, In3} = lists:split(P3, In2),
                            {B+1, In3, Out ++ S1 ++ Start ++ S2 ++ Stop}
                    end,
                    {1, Msg, ""},
                    Intervals),
    Out2 ++ In2.

%% What to mark in bold in out text.
get_all_words_to_mark(WordsU) ->
    lists:foldl(
      fun(WordU, Acc) ->
              WordsUnoWC = [element(1, check_edges_for_wildcard(Str))
                            || Str <- string:tokens(WordU, "|")],
              Acc ++ [Str || Str <- WordsUnoWC, "" /= Str]
      end,
      [],
      WordsU).

is_word(MsgU, Search1) ->
    %% check if "*" at first or last char or both in Search
    case check_edges_for_wildcard(Search1) of
        {"", _IsWcAtBeg, _IsWcAtEnd} -> false;
        {Search, IsWcAtBeg, IsWcAtEnd} ->
            AllPos = allpos(MsgU, Search),
            LenMsg = length(MsgU),
            LenSea = length(Search),
            lists:any(fun(P) ->
                              is_word(MsgU, P, LenMsg, LenSea,
                                      {IsWcAtBeg, IsWcAtEnd})
                      end,
                      AllPos)
    end.

check_edges_for_wildcard(Search1) ->
    {IsWcAtBeg, Search2} =
        case Search1 of
            [$* | Tb] -> {true, Tb};
            _ -> {false, Search1}
        end,
    {IsWcAtEnd, Search} =
        case ?lrev(Search2) of
            [$* | Te] -> {true, ?lrev(Te)};
            _ -> {false, Search2}
        end,
    {Search, IsWcAtBeg, IsWcAtEnd}.

%% Return all positions in Msg where Search can be found
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

is_word(MsgU, Pos, LenMsg, LenSea, {IsWcAtBeg, IsWcAtEnd}) ->
    IsWordAtBeg = Pos == 1,
    NextPosAfterSearch = Pos + LenSea,
    LastPosInSearch = NextPosAfterSearch - 1,
    IsWordAtEnd = LenMsg == LastPosInSearch,
    IsBoundA = IsWordAtBeg orelse
        lists:member(lists:nth(Pos - 1, MsgU), ?BoundaryChars),
    IsBoundB = IsWordAtEnd orelse
        lists:member(lists:nth(NextPosAfterSearch, MsgU), ?BoundaryChars),
    (IsBoundA or IsWcAtBeg) and (IsBoundB or IsWcAtEnd).

del_start(Sid, Title, 0) ->
    Border = "",
    del_start(Sid, Title, Border);
del_start(Sid, Title, BordInt) when is_integer(BordInt) ->
    Border = " border=\"" ++ ?i2l(BordInt) ++ "\"",
    del_start(Sid, Title, Border);
del_start(Sid, Title, Border) ->
    Start = ?HTML_TAB_START(Title, Border),
    web:deliver(Sid, Start).

del_end(Sid) ->
    web:deliver(Sid, ?HTML_TAB_END).

%% -----------------------------------------------------------------------------

%% http://mafia_test.peterlund.se/e/web/game_status
%% http://mafia_test.peterlund.se/e/web/game_status?debug
%% http://mafia_test.peterlund.se/e/web/game_status?phase=day&num=1
%% http://mafia_test.peterlund.se/e/web/game_status?phase=night&num=2
%% http://mafia_test.peterlund.se/e/web/game_status?phase=end
game_status(Sid, _Env, In) ->
    Html =
        case get_phase(In) of
            {S, Phase} when S == ok; S == current ->
                game_status_out(S, Phase);
            {error, ErrorHtml} ->
                ErrorHtml
        end,
    A = del_start(Sid, "Game Status", 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    A + B + C.


game_status_out(current, Phase) ->
    UseTime = [{?use_time, mafia_time:utc_secs1970()}],
    game_status_out2(Phase, UseTime);
game_status_out(ok, Phase) ->
    %% Check that phase is not in the future
    GameKey = ?getv(?game_key),
    Time = mafia_time:utc_secs1970(),
    case mafia_time:get_time_for_phase(GameKey, Phase) of
        PhaseTime when PhaseTime =< Time ->
            game_status_out2(Phase, []);
        _ ->
            "<tr><td>Phase has not happened yet.</td></tr>"
    end.

game_status_out2(Phase, UseTime) ->
    PeriodOpts =
        case catch mafia_web:get_state() of
            {'EXIT', _} -> [];
            KVs ->
                case {lists:keyfind(timer, 1, KVs),
                      lists:keyfind(timer_minutes, 1, KVs)} of
                    {{_, TRef},
                     {_, PeriodMins}} when TRef /= ?undefined ->
                        [{?period, PeriodMins}];
                    _ -> []
                end
        end,
    mafia_print:print_votes([{?game_key, ?getv(?game_key)},
                             {?phase, Phase},
                             {?mode, ?html}
                            ]
                            ++ UseTime
                            ++ PeriodOpts).

get_phase([]) ->
    GameKey = ?getv(?game_key),
    Phase = mafia_time:calculate_phase(GameKey),
    {current, Phase};
get_phase(In) ->
    PQ = httpd:parse_query(In),
    NotAllowed = [Key || {Key, _} <- PQ] -- ["phase", "num", "debug"],
    if NotAllowed == [] ->
            gs_phase(lists:keyfind("phase", 1, PQ),
                     lists:keyfind("num", 1, PQ),
                     lists:keyfind("debug", 1, PQ)
                    );
       true ->
            {error, ["Params: ",
                     string:join(NotAllowed, ", "),
                     " not allowed."]}
    end.

%% Is this not the same as setting time offset now?
%% maybe all "debug" stuff can be removed?
gs_phase(_, _, {"debug", ""}) ->
    GameKey = ?getv(?game_key),
    case ?rgame(GameKey) of
        [] ->
            {error, ["<tr><td>Game ",
                     ?i2l(GameKey),
                     " not found!</td></tr>"]};
        [G] ->
            Page = G#mafia_game.page_to_read,
            PRec = hd(?rpage(GameKey, Page)),
            MsgId = lists:last(PRec#page_rec.message_ids),
            Msg = hd(?rmess(MsgId)),
            Time = Msg#message.time,
            MsgPhase = mafia_time:calculate_phase(GameKey, Time),
            {current, MsgPhase}
    end;
gs_phase({"phase", "end"},
            _, _) ->
    {ok, ?game_ended};
gs_phase({"phase", "day"},
            {"num", Str},
            _) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, {Num, ?day}};
        {error, _HtmlErr} = E -> E
    end;
gs_phase({"phase", "night"},
            {"num", Str},
            _) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, {Num, ?night}};
        {error, _HtmlErr} = E -> E
    end;
gs_phase(_, _, _) ->
    {error, "<tr><td>"
     "You need to end url with .../stats?phase=day&num=1, "
     "?phase=night&num=2 or ?phase=end"
     "</td></tr>"}.

%% -----------------------------------------------------------------------------

%% http://mafia_test.peterlund.se/e/web/vote_tracker?day=1
%% http://mafia_test.peterlund.se/e/web/vote_tracker?msg_id=1420335
vote_tracker(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    A = case vote_tracker2(lists:keyfind("day", 1,  PQ),
                           lists:keyfind("msg_id", 1,  PQ)) of
            {tracker, Out} ->
                del_start(Sid, "Vote Tracker", 0);
            {error, Out} ->
                del_start(Sid, "Vote Tracker Error", 1);
            Out ->
                del_start(Sid, "Vote Message", 0)
    end,
    B = web:deliver(Sid, Out),
    C = del_end(Sid),
    A + B + C.

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
        show_msg(MsgId, row_tab)
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

%% -----------------------------------------------------------------------------

%% http://mafia_test.peterlund.se/e/web/stats?phase=day&num=1
%% http://mafia_test.peterlund.se/e/web/stats?phase=night&num=1
%% http://mafia_test.peterlund.se/e/web/stats?phase=end
%% http://mafia_test.peterlund.se/e/web/stats?phase=total
stats(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    Html =
        case stats2(lists:keyfind("phase", 1,  PQ),
                    lists:keyfind("num", 1,  PQ)) of
            {ok, Phase} ->
                ["<tr><td>",
                 mafia_print:print_stats([{?game_key, ?getv(game_key)},
                                          {?phase, Phase},
                                          {?mode, ?html}
                                         ]),
                 "</td></tr>"];
            {error, ErrorHtml} -> ErrorHtml
        end,
    A = del_start(Sid, "Posting Stats", 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    A + B + C.

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
     "?phase=night&num=2, ?phase=end or  ?phase=total"
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

%% -----------------------------------------------------------------------------

%% insert row into row_tab
show_msg(MsgId, row_tab) ->
    ["<tr><td><table cellpadding=6 cellspacing=3>",
     show_msg(MsgId),
     "</table></td></tr>"].

%% ?html mode only, use mafia_print:pp for ?text
%% return tr
show_msg(MsgId) when is_integer(MsgId) ->
    show_msg(?rmess(MsgId));

show_msg([]) -> "<tr><td>No message found with this id</td></tr>";
show_msg([#message{user_name = MsgUserB,
                   page_num = PageNum,
                   time = Time,
                   message = MsgB}]) ->
    GameKey = ?getv(?game_key),
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

%% -----------------------------------------------------------------------------

p(I) when I > 9 -> ?i2l(I);
p(I) -> string:right(?i2l(I), 2, $0).
