-module(web_impl).

%% web
-export([msgs/3,
         game_status/3,
         vote_tracker/3,
         msg/3,
         stats/3,
         forum_php/3,

         show_msg/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

-define(OUT_LIMIT, 400000).

%% record used when iterating over all messages
-record(miter,
        {bytes = 0,
         limit = init,
         phase = phase,
         page = page,
         last = false
        }).

%% http://mafia.peterlund.se/e/web/msgs
msgs(Sid, _Env, In) ->
    ThId = ?getv(?game_key),
    PQ = httpd:parse_query(In),
    Url1 = ?BotUrl,
    Url2 = "e/web/msgs?",
    In3 = [string:tokens(I, "=") || I <- string:tokens(In, "&")],
    Url3 = string:join(
            [[K, "=", V] || [K, V] <- In3, K /= "button", V /= ""], "&"),
    SearchLink = ["<a href=\"", "/", Url2, Url3, "\">", Url3, "</a>"],
    IsUserOrWord = case lists:keyfind("UorW", 1, PQ) of
                       false -> false;
                       _ -> true
                   end,
    UsersText = get_arg(PQ, "user"),
    WordsText = get_arg(PQ, "word"),
    PartsText = get_arg(PQ, "part"),
    DayCond = find_part(PartsText),
    UsersU = find_word_searches(UsersText),
    WordsU = find_word_searches(WordsText),
    IsUserCond = UsersU /= [],
    IsWordCond = WordsU /= [],
    IsDayCond = DayCond /= ?undefined,
    DoCont = IsUserCond orelse IsWordCond orelse IsDayCond,
    Fun =
        fun(acc, init) -> #miter{};
           (#message{msg_id = MsgId,
                     user_name = MsgUserB,
                     page_num = Page,
                     time = Time,
                     message = MsgB},
            MI) when MI#miter.bytes < ?OUT_LIMIT  ->
                MsgPhase = mafia_time:calculate_phase(ThId, Time),
                Msg = ?b2l(MsgB),
                B2U = fun(B) -> string:to_upper(binary_to_list(B)) end,
                MsgUserU = B2U(MsgUserB),
                TestFuns =
                    [
                     %% 1. Test if any of Users in form matches MsgUser
                     fun() ->
                             not IsUserCond orelse
                                 lists:any(
                                   fun(UserU) ->
                                           is_word(MsgUserU, UserU)
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
                     fun() when not IsDayCond -> true;
                        %% need DayNum, DoN, Page, find_part
                        () ->
                             case DayCond of
                                 ?game_ended ->
                                     MsgPhase#phase.don == ?game_ended;
                                 {Ua, Na, Ub, Nb} ->
                                     IsAok =
                                         case {Ua, Na} of
                                             {_, ?undefined} -> true;
                                             {page, _} -> Page >= Na;
                                             _ ->
                                                 SPhaseA = #phase{num = Na,
                                                                  don = Ua},
                                                 SPhaseA =< MsgPhase
                                         end,
                                     IsBok =
                                         case {Ub, Nb} of
                                             {_, ?undefined} -> true;
                                             {page, _} -> Page =< Nb;
                                             _ ->
                                                 SPhaseB = #phase{num = Nb,
                                                                  don = Ub},
                                                 MsgPhase =< SPhaseB
                                         end,
                                     IsAok and IsBok
                             end
                     end],
                AllTestsOk =
                    if not IsUserOrWord ->
                            %% All must be true same time
                            lists:all(fun(F) -> F() end, TestFuns);
                       IsUserOrWord ->
                            %% Or checkbox was checked
                            [UserTest, WordTest, PartTest] = TestFuns,
                            (UserTest() or WordTest()) and PartTest()
                    end,
                if AllTestsOk ->
                        %% if not IsUserCond, not IsWordCond ->
                        DS1 = if MsgPhase /= MI#miter.phase ->
                                      mafia_print:print_phase(
                                        MsgPhase);
                                 true -> ""
                              end,
                        DS2 = if Page /= MI#miter.page ->
                                      ["Page ", ?i2l(Page)];
                                 true -> ""
                              end,
                        DivStr = if DS1 == "", DS2 == "" ->
                                         "";
                                    DS1 /= "", DS2 /= "" ->
                                         [DS1, ", ", DS2, " : ", SearchLink];
                                    true ->
                                         [DS1 ++ DS2, " : ", SearchLink]
                                 end,
                        SizeDiv = deliver_div(Sid, DivStr),
                        DayStr =
                            case MsgPhase of
                                #phase{num = DNum, don = ?day} ->
                                    "Day-" ++ ?i2l(DNum);
                                #phase{num = DNum, don = ?night} ->
                                    "Night-" ++ ?i2l(DNum);
                                #phase{don = ?game_ended} ->
                                    "Game End "
                            end,
                        MsgBoldMarked = bold_mark_words(Msg, WordsU),
                        Hash = erlang:phash2(MsgUserB, 16#1000000),
                        Color = Hash bor 16#C0C0C0,
                        ColorStr = integer_to_list(Color, 16),
                        {HH, MM} = mafia_time:hh_mm_to_deadline(ThId, Time),

                        %% Add context link when doing User/Word search
                        MsgRef = ["msg_id=", ?i2l(MsgId)],
                        HPage =
                            if IsUserCond or IsWordCond ->
                                    {Pages, _, _} = page_context(Page, 1),
                                    ["<a href=\"msgs?part=p",
                                     Pages,
                                     "#", MsgRef,
                                     "\">page ", ?i2l(Page),
                                     "</a>"];
                               true ->
                                    ["page ", ?i2l(Page)]
                            end,
                        OutB =
                            ?l2b(
                               ["<tr bgcolor=\"#", ColorStr,
                                "\"><td valign=\"top\">"
                                "<a name=\"", MsgRef, "\">"
                                "<b>", MsgUserB,
                                "</b></a><br>", DayStr, " ",
                                p(HH), ":", p(MM), "<br>",
                                HPage,
                                "</td><td valign=\"top\">",
                                MsgBoldMarked,
                                "</td></tr>\r\n"]),
                        SizeOut = web:deliver(Sid, OutB),
                        MI#miter{bytes = MI#miter.bytes + SizeDiv + SizeOut,
                                 phase = MsgPhase,
                                 page = Page,
                                 last = true};
                   true ->
                        MI#miter{last = false}
                end;

           (_, MI) when MI#miter.limit == init,
                        MI#miter.bytes >= ?OUT_LIMIT ->
                DivStr = "You have reached the MAX OUTPUT LIMIT on 400 KB! "
                    "Please refine your search...",
                SizeDiv = deliver_div(Sid, DivStr, "#ff8888"),
                MI#miter{bytes = MI#miter.bytes + SizeDiv,
                         limit = limit,
                         last = false};
           (_, MI)  ->
                MI#miter{last = false}
        end,
    A = del_start(Sid, "Mafia Search Result", 0),
    B = if DoCont ->
                TabStart = "<tr><td><table cellpadding=6 cellspacing=3>",
                Row1 = ["<tr><td colspan=\"2\" align=center>"
                        "Copy/paste URL: ", Url1, Url2, Url3,
                        "<br><br></td></tr>"],
                TabEnd = "</table></td></tr>",
                B1 = web:deliver(Sid, [TabStart, Row1]),
                #miter{bytes = B2, last = DidLast} =
                    mafia_data:iterate_all_msgs(ThId, Fun),
                SizeDiv = if DidLast ->
                                  deliver_div(Sid, "Last Message Reached");
                             true -> 0
                          end,
                B3 = web:deliver(Sid, TabEnd),
                B1 + B2 + SizeDiv + B3;
           true ->
                MsgB = ?l2b(["<tr><td valign=\"top\">",
                            "Error: Minimum one condition needs to be "
                            "specified: User name, Word or a "
                            "single Day number!",
                            "</td></tr>"]),
                web:deliver(Sid, MsgB)
        end,
    C = del_end(Sid),
    Args = [list_to_binary(K) || {K, V} <- PQ, V/=""] -- [<<"button">>],
    {A + B + C, Args}.

deliver_div(Sid, DivStr) ->
    deliver_div(Sid, DivStr, "#aaaaff").

deliver_div(_Sid, "", _Color) -> 0;
deliver_div(Sid, DivStr, Color) ->
    web:deliver(Sid,
                ["<tr bgcolor=\"", Color, "\">"
                 "<th colspan=2><font size=+1>",
                 DivStr,
                 "</font></th></tr>"]).

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
    LenMsg = length(MsgU),
    WordsU2 = get_all_words_to_mark(WordsU),
    Intervals =
        mafia_lib:merge_intervals(
          lists:sort(
            lists:foldl(
              fun(SearchU, Acc) ->
                      case check_edges_for_wildcard(SearchU) of
                          {"", _IsWcAtBeg, _IsWcAtEnd} -> Acc;
                          {SearchU2, IsWcAtBeg, IsWcAtEnd} ->
                              Len = length(SearchU2),
                              Acc ++ [{P, P + Len - 1}
                                      || P <- allpos(MsgU, SearchU2),
                                         is_word(MsgU, P, LenMsg, Len,
                                                 {IsWcAtBeg, IsWcAtEnd})
                                     ]
                      end
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
              Acc ++ [Str || Str <- string:tokens(WordU, "|"), "" /= Str]
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

find_part(Text) ->
    find_part2(?l2u(Text)).

find_part2("END"++_) -> ?game_ended;
find_part2(TextU) ->
    %% p3-n8, p1-2, n7-d8, p33-, -55
    %% default type is p=page
    Reg6 = "^\\s*((D|N|P|DAY|NIGHT|PAGE)? *([0-9]+))? *"
        "(- *((D|N|P|DAY|NIGHT|PAGE)? *([0-9]+))?)?\\s*$",
    case re:run(TextU, Reg6, [{capture, [1,2,3,4,5,6,7]}]) of
        nomatch ->
            ?undefined;
        {match, Ms} ->
            case mafia_lib:re_matches(TextU, Ms) of
                [_, Ua0, Na0, Dash, _, Ub0, Nb0] ->
                    Ua = s_unit(Ua0),
                    Ub = s_unit(Ub0),
                    Na = mk_int(Na0),
                    Nb = mk_int(Nb0),
                    if Na == ?undefined, Nb == ?undefined ->
                            ?undefined;
                       Nb == ?undefined, Dash == "-1" -> % dash missing
                            {Ua, Na, Ua, Na};
                       true ->
                            {Ua, Na, Ub, Nb}
                    end
            end
    end.

mk_int("-1") -> ?undefined;
mk_int(Str) ->
    case catch ?l2i(Str) of
        {'EXIT', _} ->
            ?undefined;
        Int -> Int
    end.

s_unit("-1") -> page;
s_unit("P") -> page;
s_unit("PAGE") -> page;
s_unit("D") -> ?day;
s_unit("DAY") -> ?day;
s_unit("N") -> ?night;
s_unit("NIGHT") -> ?night.


%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/game_status
%% http://mafia.peterlund.se/e/web/game_status?debug
%% http://mafia.peterlund.se/e/web/game_status?phase=day&num=1
%% http://mafia.peterlund.se/e/web/game_status?phase=night&num=2
%% http://mafia.peterlund.se/e/web/game_status?phase=end
game_status(Sid, _Env, In) ->
    {A, Html} =
        case get_phase(In) of
            {current, Phase} ->
                Title = ["Game Status ", mafia_print:print_phase(Phase)],
                A0 = del_start(Sid, Title, 0),
                Html0 = game_status_out(current, Phase),
                {A0, Html0};
            {ok, Phase} ->
                Title = ["History ", mafia_print:print_phase(Phase)],
                A0 = del_start(Sid, Title, 0),
                Html0 = game_status_out(ok, Phase),
                {A0, Html0};
            {error, ErrorHtml} ->
                A0 = del_start(Sid, "Game Status", 0),
                Html0 = ErrorHtml,
                {A0, Html0}
        end,
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    PQ = httpd:parse_query(In),
    Args = make_args(PQ, ["phase", "num"]),
    {A + B + C, Args}.

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
            "<tr><td>Phase has not concluded yet.</td></tr>"
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
    {ok, #phase{don = ?game_ended}};
gs_phase({"phase", "day"},
            {"num", Str},
            _) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, don = ?day}};
        {error, _HtmlErr} = E -> E
    end;
gs_phase({"phase", "night"},
            {"num", Str},
            _) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, don = ?night}};
        {error, _HtmlErr} = E -> E
    end;
gs_phase(_, _, _) ->
    {error, "<tr><td>"
     "You need to end url with .../stats?phase=day&num=1, "
     "?phase=night&num=2 or ?phase=end"
     "</td></tr>"}.

%% -----------------------------------------------------------------------------

%% http://mafia.peterlund.se/e/web/vote_tracker?day=1
%% http://mafia.peterlund.se/e/web/vote_tracker?msg_id=1420335
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
    Args = make_args(PQ, ["day"]),
    {A + B + C, Args}.

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
        show_message(MsgId, vote)
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
%% http://mafia.peterlund.se/e/web/msg?id=(msgid)&var=vote&player=(playername)
msg(Sid, _Env, In) ->
    %% msgs
    PQ = httpd:parse_query(In),
    MsgIdText = get_arg(PQ, "id"),
    Variant = get_arg(PQ, "var"),
    Player = get_arg(PQ, "player"),
    Ms = case catch ?l2i(MsgIdText) of
             {'EXIT', _} -> [];
             MsgId -> ?rmess(MsgId)
         end,
    {HStart, Html} = msg2(Ms, Variant, Player),
    A = del_start(Sid, HStart, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = make_args(PQ, ["var"]),
    {A + B + C, Args}.

msg2([], _Variant, _Player) ->
    "No message found with this id";
msg2([M], Variant, Player) ->
    case Variant of
        "death" ->
            {"Death Announcement - " ++ Player,
             show_message(M, death)};
        "replacement" ->
            {"Replacement - " ++ Player,
             show_message(M, replacement)};
        "vote" ->
            {"Vote - " ++ Player,
             show_message(M, vote)};
        _ ->
            {"Message - " ++ Player,
             show_message(M, msg)}
    end.

%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/stats?phase=day&num=1
%% http://mafia.peterlund.se/e/web/stats?phase=night&num=1
%% http://mafia.peterlund.se/e/web/stats?phase=end
%% http://mafia.peterlund.se/e/web/stats?phase=total
stats(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    Sort = case get_arg(PQ, "sort") of
               "words_per_post" ->
                   [{?sort, ?words_per_post}];
               "words" ->
                   [{?sort, ?words}];
               _ -> []
           end,
    Html =
        case stats2(lists:keyfind("phase", 1,  PQ),
                    lists:keyfind("num", 1,  PQ)) of
            {ok, Phase} ->
                ["<tr><td>",
                 mafia_print:print_stats([{?game_key, ?getv(game_key)},
                                          {?phase, Phase},
                                          {?mode, ?html}
                                         ] ++ Sort),
                 "</td></tr>"];
            {error, ErrorHtml} -> ErrorHtml
        end,
    A = del_start(Sid, "Posting Stats", 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = make_args(PQ, ["sort", "phase", "num"]),
    {A + B + C, Args}.

stats2({"phase", "total"},
       _) ->
    {ok, ?total_stats};
stats2({"phase", "end"},
       _) ->
    {ok, #phase{don = ?game_ended}};
stats2({"phase", "day"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, don = ?day}};
        {error, _HtmlErr} = E -> E
    end;
stats2({"phase", "night"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, don = ?night}};
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
forum_php(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    ThId = get_arg(PQ, "threadID"),
    UrlDisp = ["http://webdiplomacy.net/forum.php?", In],
    Url = [UrlDisp, "#", ThId],
    A = del_start(Sid, "Redirect", 0),
    B = web:deliver(
          Sid,
          ["<tr><td align=center>Redirecting you to webdiplomacy.net"
           "<p>"
           "<a href=\"", Url, "\">", UrlDisp, "</a></td></tr>"]
         ),
    C = del_end(Sid),
    {A + B + C, ?none}.

%% -----------------------------------------------------------------------------

%% insert row into row_tab
show_message(Msg, Variant) ->
    ["<tr><td><table cellpadding=6 cellspacing=3>",
     show_message2(Msg, Variant),
     "</table></td></tr>"
    ].

show_message2(MsgId, Var) when is_integer(MsgId) ->
    show_message2(?rmess(MsgId), Var);
show_message2([M], Var) ->
    show_message2(M, Var);
show_message2([], _Var) ->
    "No message found with this id";
show_message2(M, vote) ->
    show_message3(M, " where the vote is found");
show_message2(M, death) ->
    show_message3(M, " where the announcement is found");
show_message2(M, replacement) ->
    show_message3(M, " where the replacement message is found");
show_message2(M, msg) ->
    show_message3(M, "").

show_message3(M, Str) ->
    ["<tr><td><table cellpadding=6 cellspacing=3>",
     show_msg(M),
     page_links(M, Str),
     "</table></td></tr>"
    ].

page_links(MsgId, Str) when is_integer(MsgId) ->
    page_links(?rmess(MsgId), Str);
page_links([], _Str) -> "<tr><td>No message found with this id</td></tr>";
page_links([M], Str) -> page_links(M, Str);
page_links(M, Str) ->
    PageNum = M#message.page_num,
    MsgId = M#message.msg_id,
    UrlPart1 = "/e/web/msgs?part=p",
    PageStr = ?i2l(PageNum),
    {PageCont, VPPrev, VPNext} = page_context(PageNum, 1),
    LinkEnd = "#msg_id=" ++ ?i2l(MsgId),
    Link1 = UrlPart1 ++ PageStr ++ LinkEnd,
    Link3 = UrlPart1 ++ PageCont ++ LinkEnd,
    ["<tr><td colspan=2 align=center><br>"
     "<a href=\"", Link1 ,"\">Page ", PageStr, Str, "</a>"
     "<p>"
     "<a href=\"", Link3 ,"\">Pages ", VPPrev, " to ", VPNext, "</a>"
     "</td></tr>\r\n"].

page_context(PageNum, Context) ->
    VPPrev = ?i2l(max(PageNum - Context, 1)),
    VPNext = ?i2l(PageNum + Context),
    {VPPrev ++ "-" ++ VPNext, VPPrev, VPNext}.

%% ?html mode only, use mafia_print:pp for ?text
%% return tr
show_msg(MsgId) when is_integer(MsgId) ->
    show_msg(?rmess(MsgId));

show_msg([]) -> "<tr><td>No message found with this id</td></tr>";
show_msg([M]) -> show_msg(M);
show_msg(#message{user_name = MsgUserB,
                  page_num = PageNum,
                  time = Time,
                  message = MsgB}) ->
    GameKey = ?getv(?game_key),
    MsgPhase = mafia_time:calculate_phase(GameKey, Time),
    DayStr =
        case MsgPhase of
            #phase{num = DNum, don = ?day} ->
                "Day-" ++ ?i2l(DNum);
            #phase{num = DNum, don = ?night} ->
                "Night-" ++ ?i2l(DNum);
            #phase{don = ?game_ended} -> "Game End "
        end,
    Color = mafia_lib:bgcolor(MsgUserB),
    {HH, MM} = mafia_time:hh_mm_to_deadline(GameKey, Time),
    ?l2b(["<tr", Color, "><td valign=\"top\"><b>", MsgUserB,
          "</b><br>",
          DayStr, " ", p(HH), ":", p(MM),
          "<br> page ", ?i2l(PageNum),
          "</td><td valign=\"top\">", MsgB,
          "</td></tr>"
         ]).

%% ----------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ----------------------------------------------------------------------------

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

get_arg(PQ, ArgStr) ->
    case lists:keyfind(ArgStr, 1, PQ) of
        false -> "";
        {_, V} -> V
    end.

make_args(PQ, Keys) ->
    lists:foldl(fun({_K, ""}, Acc) -> Acc;
                   ({K, V}, Acc) ->
                        case lists:member(K, Keys) of
                            true ->
                                Acc ++ [?l2b(K), ?l2b(V)];
                            false  -> Acc
                        end
                end,
                [],
                PQ).

p(I) when I > 9 -> ?i2l(I);
p(I) -> string:right(?i2l(I), 2, $0).

%% ----------------------------------------------------------------------------
%% EUNIT TESTS
%% ----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

find_part_test_() ->
    [
     ?_assertMatch({page, 23, page, 88}, find_part("p23-p88")),
     ?_assertMatch({page, 23, page, 88}, find_part("23-88")),
     ?_assertMatch({night, 23, day, 88}, find_part("n23-d88")),
     ?_assertMatch({page, undefined, page, 88}, find_part("-p88")),
     ?_assertMatch({page, 23, page, undefined}, find_part("p23-")),

     ?_assertMatch({page, 23, day, 1}, find_part("page 23 - day 1")),
     ?_assertMatch({page, 23, day, 1}, find_part(" page23-day1 "))
    ].
