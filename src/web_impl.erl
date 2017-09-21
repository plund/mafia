-module(web_impl).

%% web
-export([front_page/3,
         game_status/3,
         search/3,
         msgs/3,
         vote_tracker/3,
         msg/3,
         stats/3,
         'forum.php'/3,
         dst_changes/3,
         users/3,
         game_settings/3,

         show_msg/2
        ]).

%% for other web modules
-export([del_start/3, del_end/1, get_arg/2,
         game_nums_rev_sort/0]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------
%% show front_page
-define(CUR_START_MARK, "<!-- START CURRENT GAME SECTION -->").
-define(CUR_END_MARK, "<!-- END CURRENT GAME SECTION -->").

front_page(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    CurGameNum = mafia_db:getv(game_key),
    GameNum = get_gnum(get_arg(PQ, "g")),
    DocRoot = mafia_file:get_path(h_doc_root),
    FN = filename:join(DocRoot, "index.html"),
    {ok, Bin} = file:read_file(FN),
    Page = ?b2l(Bin),
    {_, Pre, Post1} = mafia_vote:find_parts(Page, ?CUR_START_MARK),
    {_, _Middle, Post} = mafia_vote:find_parts(Post1, ?CUR_END_MARK),
    GNStr = ?i2l(GameNum),
    G = hd(?rgame(GameNum)),
    CurPhase = mafia_time:calculate_phase(G),
    RevPhases =
        case G#mafia_game.game_end of
            ?undefined ->
                ?lrev(mafia_time:phases_upto(CurPhase));
            {EndTime, _} ->
                LastPhase = mafia_time:calculate_phase(G, EndTime - 1),
                [LastPhase | ?lrev(mafia_time:phases_upto(LastPhase))]
        end,
    GameNums = ?lrev(mafia_lib:all_keys(mafia_game)),
    Txt = fun(GN) when GN == CurGameNum -> " Current";
             (_) -> ""
          end,
    Attr = fun(GN) when GN == GameNum -> " selected";
              (_) -> ""
           end,
    POpt = fun(GN) -> ["<option value=\"", ?i2l(GN), "\"", Attr(GN),
                       ">M", ?i2l(GN), Txt(GN), "</option>\r\n"]
           end,
    Opts = ["<select name=\"g\" onchange='this.form.submit()'>>\r\n",
            [POpt(GN) || GN <- GameNums],
            "</select>"],
    Form =
        ["<table>"
         "<tr><td>",
         "<form method=get>\r\n",
         "Select Game ", Opts,
         "</form>"
         "</td></tr>"
         "</table>"],
    RolePmLink =
        if G#mafia_game.role_pm /= ?undefined ->
                [" href=\"", ?b2l(G#mafia_game.role_pm), "\""];
           true -> ""
        end,
    SignupLink =
        if G#mafia_game.signup_thid /= ?undefined ->
                [" href=\"http://webdiplomacy.net/forum.php?threadID=",
                 ?i2l(G#mafia_game.signup_thid), "#",
                 ?i2l(G#mafia_game.signup_thid),
                 "\""
                ];
           true -> ""
        end,
    RolePmSignUp =
        ["<br>\r\n"
         "<table>"
         "<tr><td>"
         "<a", RolePmLink, ">M", GNStr, " Role PM</a> | \r\n"
         "<a", SignupLink, ">M", GNStr, " Signup-thread</a>"
         "</td></tr>"
         "</table>"],
    CurGameLinks =
        ["<br>\r\n"
         "<table cellspacing=4>",
         ["<tr ", ?BG_MED_GREEN, ">"],
         "<th colspan=3>\r\n"
         "<a href=\"game_status?g=", GNStr, "\">M",
         GNStr,
         " Game Status </a>"
         "</th></tr>\r\n",
         ["<tr ", ?BG_MED_GREEN, ">"],
         "<th>History"
         "</th><th>Statistics"
         "</th><th>Vote Tracker"
         "</th></tr>\r\n",
         if CurPhase#phase.don == ?game_ended ->
                 [["<tr ", ?BG_MED_GREEN, ">"],
                  "<td>"
                  "<a href=\"game_status?g=", GNStr, "&phase=end\">Game End</a>"
                  "</td><td>"
                  "<a href=\"stats?g=", GNStr, "&phase=end\">Game End</a>"
                  "</td><td>"
                  "</td></tr>\r\n"];
            true -> []
         end,
         [game_day_links(DoN, GameNum, DayNum)
          || #phase{num = DayNum, don = DoN} <- RevPhases],
         ["<tr ", ?BG_MED_GREEN, ">"],
         "<td colspan=3 align=center>"
         "<a href=\"stats?g=", GNStr, "&phase=global\">"
         "Game Global Statistics</a>"
         "</td></tr>\r\n"
         "</table>"],
    HLinks = [["<a href=\"/m",
               ?i2l(GNum),
               "/\">M",
               ?i2l(GNum),
               "</a>"
              ] || GNum <- GameNums],
    OldGamesHistoryLinks =
        ["<table>"
         "<tr><td align=center>",
         "<br>"
         "<b>Game History for All Games</b><br>",
         string:join(HLinks, ", \r\n"),
         "</td></tr>\r\n"
         "</table>"],
    Size = web:deliver(Sid, [Pre,
                             Form,
                             RolePmSignUp,
                             CurGameLinks,
                             OldGamesHistoryLinks,
                             Post]),
    {Size, ?none}.

game_day_links(?night, GNum, DayNum) ->
    [["<tr ", ?BG_MED_GREEN, ">\r\n"],
     "<td>",
     hist_link("game_status", GNum, ?night, DayNum),
     "</td><td>",
     hist_link("stats", GNum, ?night, DayNum),
     "</td><td>"
     "</td></tr>\r\n"];
game_day_links(?day, GNum, DayNum) ->
     [["<tr ", ?BG_MED_GREEN, ">"],
     "<td>",
     hist_link("game_status", GNum, ?day, DayNum),
     "</td><td>",
     hist_link("stats", GNum, ?day, DayNum),
     "</td><td>\r\n",
     hist_link("vote_tracker", GNum, ?day, DayNum),
     "</td></tr>\r\n"
    ].

hist_link(Page, GNum, DoN, DNum) ->
    ["<a href=\"", Page, "?g=", ?i2l(GNum), "&phase=", donstr(DoN),
     "&num=", ?i2l(DNum), "\">", ddonstr(DoN), " ", ?i2l(DNum), "</a>"].

donstr(?night) -> "night";
donstr(?day) -> "day".

ddonstr(?night) -> "Night";
ddonstr(?day) -> "Day".

%% -----------------------------------------------------------------------------
%% replace game selection section in file.
%%
-define(START_MARK, "<!-- START GAME SELECTION -->").
-define(END_MARK, "<!-- END GAME SELECTION -->").

search(Sid, _Env, _In) ->
    DocRoot = mafia_file:get_path(h_doc_root),
    SearchFormFN = filename:join(DocRoot, "search_form.html"),
    {ok, MsgBin} = file:read_file(SearchFormFN),
    Msg = ?b2l(MsgBin),
    {_, Pre, Post1} = mafia_vote:find_parts(Msg, ?START_MARK),
    {_, _Pre2, Post} = mafia_vote:find_parts(Post1, ?END_MARK),
    GNums = game_nums_rev_sort(),
    Curr = mafia_db:getv(game_key),
    %% <select name="g">
    %%   <option value="27" selected>M27</option>
    %%   <option value="26">M26</option>
    %% </select>
    POpt =
        fun(GN) when GN == Curr ->
                ["<option value=\"", ?i2l(GN), "\"",
                 " selected",
                 ">M", ?i2l(GN), " Current</option>\r\n"];
           (GN) ->
                ["<option value=\"", ?i2l(GN), "\"",
                 ">M", ?i2l(GN), "</option>\r\n"]
        end,
    Opts = ["<select name=\"g\">", [POpt(GN) || GN <- GNums], "</select>"],
    Size = web:deliver(Sid, [Pre, Opts, Post]),
    {Size, ?none}.

%% -----------------------------------------------------------------------------

-define(OUT_LIMIT, 400000).

%% record used when iterating over all messages
-record(miter,
        {bytes = 0,
         limit = init,
         phase = phase,
         page = page,
         last = ?false
        }).

%% http://mafia.peterlund.se/e/web/msgs
msgs(Sid, _Env, In) ->
    PQ = httpd:parse_query(In) -- [{[],[]}],
    NotAllowed = [Key || {Key, _} <- PQ]
        -- ["g", "user", "word", "part", "UorW", "signup", "button"],
    GNum = get_gnum(get_arg(PQ, "g")),
    msgs2(Sid, GNum, In, PQ, NotAllowed).

msgs2(Sid, _GNum, _In, _PQ, NotAllowed) when NotAllowed /= [] ->
    error_resp(Sid, ["Params not allowed: ",
                string:join(NotAllowed, ", ")]);
msgs2(Sid, {?error, _}, _In, _PQ, _)  ->
    error_resp(Sid, "Bad: g=value");
msgs2(Sid, GNum, In, PQ, []) ->
    GThId = mafia:game_thread(GNum),
    Url2 = "e/web/msgs?",
    In3 = [string:tokens(I, "=") || I <- string:tokens(In, "&")],
    Url3 = string:join(
            [[K, "=", V] || [K, V] <- In3, K /= "button", V /= ""], "&"),
    SearchLink = ["<a href=\"", "/", Url2, Url3, "\">", Url3, "</a>"],
    GnumText = "M" ++ ?i2l(GNum),
    UsersText = get_arg(PQ, "user"),
    WordsText = get_arg(PQ, "word"),
    PartsText = get_arg(PQ, "part"),
    IsUserOrWord = case proplists:get_value("UorW", PQ) of
                       ?undefined -> ?false;
                       "true" -> ?true;
                       "false" -> ?false
                   end,
    DoSignup = case proplists:get_value("signup", PQ) of
                   ?undefined -> ?false;
                   "true" -> ?true;
                   "false" -> ?false
               end,
    SignupText = if DoSignup -> "+signup"; true -> "" end,
    Title = string:join([Arg
                         || Arg <- [GnumText, UsersText, WordsText,
                                    PartsText, SignupText],
                            Arg /= ""],
                        ", "),
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
                     thread_id = MThId,
                     user_name = MsgUserB,
                     page_num = Page,
                     time = Time,
                     message = MsgB},
            MI) when MI#miter.bytes < ?OUT_LIMIT  ->
                MsgPhase = mafia_time:calculate_phase(GNum, Time),
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
                     fun() when not IsDayCond -> ?true;
                        %% need DayNum, DoN, Page, find_part
                        () ->
                             case DayCond of
                                 ?game_ended ->
                                     MsgPhase#phase.don == ?game_ended;
                                 {Ua, Na, Ub, Nb} ->
                                     IsAok =
                                         case {Ua, Na} of
                                             {_, ?undefined} -> ?true;
                                             {page, _} -> Page >= Na;
                                             _ ->
                                                 SPhaseA = #phase{num = Na,
                                                                  don = Ua},
                                                 SPhaseA =< MsgPhase
                                         end,
                                     IsBok =
                                         case {Ub, Nb} of
                                             {_, ?undefined} -> ?true;
                                             {page, _} -> Page =< Nb;
                                             _ ->
                                                 SPhaseB = #phase{num = Nb,
                                                                  don = Ub},
                                                 MsgPhase =< SPhaseB
                                         end,
                                     (MThId /= GThId) or (IsAok and IsBok)
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
                                 ?true -> ""
                              end,
                        DS2 = if Page /= MI#miter.page ->
                                      ["Page ", ?i2l(Page)];
                                 ?true -> ""
                              end,
                        DivStr = if DS1 == "", DS2 == "" ->
                                         "";
                                    DS1 /= "", DS2 /= "" ->
                                         [DS1, ", ", DS2, " : ", SearchLink];
                                    ?true ->
                                         [DS1 ++ DS2, " : ", SearchLink]
                                 end,
                        SizeDiv = deliver_div(Sid, DivStr),
                        DayStr =
                            case MsgPhase of
                                #phase{num = DNum, don = ?day} ->
                                    "Day-" ++ ?i2l(DNum);
                                #phase{num = DNum, don = ?night} ->
                                    "Night-" ++ ?i2l(DNum);
                                #phase{don = ?game_start} ->
                                    "Game Start ";
                                #phase{don = ?game_ended} ->
                                    "Game End "
                            end,
                        ModifiedMsg = modify_message(Msg, WordsU),
                        BgColor = mafia_lib:bgcolor(MsgUserB),
                        {HH, MM} = mafia_time:hh_mm_to_deadline(GNum, Time),
                        %% Add context link when doing User/Word search
                        MsgRef = ["msg_id=", ?i2l(MsgId)],

                        {Pages, _, _} = page_context(Page, 1),
                        HPage =
                            ["<a href=\"msgs"
                             "?g=", ?i2l(GNum),
                             "&part=p", Pages,
                             "#", MsgRef,
                             "\">page ", ?i2l(Page),
                             "</a>"],
                        OutB =
                            ?l2b(
                               ["<tr", BgColor, ">"
                                "<td valign=\"top\">"
                                "<a name=\"", MsgRef, "\">"
                                "<b>", MsgUserB,
                                "</b></a><br>", DayStr, " ",
                                p(HH), ":", p(MM), "<br>",
                                HPage,
                                "</td><td valign=\"top\">",
                                ModifiedMsg,
                                "</td></tr>\r\n"]),
                        SizeOut = web:deliver(Sid, OutB),
                        MI#miter{bytes = MI#miter.bytes + SizeDiv + SizeOut,
                                 phase = MsgPhase,
                                 page = Page,
                                 last = ?true};
                   ?true ->
                        MI#miter{last = ?false}
                end;

           (_, MI) when MI#miter.limit == init,
                        MI#miter.bytes >= ?OUT_LIMIT ->
                DivStr = "You have reached the MAX OUTPUT LIMIT on 400 KB! "
                    "Please refine your search...",
                SizeDiv = deliver_div(Sid, DivStr, "#ff8888"),
                MI#miter{bytes = MI#miter.bytes + SizeDiv,
                         limit = limit,
                         last = ?false};
           (_, MI)  ->
                MI#miter{last = ?false}
        end,
    A = del_start(Sid, Title, 0),
    B = if DoCont ->
                TabStart = "<tr><td><table cellpadding=6 cellspacing=3>",
                Row1 = ["<tr><td colspan=\"2\" align=center>"
                        "Copy/paste URL: ", ?BotUrl, Url2, Url3,
                        "<br><br></td></tr>"],
                TabEnd = "</table></td></tr>",
                B1 = web:deliver(Sid, [TabStart, Row1]),
                #miter{bytes = B2, last = DidLast} =
                    mafia_data:iterate_all_game_msgs(GNum, DoSignup, Fun),
                SizeDiv = if DidLast ->
                                  deliver_div(Sid, "Last Message Reached");
                             ?true -> 0
                          end,
                B3 = web:deliver(Sid, TabEnd),
                B1 + B2 + SizeDiv + B3;
           ?true ->
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
       ?true -> QStrs
    end.

%% 1) fixes http links
%% 2) bold marks search words
modify_message(Msg, WordsU) ->
    lists:foldl(
      fun({no_url, SubMsg}, Acc) ->
              Acc ++ bold_mark_words(SubMsg, WordsU);
         ({url, Url}, Acc) ->
              Acc ++ ["<a href=\"", Url, "\">", Url, "</a>"]
      end,
      "",
      split_msg_on_urls(remove_links(Msg))).

bold_mark_words(Msg, WordsU) ->
    MsgU = ?l2u(Msg),
    LenMsg = length(MsgU),
    WordsU2 = get_all_words_to_mark(WordsU),
    RawIntervals =
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
          WordsU2),
    Intervals = mafia_lib:merge_intervals(lists:sort(RawIntervals)),
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

%% removes <a> tags
remove_links(Msg) -> remove_links(Msg, out).

remove_links("<a href=\"forum.php?" ++ Msg, out) ->
    "<a href=\"forum.php?" ++ remove_links(Msg, keep);
remove_links("<a" ++ Msg, out) -> remove_links(Msg, in);
remove_links(">" ++ Msg, in) -> remove_links(Msg, out);
remove_links("</a>" ++ Msg, out) -> remove_links(Msg, out);
remove_links("</a>" ++ Msg, keep) -> "</a>" ++ remove_links(Msg, out);
remove_links([H|T], S) when S == out; S == keep ->
    [H | remove_links(T, S)];
remove_links([_|T], in) -> remove_links(T, in);
remove_links([], _) -> [].

-define(UrlBoundaryChars, " ()[]{}<>").

-spec split_msg_on_urls(Msg :: string())
                       -> [{url | no_url, SubMsg :: string()}].
split_msg_on_urls(Msg) ->
    split_msg_on_urls(Msg, []).

split_msg_on_urls(Msg, Acc) ->
    %% find "http://" or "https://"
    S1 = "http://",
    S2 = "https://",
    Res =
        case {string:str(Msg, S1), string:str(Msg, S2)} of
            {0, 0} -> ?nomatch;
            {P1, 0} -> {P1, S1};
            {0, P2} -> {P2, S2};
            {P1, P2} when P1 < P2 -> {P1, S1};
            {_, P2} -> {P2, S2}
        end,
    case Res of
        ?nomatch ->
            ?lrev([{no_url, Msg}|Acc]);
        {HPos, Search} ->
            HLen = length(Search),
            Pre = string:substr(Msg, 1, HPos - 1),
            A2 = [{no_url, Pre} | Acc],
            Rest1 = string:substr(Msg, HPos + HLen),
            case string:tokens(Rest1, ?UrlBoundaryChars) of
                [] ->
                    ?lrev([{url, Search} | A2]);
                [T1|_] ->
                    A3 = [{url, Search ++ T1} | A2],
                    Msg2 = string:substr(Rest1, 1 + length(T1)),
                    split_msg_on_urls(Msg2, A3)
            end
    end.

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
        {"", _IsWcAtBeg, _IsWcAtEnd} -> ?false;
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

%% does search start or end with *
check_edges_for_wildcard(Search1) ->
    {IsWcAtBeg, Search2} =
        case Search1 of
            [$* | Tb] -> {?true, Tb};
            _ -> {?false, Search1}
        end,
    {IsWcAtEnd, Search} =
        case ?lrev(Search2) of
            [$* | Te] -> {?true, ?lrev(Te)};
            _ -> {?false, Search2}
        end,
    {Search, IsWcAtBeg, IsWcAtEnd}.

%% Return all positions in Msg where Search can be found
-spec allpos(MsgU :: string(), Search :: string())
            -> [pos_integer()].
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

-define(BoundaryChars, " !\"@#€$%?&/\\|()[]{}=≈≠´`^*'™’-_.:…·,;‚„<>≥≤").

%% returns boolean()
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
                       ?true ->
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
%% http://mafia.peterlund.se/e/web/game_status?game=m28&phase=day&num=1
%% http://mafia.peterlund.se/e/web/game_status?game=m28phase=night&num=2
%% http://mafia.peterlund.se/e/web/game_status?game=m28phase=end
game_status(Sid, _Env, In) ->
    PQ = httpd:parse_query(In) -- [{[],[]}],
    NotAllowed = [Key || {Key, _} <- PQ] -- ["g", "phase", "num"],
    GameKey = get_gnum(get_arg(PQ, "g")),
    game_status2(Sid, GameKey, PQ, NotAllowed).

game_status2(Sid, GNum, _PQ, _) when not is_integer(GNum) ->
    error_resp(Sid, "Invalid game number");
game_status2(Sid, _GameKey, _PQ, NotAllowed) when NotAllowed /= [] ->
    error_resp(Sid, ["Params not allowed: ",
                string:join(NotAllowed, ", ")]);
game_status2(Sid, {?error, _}, _PQ, _) ->
    error_resp(Sid, "Bad: g=value");
game_status2(Sid, GameKey, PQ, []) ->
    game_status3(Sid, GameKey, PQ, ?rgame(GameKey)).

game_status3(Sid, GameKey, PQ, [G]) ->
    {_IsReady, _G2, Es} =
        web_game_settings:is_ready_to_go(G, {G, []}),
    AnyError = lists:any(fun({error, _}) -> ?true; (_) -> ?false end, Es),
    if AnyError ->
            EStr = ["Game is missing some parameters to be displayed:<br>\r\n",
                    [[Txt, "<br>\r\n"] || {error, Txt} <- Es]],
            error_resp(Sid, EStr);
       not AnyError ->
            game_status4(Sid, GameKey, PQ)
    end.

game_status4(Sid, GameKey, PQ) ->
    PhaseStr = get_arg(PQ, "phase"),
    NumStr = get_arg(PQ, "num"),
    Html =
        case get_phase(GameKey, PhaseStr, NumStr) of
            {?current, Phase} ->
                game_status_out(?current, GameKey, Phase);
            {?history, Phase} ->
                game_status_out(?history, GameKey, Phase);
            {?error, ErrorHtml} ->
                [?HTML_TAB_START("Game Status", " border=\"0\""),
                 ErrorHtml,
                 ?HTML_TAB_END]
        end,
    web:deliver(Sid, ""), %% No special headers
    NumBytes = web:deliver(Sid, Html),
    Args = make_args(PQ, ["phase", "num"]),
    {NumBytes, Args}.

-spec game_status_out(?history | ?current, integer(), #phase{})
                     -> [ok | string()].
game_status_out(?current, GameKey, Phase) ->
    Title = ["Game Status ", mafia_print:print_phase(Phase)],
    game_status_out_current(GameKey, Phase, Title);
game_status_out(?history, GameKey, Phase) ->
    %% Check that phase is not in the future
    %% GameKey = ?getv(?game_key),
    FileName = mafia_file:game_phase_full_fn(?html, GameKey, Phase),
    game_status_out_hist(GameKey, Phase, FileName, read_file(FileName)).

read_file(FileName) ->
    case file:read_file_info(FileName) of
        {ok, _} ->
            file:read_file(FileName);
        _ ->
            not_on_file
    end.

game_status_out_current(GameKey, Phase, Title) ->
    Opts = [{?use_time, mafia_time:utc_secs1970()},
            {?period, mafia_time:timer_minutes(GameKey)}],
    do_game_status_out(GameKey, Phase, Title, Opts).

game_status_out_hist(_GameKey, _Phase, _FileName, {ok, Bin}) ->
    Bin;
game_status_out_hist(GameKey, Phase, FileName, _) ->
    Title = ["History ", mafia_print:print_phase(Phase)],
    Time = mafia_time:utc_secs1970(),
    case mafia_time:get_time_for_phase(GameKey, Phase) of
        PhaseTime when PhaseTime =< Time, is_integer(PhaseTime) ->
            %% Normally there should be a file and this generate should not run
            mafia_web:regen_history(PhaseTime, {GameKey, Phase}),
            case read_file(FileName) of
                {ok, Bin} ->
                    Bin;
                _ ->
                    %% Should not happen!
                    io:format("Still not Found ~p\n", [FileName]),
                    do_game_status_out(GameKey, Phase, Title, [])
            end;
        _ ->
            [?HTML_TAB_START(Title, " border=\"0\""),
             "<tr><td>Phase has not concluded yet.</td></tr>"
             ?HTML_TAB_END]
    end.

do_game_status_out(GameKey, Phase, Title, ExtraOpts) ->
    Opts = [{?game_key, GameKey},
            {?phase, Phase},
            {?mode, ?html}
           ] ++ ExtraOpts,
    mafia_web:get_html(Title, Opts).

-spec get_phase(integer(), string(), string) ->
                       {history | current, #phase{}} | {error, term()}.
get_phase(GameKey, "", "") ->
    Phase = mafia_time:calculate_phase(GameKey),
    CurGameNum = mafia_db:getv(game_key),
    if GameKey == CurGameNum ->
            {?current, Phase};
       ?true ->
            {?history, Phase}
    end;
get_phase(_GameKey, PhaseStr, NumStr) ->
    gs_phase(PhaseStr, NumStr).

gs_phase("end", _) ->
    {?history, #phase{don = ?game_ended}};
gs_phase("day", Str) ->
    case conv_to_num(Str) of
        {ok, Num} ->
            {?history, #phase{num = Num, don = ?day}};
        {error, _HtmlErr} = E ->
            E
    end;
gs_phase("night", Str) ->
    case conv_to_num(Str) of
        {ok, Num} ->
            {?history, #phase{num = Num, don = ?night}};
        {error, _HtmlErr} = E ->
            E
    end;
gs_phase(_, _) ->
    {error, "<tr><td>"
     "You need to end url with .../stats?phase=day&num=1, "
     "?phase=night&num=2 or ?phase=end"
     "</td></tr>"}.

%% -----------------------------------------------------------------------------

%% http://mafia.peterlund.se/e/web/vote_tracker?day=1
%% http://mafia.peterlund.se/e/web/vote_tracker?msg_id=1420335
vote_tracker(Sid, _Env, In) ->
    PQ = httpd:parse_query(In) -- [{[],[]}],
    GameNumStrArg = get_arg(PQ, "g"),
    GNum = get_gnum(GameNumStrArg),
    GameNumStr = ?i2l(GNum),
    A = case vote_tracker2(GNum,
                           get_arg(PQ, "day"),
                           get_arg(PQ, "msg_id")) of
            {tracker, Out} ->
                Title = "Vote Tracker M" ++ GameNumStr,
                del_start(Sid, Title, 0);
            {error, Out} ->
                del_start(Sid, "Vote Tracker Error", 1);
            Out ->
                del_start(Sid, "Vote Message", 0)
    end,
    B = web:deliver(Sid, Out),
    C = del_end(Sid),
    Args = make_args(PQ, ["day"]),
    {A + B + C, Args}.

vote_tracker2(GameKey, DayStr, _) when DayStr /= "" ->
    try
        DayNum = list_to_integer(DayStr),
        [RK, VT] = mafia_print:web_vote_tracker([{?game_key, GameKey},
                                                 {?day, DayNum}]),
        {tracker, ?l2b(["<tr><td>", RK, "</td></tr>",
                        "<tr><td>", VT, "</td></tr>"
                       ])}
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert day value to integer"
             "</td></tr>"}
    end;
vote_tracker2(GNum, "", MsgIdStr) when MsgIdStr /= "" ->
    try
        MsgId = list_to_integer(MsgIdStr),
        show_message(?rgame(GNum), ?rmess(MsgId), vote)
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert msg_id value to integer"
             "</td></tr>"}
    end;
vote_tracker2(_, "", "") ->
    {error, "<tr><td>bad_request</td></tr>"}.

%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/msg?\
%% g=24&id=(msgid)&var=vote&player=(playername)
msg(Sid, _Env, In) ->
    %% msgs
    PQ = httpd:parse_query(In),
    GNumStr = get_arg(PQ, "g"),
    GNum = get_gnum(GNumStr),
    MsgIdText = get_arg(PQ, "id"),
    Variant = get_arg(PQ, "var"),
    Ms = case catch ?l2i(MsgIdText) of
             {'EXIT', _} -> [];
             MsgId -> ?rmess(MsgId)
         end,
    {HStart, Html} = msg2(?rgame(GNum), Ms, Variant, PQ),
    A = del_start(Sid, HStart, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = make_args(PQ, ["var"]),
    {A + B + C, Args}.

msg2([], _, _Variant, _PQ) ->
    "Game not found";
msg2(_, [], _Variant, _PQ) ->
    "No message found with this id";
msg2([G], [M], Variant, PQ) ->
    case Variant of
        "death" ->
            Player = get_arg(PQ, "player"),
            {"Death Announcement - " ++ Player,
             show_message(G, M, death)};
        "replacement" ->
            Player = get_arg(PQ, "player"),
            {"Replacement - " ++ Player,
             show_message(G, M, replacement)};
        "vote" ->
            Player = get_arg(PQ, "player"),
            {"Vote - " ++ Player,
             show_message(G, M, vote)};
        "last_msg" ->
            Phase = get_arg(PQ, "phase"),
            User = get_arg(PQ, "user"),
            {"Last Message - " ++ Phase ++ " - " ++ User,
             show_message(G, M, msg)};
        _ ->
            Player = get_arg(PQ, "player"),
            {"Message - " ++ Player,
             show_message(G, M, msg)}
    end.

%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/stats?phase=day&num=1
%% http://mafia.peterlund.se/e/web/stats?phase=night&num=1
%% http://mafia.peterlund.se/e/web/stats?phase=end
%% http://mafia.peterlund.se/e/web/stats?phase=total
stats(Sid, _Env, In) ->
    PQ = httpd:parse_query(In) -- [{[],[]}],
    GNum = get_gnum(get_arg(PQ, "g")),
    PhType = get_arg(PQ, "phase"),
    Num = get_arg(PQ, "num"),
    GnumText = "M" ++ ?i2l(GNum),
    Title = string:join([Arg
                         || Arg <- ["Stats", GnumText, PhType, Num],
                            Arg /= ""],
                        ", "),
    Sort = case get_arg(PQ, "sort") of
               "words" ->
                   [{?sort, ?sort_words}];
               "words_per_post" ->
                   [{?sort, ?sort_words_per_post}];
               "last_msg_time" ->
                   [{?sort, ?sort_last_msg_time}];
               _ -> []
           end,
    Html =
        case stats2(lists:keyfind("phase", 1,  PQ),
                    lists:keyfind("num", 1,  PQ)) of
            {ok, Phase} ->
                ["<tr><td>",
                 mafia_print:print_stats([{?game_key, GNum},
                                          {?phase, Phase},
                                          {?mode, ?html}
                                         ] ++ Sort),
                 "</td></tr>"];
            {error, ErrorHtml} -> ErrorHtml
        end,
    A = del_start(Sid, Title, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = make_args(PQ, ["sort", "phase", "num"]),
    {A + B + C, Args}.

stats2({"phase", PhType},
       _) when PhType == "total"; PhType == "global" ->
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
     "?phase=night&num=2, ?phase=end or  ?phase=global"
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

'forum.php'(Sid, _Env, In) ->
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

dst_changes(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    CAbbr = get_arg(PQ, "country"),
    Year =
        case get_arg(PQ, "year") of
            "" ->
                {{Y1, _, _}, _} = calendar:universal_time(),
                Y1;
            Y1 ->
                ?l2i(Y1)
        end,
    NextYear = Year + 10,
    Years = lists:seq(Year, NextYear - 1),
    CAbbrevs = [?a2l(C) || C <- mafia_time:dst_change_date()],
    IsCAbbrOk = lists:member(CAbbr, CAbbrevs),
    Body =
        case {CAbbr, IsCAbbrOk} of
            {"", ?false} ->
                %% show form with list of countries (and year entry)
                ["<tr><td align=center>Daylight Saving Time Zones:"
                 "</td></tr>",
                 [["<tr><td align=center>",
                   "<a href=\"?country=", ?a2l(C), "\">",
                   mafia_time:dst_name(C), "</a>"
                   "</td></tr>"]
                  || C <- mafia_time:dst_change_date()],
                 "<tr><td align=center>None"
                 "</td></tr>"];
            {_, ?true} ->
                CAtom = ?l2a(CAbbr),
                Country = mafia_time:dst_name(CAtom),
                P2 = fun(I) -> string:right(?i2l(I), 2, $0) end,
                PrDT = fun({Y, M, D}) ->
                               io_lib:format("~p-~s-~s",
                                             [Y, P2(M), P2(D)])
                       end,
                ToD =
                    fun(Y) ->
                            DT = mafia_time:dst_change_date(CAtom,
                                                            Y,
                                                            to_dst),
                            PrDT(DT)
                    end,
                ToN =
                    fun(Y) ->
                            DT = mafia_time:dst_change_date(CAtom,
                                                            Y,
                                                            to_normal),
                            PrDT(DT)
                    end,
                %% show rules for dst_changes
                [ToDst, ToNormal] = mafia_time:dst_change_date(CAtom),
                DoRev = ToD(Year) > ToN(Year),
                ["<tr><td align=center>"
                 "<table>"
                 "<tr><td><b>Country/Region:</b> ", Country, "</td></tr>"
                 "<tr><td><b>DST Start:</b> ", ToDst, "</td></tr>"
                 "<tr><td><b>DST End:</b> ", ToNormal, "</td></tr>"
                 "</table>"
                 "<table>",
                 if not DoRev ->
                         "<tr><th>DST Start</th><th>DST End</th></tr>";
                    DoRev ->
                         "<tr><th>DST End</th><th>DST Start</th></tr>"
                 end,
                 [["<tr><td>",
                 if not DoRev ->
                         [ToD(Y), "</td><td>", ToN(Y)];
                    DoRev ->
                         [ToN(Y), "</td><td>", ToD(Y)]
                 end,
                   "</td></tr>"]
                  || Y <- Years],
                 "</table>"
                 "</td></tr>",
                 if NextYear < 2200 ->
                         ["<tr><td align=center><a href=\"?country=", CAbbr,
                          "&year=", ?i2l(NextYear), "\">More</a>"
                          "</td></tr>"];
                    ?true -> []
                 end
                 ]
        end,
    A = del_start(Sid, "DST Changes", 0),
    B = web:deliver(Sid, Body),
    C = del_end(Sid),
    {A + B + C, ?none}.

%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/users
users(Sid, _Env, _In) ->
    A = del_start(Sid, "Existing Users and Aliases in bot DB", 0),
    Html = ["<tr><td><font size=-1><i>Players must exist in the user DB before "
            "they can replace a player in game.<br>\r\n"
            "Ask replacement player to send one message in game if they "
            "are not listed here</i></font></td></tr>\r\n",
            "<tr><td><pre>",
            mafia:show_all_users(?return_text),
            "</pre></td></tr>"],
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    {A + B + C, ?none}.

%% -----------------------------------------------------------------------------

game_settings(Sid, Env, In) ->
    web_game_settings:game_settings(Sid, Env, In).

%% -----------------------------------------------------------------------------

%% insert row into row_tab
show_message([], _, _) ->
    "Game not found";
show_message([G], Msg, Variant) ->
    show_message(G, Msg, Variant);
show_message(_, [], _) ->
    "Message not found";
show_message(G, [M], Variant) ->
    show_message(G, M, Variant);
show_message(G = #mafia_game{}, M = #message{}, Variant) ->
    ["<tr><td><table cellpadding=6 cellspacing=3>",
     show_message2(G, M, Variant),
     "</table></td></tr>"
    ].

show_message2(G, M, vote) ->
    show_message3(G, M, " where the vote is found");
show_message2(G, M, death) ->
    show_message3(G, M, " where the announcement is found");
show_message2(G, M, replacement) ->
    show_message3(G, M, " where the replacement message is found");
show_message2(G, M, msg) ->
    show_message3(G, M, "").

show_message3(G, M, Str) ->
    ["<tr><td><table cellpadding=6 cellspacing=3>",
     show_msg(G, M),
     page_links(G, M, Str),
     "</table></td></tr>"
    ].

page_links(G, M, Str) ->
    PageNum = M#message.page_num,
    MsgId = M#message.msg_id,
    UrlPart1 = ["/e/web/msgs?g=", ?i2l(G#mafia_game.game_num), "&part=p"],
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

show_msg(G = #mafia_game{}, M = #message{}) ->
    show_msgI(G, M);
show_msg(G = #mafia_game{}, MsgId) when is_integer(MsgId) ->
     show_msgI(G, hd(?rmess(MsgId))).

%% show_msgI(_, []) -> "<tr><td>No message found with this id</td></tr>";
%% show_msgI(G, [M]) -> show_msgI(G, M);
show_msgI(G, #message{user_name = MsgUserB,
                         page_num = PageNum,
                         time = Time,
                         message = MsgB}) ->
    MsgPhase = mafia_time:calculate_phase(G, Time),
    DayStr =
        case MsgPhase of
            #phase{num = DNum, don = ?day} ->
                "Day-" ++ ?i2l(DNum);
            #phase{num = DNum, don = ?night} ->
                "Night-" ++ ?i2l(DNum);
            #phase{don = ?game_start} -> "Game Start";
            #phase{don = ?game_ended} -> "Game End "
        end,
    Color = mafia_lib:bgcolor(MsgUserB),
    {HH, MM} = mafia_time:hh_mm_to_deadline(G, Time),
    ?l2b(["<tr", Color, "><td valign=\"top\"><b>", MsgUserB,
          "</b><br>",
          DayStr, " ", p(HH), ":", p(MM),
          "<br> page ", ?i2l(PageNum),
          "</td><td valign=\"top\">", MsgB,
          "</td></tr>"
         ]).

%% ----------------------------------------------------------------------------
%% API for other modules
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
        ?false -> "";
        {_, V} -> V
    end.

game_nums_rev_sort() -> ?lrev(lists:sort(mnesia:dirty_all_keys(mafia_game))).

%% ----------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ----------------------------------------------------------------------------

get_gnum("") -> ?getv(?game_key);
get_gnum("m" ++ NumStr) ->
    get_gnum(NumStr);
get_gnum(NumStr) ->
    case catch ?l2i(NumStr) of
        Int when is_integer(Int) ->
            Int;
        _ ->
            ?dbg({illegal_game_number, NumStr})
    end.

error_resp(Sid, Specific) ->
    Html = [?HTML_TAB_START("Game Status", " border=\"0\""),
            ["<tr align=center><td>", Specific, "</td></tr>"],
            ?HTML_TAB_END],
    web:deliver(Sid, ""), %% No special headers
    NumBytes = web:deliver(Sid, Html),
    Args = [],
    {NumBytes, Args}.

make_args(PQ, Keys) ->
    lists:foldl(fun({_K, ""}, Acc) -> Acc;
                   ({K, V}, Acc) ->
                        case lists:member(K, Keys) of
                            ?true ->
                                Acc ++ [?l2b(K), ?l2b(V)];
                            ?false  -> Acc
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
