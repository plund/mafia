-module(web_msgs).

-export([msgs/3]).

-include("mafia.hrl").

-import(web_impl,
        [get_arg/2,
         get_gnum/1,
         del_start/3,
         del_end/1,
         error_resp/2,
         page_context/2,
         pr2dig/1
        ]).

-define(OUT_LIMIT, 400000).

%% record used when iterating over all messages
-record(miter,
        {bytes = 0,
         limit = init,
         phase = phase,
         page = page,
         last = ?false
        }).

%% -----------------------------------------------------------------------------

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
    PartCond = find_part(PartsText),
    UsersU = find_word_searches(UsersText),
    WordsU = find_word_searches(WordsText),
    IsUserCond = UsersU /= [],
    IsWordCond = WordsU /= [],
    PartMode = if PartCond /= ?undefined -> ?valid;
                  PartCond == ?undefined,
                  PartsText /= "" -> ?invalid;
                  true -> ?undefined
               end,
    DoCont = PartMode /= ?invalid andalso
        (IsUserCond orelse IsWordCond orelse PartMode == ?valid),
    Fun =
        fun(acc, init) -> #miter{};
           (#message{msg_key = MsgKey,
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

                     %% 3. Test part
                     fun() when PartMode == ?undefined -> ?true;
                        () ->
                             case PartCond of
                                 ?game_ended ->
                                     MsgPhase#phase.ptype == ?game_ended;
                                 {Ua, Na, Ub, Nb} ->
                                     IsAok =
                                         case {Ua, Na} of
                                             {_, ?undefined} -> ?true;
                                             {page, _} -> Page >= Na;
                                             _ ->
                                                 SPhaseA = #phase{num = Na,
                                                                  ptype = Ua},
                                                 SPhaseA =< MsgPhase
                                         end,
                                     IsBok =
                                         case {Ub, Nb} of
                                             {_, ?undefined} -> ?true;
                                             {page, _} -> Page =< Nb;
                                             _ ->
                                                 SPhaseB = #phase{num = Nb,
                                                                  ptype = Ub},
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
                                #phase{num = DNum, ptype = ?day} ->
                                    "Day-" ++ ?i2l(DNum);
                                #phase{num = DNum, ptype = ?night} ->
                                    "Night-" ++ ?i2l(DNum);
                                #phase{ptype = ?game_start} ->
                                    "Game Start ";
                                #phase{ptype = ?game_ended} ->
                                    "Game End "
                            end,
                        ModifiedMsg = modify_message(Msg, WordsU),
                        BgColor = mafia_lib:bgcolor(MsgUserB),
                        {HH, MM} = mafia_time:hh_mm_to_deadline(GNum, Time),
                        %% Add context link when doing User/Word search
                        MsgRef = ["msg_id=", web:msg_key2str(MsgKey)],

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
                                pr2dig(HH), ":", pr2dig(MM), "<br>",
                                HPage,
                                "</td><td valign=\"top\">",
                                break_long_words(ModifiedMsg),
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

-define(MAX_WORD, 60).
-define(BRNCH, "&#8203;"). %% breaking non-character

break_long_words(Msg) ->
    dblw(Msg, ?MAX_WORD, out).

%% do_break_long_words
dblw([], _, _) -> [];
dblw([$\s | T], _, out) -> [$\s | dblw(T, ?MAX_WORD, out)];

dblw([$< | T], N, out)  -> [$< | dblw(T, N - 1, html)];
dblw([$> | T], N, html) -> [$> | dblw(T, N - 1, out)];

dblw([$& | T], N, out) -> [$\& | dblw(T, N - 1, amp)];
dblw([$; | T], N, amp) -> [$\; | dblw(T, N - 1, out)];

dblw(Str, N, out) when N =< 0 -> ?BRNCH ++ dblw(Str, ?MAX_WORD, out);
dblw([H | T], N, Mode) -> [H | dblw(T, N - 1, Mode)].


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
