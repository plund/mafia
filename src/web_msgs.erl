-module(web_msgs).

-export([search/3,
         msgs/3,
         msg/3,
         show_message/3,
         show_msg/2
        ]).

-include("mafia.hrl").

-import(web_impl,
        [get_arg/2,
         del_start/3,
         del_end/1,
         error_resp/2
        ]).

-define(su_page, su_page).
-define(g_page, g_page).
-define(g_end, g_end).

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
    GNums = web_impl:game_nums_rev_sort(),
    Curr = mafia_db:getv(?game_key),
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
%% Respond to msgs queries
%%
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
        -- ["g", "user", "word", "part", "UorW", "button"],
    case web_impl:get_gnum2(get_arg(PQ, "g")) of
        GNum when is_integer(GNum) ->
            msgs2(Sid, ?rgame(GNum), In, PQ, NotAllowed);
        ?none ->
            error_resp(Sid, ["Bad or missing game id"])
    end.

msgs2(Sid, _, _In, _PQ, NotAllowed) when NotAllowed /= [] ->
    error_resp(Sid, ["Params not allowed: ",
                     string:join(NotAllowed, ", ")]);
msgs2(Sid, [], _In, _PQ, _)  ->
    error_resp(Sid, ["Game does not exist"]);
msgs2(Sid, [G], In, PQ, []) ->
    GNum = G#mafia_game.game_num,
    Url2 = "e/web/msgs?",
    In3 = [string:tokens(I, "=") || I <- string:tokens(In, "&")],
    Url3 = string:join(
             [[K, "=", V] || [K, V] <- In3, K /= "button", V /= ""], "&"),
    SearchLink = ["<a href=\"", "/", Url2, Url3, "\">", Url3, "</a>"],
    GnumText = "M" ++ ?i2l(GNum),
    UsersText = get_arg(PQ, "user"),
    WordsText0 =
        unicode:characters_to_list(
          list_to_binary(get_arg(PQ, "word"))),
    WordsText = mafia_lib:escapes_to_unicode(WordsText0),
    PartsText = get_arg(PQ, "part"),
    IsUserOrWord = case proplists:get_value("UorW", PQ) of
                       ?undefined -> ?false;
                       "true" -> ?true;
                       "false" -> ?false
                   end,
    Title = string:join(
              [Arg
               || Arg <- [GnumText, UsersText,
                          ?b2l(unicode:characters_to_binary(WordsText)),
                          %% Do: [226,153,164,226,153,161,226,150,160],
                          %% Not: [9828,9825,9632] and not unicode-binary
                          PartsText],
                  Arg /= ""],
              ", "),
    PartCond = find_part(PartsText),
    UsersU = find_word_searches(UsersText),
    WordsU = find_word_searches(WordsText),
    IsUserCond = UsersU /= [],
    IsWordCond = WordsU /= [],
    PartMode = if PartCond /= ?undefined ->
                       ?valid;
                  PartCond == ?undefined,
                  PartsText /= "" ->
                       ?invalid;
                  ?true -> ?undefined
               end,
    DoCont =
        PartMode /= ?invalid
        andalso
          (IsUserCond orelse IsWordCond orelse PartMode == ?valid),
    Fun =
        fun(acc, init) -> #miter{};
           (#message{msg_key = MsgKey,
                     user_name = MsgUserB,
                     page_num = Page,
                     time = Time,
                     message = MsgB} = IMsg,
            MI) when MI#miter.bytes < ?OUT_LIMIT  ->
                MsgPhase = mafia_time:calculate_phase(GNum, Time),
                Msg0 = unicode:characters_to_list(MsgB),
                Msg = mafia_lib:escapes_to_unicode(Msg0),
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

                     %% 2. Test Words
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
                        () -> is_part_ok(G, IMsg, MsgPhase, PartCond)
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
                            unicode:characters_to_binary(
                              ["<tr", BgColor, ">"
                               "<td valign=\"top\">"
                               "<a name=\"", MsgRef, "\">"
                               "<b>", MsgUserB,
                               "</b></a><br>", DayStr, " ",
                               pr2dig(HH), ":", pr2dig(MM), "<br>",
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
                InclSignup = ?true,
                #miter{bytes = B2, last = DidLast} =
                    mafia_data:iterate_all_game_msgs(GNum, InclSignup, Fun),
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

is_part_ok(G, IMsg, MsgPhase, {Ua, Na, Ub, Nb}) ->
    GThId = G#mafia_game.thread_id,
    GSuId = G#mafia_game.signup_thid,
    MThId = IMsg#message.thread_id,
    %% ?dbg({GThId, GSuId, MThId}),
    Page = IMsg#message.page_num,
    IsAok =
        fun() ->
                case {Ua, Na} of
                    {?g_end, _} ->
                        Na == ?undefined andalso
                            MsgPhase#phase.ptype == ?game_ended;
                    {_, ?undefined} ->
                        %% -xx, s-xx
                        ?true;
                    {?g_page, _} ->
                        MThId == GThId andalso Page >= Na;
                    {?su_page, _} ->
                        MThId == GThId orelse Page >= Na;
                    _ ->
                        SPhaseA = #phase{num = Na, ptype = Ua},
                        SPhaseA =< MsgPhase
                end
        end,
    IsBok =
        fun() ->
                case {Ub, Nb} of
                    {?su_page, ?undefined} ->
                        MThId == GSuId;
                    {?g_end, _} ->
                        if Ua == ?g_end -> ?true;
                           true -> MsgPhase#phase.ptype /= ?game_ended
                        end;
                    {?g_page, ?undefined} ->
                        %% xx-
                        ?true;
                    {?su_page, _} ->
                        MThId == GSuId andalso Page =< Nb;
                    {?g_page, _} ->
                        MThId == GSuId orelse Page =< Nb;
                    _ ->
                        SPhaseB = #phase{num = Nb, ptype = Ub},
                        MsgPhase =< SPhaseB
                end
        end,
    IsNotLateSuMsg =
        fun() ->
                MThId == GThId orelse
                    Ub == ?su_page orelse
                    MsgPhase#phase.ptype == ?game_start
        end,
    IsAok() andalso IsBok() andalso IsNotLateSuMsg().

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
    Parts1 = normalize_links(Msg),
    Parts2 =
        lists:foldl(
          fun({out, S}, Acc) ->
                  Acc ++ split_msg_on_urls(S);
             ({fix, _} = F, Acc) ->
                  Acc ++ [F];
             ({url, _} = F, Acc) ->
                  Acc ++ [F]
          end,
          [],
          Parts1
         ),
    Parts3 = break_long_words_in_out(Parts2),
    Parts4 = remove_same_text(Parts3),
    lists:foldl(
      fun({out, SubMsg}, Acc) ->
              Acc ++ bold_mark_words(SubMsg, WordsU);
         ({fix, Fix}, Acc) ->
              Acc ++ Fix;
         ({url, Url}, Acc) ->
              Acc ++ ["<a href=\"", Url, "\">", Url, "</a>"]
      end,
      "",
      Parts4).

remove_same_text([Url = {url, TextUrl}, Out = {fix, TextOut} | T]) ->
    case add_parenteses(TextUrl) of
        TextOut -> [Url | remove_same_text(T)];
        _ -> [Url, Out | remove_same_text(T)]
    end;
remove_same_text([H | T]) -> [H | remove_same_text(T)];
remove_same_text([]) -> [].

break_long_words_in_out(Parts) ->
    [case P of
         {out, Str} -> {out, break_long_words(Str)};
         _ -> P
     end || P <- Parts].

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

-define(H0, "<a href=\"msg?id=w:").
-define(HREF, "<a href=\"").
-define(U1, "forum.php?").
-define(R1, "http://webdiplomacy.net/forum.php?").
-define(U2, "contrib/phpBB3/viewtopic.php?").
-define(R2, "http://www.webdiplomacy.net/contrib/phpBB3/viewtopic.php?").

norm_url(?U1 ++ Msg) -> ?R1 ++ Msg;
norm_url(?U2 ++ Msg) -> ?R2 ++ Msg;
norm_url(Msg) -> Msg.

%% removes all '<a' tags
%% Only the URL part of the links are kept and marked with 'url' so they will
%% not be bold marked or interrupted with line breaks
-spec normalize_links(string()) -> [{out | url | fix, string()}].
normalize_links(Msg) ->
    normalize_links(Msg, {out, ""}, []).

%% Cite links should be unchanged - state 'keep'
normalize_links(?H0 ++ Msg, {out, B}, Acc) ->
    normalize_links(Msg, {keep, ?H0}, [{out, B} | Acc]);
normalize_links("</a>" ++ Msg, {keep, B}, Acc) ->
    normalize_links(Msg, {out, ""}, [{fix, B ++ "</a>"} | Acc]);

%% non '<a' with 'href="' - ignore link text - state 'link'
normalize_links(?HREF ++ Msg, {out, B}, Acc) ->
    {Url, Rest} = mafia_lib:split_on_url_boundary(Msg),
    NUrl = norm_url(Url),
    normalize_links(Rest, {link, ""}, [{url, NUrl}, {out, B} | Acc]);
normalize_links(">" ++ Msg, {link, _}, Acc) ->
    normalize_links(Msg, {link_text, ""}, Acc);
normalize_links("</a>" ++ Msg, {link_text, B}, Acc) ->
    normalize_links(Msg, {out, ""}, [{fix, add_parenteses(B)} | Acc]);

%% non '<a' without 'href="' - keep link text  - state 'in'
normalize_links("<a" ++ Msg, {out, B}, Acc) ->
    normalize_links(Msg, {in, B}, Acc);
normalize_links(">" ++ Msg, {in, B}, Acc) ->
    normalize_links(Msg, {out, B}, Acc);

%%<img class="smilies" src="./images/smilies/icon_e_smile.gif"
%% Add http://webdiplomacy.net/contrib/phpBB3/
normalize_links("<img " ++ Msg,
                {S, B}, Acc) ->
    normalize_links(Msg, {img, "<img "}, [{S, B} | Acc]);
normalize_links("src=\"./images" ++ Msg, {img, B}, Acc) ->
    Repl = "src=\"http://webdiplomacy.net/contrib/phpBB3/images",
    normalize_links(Msg, {img, B ++ Repl}, Acc);
normalize_links(">" ++ Msg, {img, B}, [{S, _} | _] = Acc) ->
    normalize_links(Msg, {S, ""}, [{fix, B ++ ">"} | Acc]);

normalize_links("</a>" ++ Msg, {out, B}, Acc) ->
    normalize_links(Msg, {out, B}, Acc);

normalize_links([H | T], {S, B}, Acc)
  when S == out; S == link_text; S == keep; S == img ->
    normalize_links(T, {S, B ++ [H]}, Acc);

normalize_links([_ | T], {S, B}, Acc) when S == in; S == link ->
    normalize_links(T, {S, B}, Acc);

normalize_links([], {out, B}, Acc) -> ?lrev([{out, B} | Acc]);
normalize_links([], {S, _}, Acc)
  when S == in; S == link; S == link_text ->
    ?lrev(Acc).

add_parenteses(B) -> " (" ++ B ++ ")".

%% Split string into url and no_url segments, so no bold marking is performed
%% in the url sections
-spec split_msg_on_urls(string()) -> [{out | url, string()}].
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
            Acc ++ [{out, Msg}];
        {HPos, Search} ->
            HLen = length(Search),
            Pre = string:substr(Msg, 1, HPos - 1),
            A2 = [{out, Pre} | Acc],
            Rest1 = string:substr(Msg, HPos + HLen),
            case mafia_lib:get_url(Rest1) of
                "" ->
                    ?lrev([{url, Search} | A2]);
                T1 ->
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

-define(BoundaryChars, "\s\t\r\n!\"@#€$%?&/\\|()[]{}=≈≠´`^*'™’-_.:…·,;‚„<>≥≤").

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

-define(undef(X), (X == ?undefined)).

find_part(Text) ->
    find_part2(?l2u(Text)).

find_part2(TextU) ->
    %% p3-n8, p1-2, n7-d8, p33-, -55
    %% NEW: s, s1-p1, s4-s
    %% NEW: end, p55-end = 55-
    Re = "^\\s*((S|D|N|P|DAY|NIGHT|PAGE|END)? *([0-9]*))? *"
        "(- *((S|D|N|P|DAY|NIGHT|PAGE|END)? *([0-9]*))?)?\\s*$",
    case re:run(TextU, Re, [{capture, [2, 3, 4, 6, 7], list}, unicode]) of
        nomatch ->
            ?undefined;
        {match, [Ua0, Na0, Dash, Ub0, Nb0]} ->
            Ua = s_unit(Ua0),
            Ub = s_unit(Ub0),
            Na = mk_int(Na0),
            Nb = mk_int(Nb0),
            if Ua == ?g_page, Ub == ?g_page, ?undef(Na), ?undef(Nb) ->
                    ?undefined;
               Ua == ?g_end, not ?undef(Na) ->
                    ?undefined;
               Ub == ?g_end, not ?undef(Nb) ->
                    ?undefined;
               Nb == ?undefined, Dash == "" -> % dash missing
                    {Ua, Na, Ua, Na};
               ?true ->
                    {Ua, Na, Ub, Nb}
            end
    end.

mk_int("") -> ?undefined;
mk_int(Str) ->
    case catch ?l2i(Str) of
        {'EXIT', _} ->
            ?undefined;
        Int -> Int
    end.

s_unit("") -> ?g_page;
s_unit("S") -> ?su_page;
s_unit("END") -> ?g_end;
s_unit("P") -> ?g_page;
s_unit("PAGE") -> ?g_page;
s_unit("D") -> ?day;
s_unit("DAY") -> ?day;
s_unit("N") -> ?night;
s_unit("NIGHT") -> ?night.



%% -----------------------------------------------------------------------------
%% http://mafia.peterlund.se/e/web/msg?\
%% g=24&id=(msgid)&var=vote&player=(playername)
msg(Sid, _Env, In) ->
    %% msgs
    PQ = httpd:parse_query(In),
    GNumStr = get_arg(PQ, "g"),
    GNum = web_impl:get_gnum(GNumStr),
    MsgIdText = get_arg(PQ, "id"),
    Variant = get_arg(PQ, "var"),
    MsgKey = web:str2msg_key(MsgIdText),
    Msg = case ?rmess(MsgKey) of
              [] -> {no_msg_found, MsgIdText};
              [Msg2 | _] -> Msg2
          end,
    {HStart, Html} = msg2(?rgame(GNum), Msg, Variant, PQ),
    A = del_start(Sid, HStart, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = web_impl:make_args(PQ, ["var"]),
    {A + B + C, Args}.

msg2(_, Err = {no_msg_found, MsgIdStr} , _, _) ->
    ?dbg(Err),
    {"Error",
     ["<tr><td><table cellpadding=6 cellspacing=3>",
      "No message found with this id: " ++ MsgIdStr,
      "</table></td></tr>"
     ]};
msg2([], _, _, _) ->
    {"Error",
     ["<tr><td><table cellpadding=6 cellspacing=3>",
      "Game not found",
      "</table></td></tr>"
     ]};
msg2([G], M = #message{}, Variant, PQ) ->
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
    MsgKey = M#message.msg_key,
    UrlPart1 = ["/e/web/msgs?g=", ?i2l(G#mafia_game.game_num), "&part=p"],
    PageStr = ?i2l(PageNum),
    {PageCont, VPPrev, VPNext} = page_context(PageNum, 1),
    LinkEnd = "#msg_id=" ++ web:msg_key2str(MsgKey),
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
show_msg(G = #mafia_game{site = Site}, MsgId) when is_integer(MsgId) ->
     show_msgI(G, hd(?rmess({MsgId, Site}))).

%% show_msgI(_, []) -> "<tr><td>No message found with this id</td></tr>";
%% show_msgI(G, [M]) -> show_msgI(G, M);
show_msgI(G, #message{user_name = MsgUserB,
                      page_num = PageNum,
                      time = Time,
                      message = MsgB}) ->
    MsgPhase = mafia_time:calculate_phase(G, Time),
    DayStr =
        case MsgPhase of
            #phase{num = DNum, ptype = ?day} ->
                "Day-" ++ ?i2l(DNum);
            #phase{num = DNum, ptype = ?night} ->
                "Night-" ++ ?i2l(DNum);
            #phase{ptype = ?game_start} -> "Game Start";
            #phase{ptype = ?game_ended} -> "Game End "
        end,
    Color = mafia_lib:bgcolor(MsgUserB),
    {HH, MM} = mafia_time:hh_mm_to_deadline(G, Time),
    ?l2b(["<tr", Color, "><td valign=\"top\"><b>", MsgUserB,
          "</b><br>",
          DayStr, " ", pr2dig(HH), ":", pr2dig(MM),
          "<br> page ", ?i2l(PageNum),
          "</td><td valign=\"top\">", MsgB,
          "</td></tr>"
         ]).

pr2dig(I) when I > 9 -> ?i2l(I);
pr2dig(I) when I =< 9 -> "0" ++ ?i2l(I).

%% ----------------------------------------------------------------------------
%% EUNIT TESTS
%% ----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-define(ud, undefined).

find_part_test_() ->
    [
     ?_assertMatch({?g_page, 23, ?g_page, 88}, find_part("p23-p88")),
     ?_assertMatch({?g_page, 23, ?g_page, 88}, find_part("23-88")),
     ?_assertMatch({?night, 23, ?day, 88}, find_part("n23-d88")),
     ?_assertMatch({?g_page, ?ud, ?g_page, 88}, find_part("-p88")),
     ?_assertMatch({?g_page, 23, ?g_page, ?ud}, find_part("p23-")),

     ?_assertMatch({?g_page, 23, ?day, 1}, find_part("page 23 - day 1")),
     ?_assertMatch({?g_page, 23, ?day, 1}, find_part(" page23-day1 ")),

     ?_assertMatch({?su_page, ?ud, ?su_page, ?ud}, find_part("s")),
     ?_assertMatch(?ud, find_part("send")),
     ?_assertMatch({?su_page, 3, ?su_page, ?ud}, find_part("s3-s")),
     ?_assertMatch({?g_end, ?ud, ?g_end, ?ud}, find_part("end")),
     ?_assertMatch({?g_page, ?ud, ?g_end, ?ud}, find_part("-end")),
     ?_assertMatch({?g_page, 44, ?g_end, ?ud}, find_part("44-end")),
     ?_assertMatch({?night, 7, ?g_end, ?ud}, find_part("n7-end"))
    ].
