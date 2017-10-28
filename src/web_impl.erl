-module(web_impl).

%% web
-export([front_page/3,
         game_status/3,
         search/3,
         vote_tracker/3,
         msg/3,
         stats/3,
         'forum.php'/3,
         dst_changes/3,
         users/3,
         game_settings/3,

         show_msg/2,

         hist_link/5
        ]).

%% for other web modules
-export([del_start/3, del_end/1, get_arg/2,
         game_nums_rev_sort/0]).

%% for web_msgs
-export([get_gnum/1,
         error_resp/2,
         page_context/2,
         pr2dig/1
        ]).

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
    G = hd(?rgame(GameNum)),
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
    [RolePmSignUp, UserAliasLink, CurGameLinks] =
        case catch get_fp(G) of
            {'EXIT', _} -> ["", "", ""];
            GameLink -> GameLink
        end,
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
                             UserAliasLink,
                             CurGameLinks,
                             OldGamesHistoryLinks,
                             Post]),
    {Size, ?none}.

get_fp(G) ->
    GNum = G#mafia_game.game_num,
    GNStr = ?i2l(GNum),
    CurPhase = mafia_time:calculate_phase(G),
    RevPhases =
        case G#mafia_game.game_end of
            ?undefined ->
                ?lrev(mafia_time:phases_upto(CurPhase));
            {EndTime, _} ->
                LastPhase = mafia_time:calculate_phase(G, EndTime - 1),
                [LastPhase | ?lrev(mafia_time:phases_upto(LastPhase))]
        end,
    RolePmLink =
        if G#mafia_game.role_pm /= ?undefined ->
                [" href=\"", ?b2l(G#mafia_game.role_pm), "\""];
           true -> ""
        end,
    SignupLink =
        if G#mafia_game.signup_thid /= ?undefined ->
                SignUpThid = ?i2l(G#mafia_game.signup_thid),
                [" href=\"", mafia_lib:get_url_begin(G),
                 SignUpThid, "#", SignUpThid,
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
    UserAliasLink =
        ["<br>\r\n"
         "<table>"
         "<tr><td>"
         "<a href=/e/web/users?g=", GNStr, ">User and Aliases in M", GNStr,
         "</a>\r\n"
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
         if CurPhase#phase.ptype == ?game_ended ->
                 [["<tr ", ?BG_MED_GREEN, ">"],
                  "<td>"
                  "<a href=\"game_status?g=", GNStr, "&phase=end\">Game End</a>"
                  "</td><td>"
                  "<a href=\"stats?g=", GNStr, "&phase=end\">Game End</a>"
                  "</td><td>"
                  "</td></tr>\r\n"];
            true -> []
         end,
         [game_day_links(Ptype, GNum, DayNum)
          || #phase{num = DayNum, ptype = Ptype} <- RevPhases],
         ["<tr ", ?BG_MED_GREEN, ">"],
         "<td colspan=3 align=center>"
         "<a href=\"stats?g=", GNStr, "&phase=global\">"
         "Game Global Statistics</a>"
         "</td></tr>\r\n"
         "</table>"],
    [RolePmSignUp, UserAliasLink, CurGameLinks].

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

%% new module web_lib for the below?
hist_link(Page, GNum, Ptype, DNum) ->
    LinkF = fun(Ptype2, DNum2) -> [dptypestr(Ptype2), " ", ?i2l(DNum2)] end,
    hist_link(Page, GNum, Ptype, DNum, LinkF).

hist_link("vote_tracker" = Page, GNum, ?day, DNum, _LinkF) ->
    ["<a href=\"", Page, "?g=", ?i2l(GNum), "&day=", ?i2l(DNum), "\">Day ",
     ?i2l(DNum), "</a>"];
hist_link(Page, GNum, Ptype, DNum, LinkF) ->
    ["<a href=\"", Page, "?g=", ?i2l(GNum), "&phase=", ptypestr(Ptype),
     "&num=", ?i2l(DNum), "\">", LinkF(Ptype, DNum), "</a>"].

ptypestr(?night) -> "night";
ptypestr(?day) -> "day".

dptypestr(?night) -> "Night";
dptypestr(?day) -> "Day".

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
%% http://mafia.peterlund.se/e/web/game_status
%% http://mafia.peterlund.se/e/web/game_status?game=m28&phase=day&num=1
%% http://mafia.peterlund.se/e/web/game_status?game=m28phase=night&num=2
%% http://mafia.peterlund.se/e/web/game_status?game=m28phase=end
game_status(Sid, _Env, In) ->
    PQ = httpd:parse_query(In) -- [{[],[]}],
    NotAllowed = [Key || {Key, _} <- PQ] -- ["g", "phase", "num"],
    GNum = get_gnum(get_arg(PQ, "g")),
    game_status2(Sid, GNum, PQ, NotAllowed).

game_status2(Sid, GNum, _PQ, _) when not is_integer(GNum) ->
    error_resp(Sid, "Invalid game number");
game_status2(Sid, _GNum, _PQ, NotAllowed) when NotAllowed /= [] ->
    error_resp(Sid, ["Params not allowed: ",
                string:join(NotAllowed, ", ")]);
game_status2(Sid, {?error, _}, _PQ, _) ->
    error_resp(Sid, "Bad: g=value");
game_status2(Sid, GNum, PQ, []) ->
    game_status3(Sid, GNum, PQ, ?rgame(GNum)).

game_status3(Sid, GNum, PQ, [G]) ->
    {_IsReady, _G2, Es} =
        web_game_settings:is_ready_to_go(G, {G, []}),
    AnyError = lists:any(fun({error, _}) -> ?true; (_) -> ?false end, Es),
    if AnyError ->
            EStr = ["Game is missing some parameters to be displayed:<br>\r\n",
                    [[Txt, "<br>\r\n"] || {error, Txt} <- Es]],
            error_resp(Sid, EStr);
       not AnyError ->
            game_status4(Sid, GNum, PQ)
    end.

game_status4(Sid, GNum, PQ) ->
    PhaseStr = get_arg(PQ, "phase"),
    NumStr = get_arg(PQ, "num"),
    Html =
        case get_phase(GNum, PhaseStr, NumStr) of
            {?current, Phase} ->
                game_status_out(?current, GNum, Phase);
            {?history, Phase} ->
                game_status_out(?history, GNum, Phase);
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
game_status_out(?current, GNum, Phase) ->
    Title = ["Game Status ", mafia_print:print_phase(Phase)],
    game_status_out_current(GNum, Phase, Title);
game_status_out(?history, GNum, Phase) ->
    %% Check that phase is not in the future
    %% GNum = ?getv(?game_key),
    FileName = mafia_file:game_phase_full_fn(?html, GNum, Phase),
    game_status_out_hist(GNum, Phase, FileName, read_file(FileName)).

read_file(FileName) ->
    case file:read_file_info(FileName) of
        {ok, _} ->
            file:read_file(FileName);
        _ ->
            not_on_file
    end.

game_status_out_current(GNum, Phase, Title) ->
    Opts = [{?use_time, mafia_time:utc_secs1970()},
            {?period, game:poll_minutes(GNum)}],
    do_game_status_out(GNum, Phase, Title, Opts).

game_status_out_hist(_GNum, _Phase, _FileName, {ok, Bin}) ->
    Bin;
game_status_out_hist(GNum, Phase, FileName, _) ->
    Title = ["History ", mafia_print:print_phase(Phase)],
    Time = mafia_time:utc_secs1970(),
    case mafia_time:get_time_for_phase(GNum, Phase) of
        PhaseTime when PhaseTime =< Time, is_integer(PhaseTime) ->
            %% Normally there should be a file and this generate should not run
            game:regen_history(PhaseTime, {GNum, Phase}),
            case read_file(FileName) of
                {ok, Bin} ->
                    Bin;
                _ ->
                    %% Should not happen!
                    io:format("Still not Found ~p\n", [FileName]),
                    do_game_status_out(GNum, Phase, Title, [])
            end;
        _ ->
            CurPhase = mafia_time:calculate_phase(GNum),
            if Phase == CurPhase ->
                    CurTitle = ["Game Status ", mafia_print:print_phase(Phase)],
                    game_status_out_current(GNum, Phase, CurTitle);
               true ->
                    [?HTML_TAB_START(Title, " border=\"0\""),
                     "<tr><td align=center>Phase has not begun yet.</td></tr>",
                     ?HTML_TAB_END]
            end
    end.

do_game_status_out(GNum, Phase, Title, ExtraOpts) ->
    Opts = [{?game_key, GNum},
            {?phase, Phase},
            {?mode, ?html}
           ] ++ ExtraOpts,
    game_gen:get_html(Title, Opts).

-spec validate_phase(game_num(), #phase{}) -> error | show_phase | show_end.
validate_phase(GNum, Phase) ->
    case ?rgame(GNum) of
        [] ->
            error;
        [G] when Phase#phase.ptype == ?game_ended ->
            case G#mafia_game.game_end of
                ?undefined -> error;
                _ -> show_phase
            end;
        [#mafia_game{deadlines = DLs} = G] ->
            %% 1 does phase exist in DLs == yes
            case lists:keyfind(Phase, #dl.phase, DLs) of
                false -> error;
                #dl{time = DTime} ->
                    case G#mafia_game.game_end of
                        {EndTime, _} when DTime > EndTime ->
                            error;
                        _ ->
                            show_phase
                    end
            end
    end.

-spec get_phase(integer(), string(), string()) ->
                       {history | current, #phase{}} | {error, term()}.
get_phase(GNum, PhaseStr, NumStr) ->
    case get_phase2(GNum, PhaseStr, NumStr) of
        {error, _} = Err -> Err;
        Res = {_, Phase = #phase{}} ->
            case validate_phase(GNum, Phase) of
                show_phase ->
                    Res;
                _ ->
                    {error,
                     "<tr><td align=center>Phase does not exist</td></tr>"}
            end
    end.

-spec get_phase2(integer(), string(), string()) ->
                        {history | current, #phase{}} | {error, term()}.
get_phase2(GNum, "", "") ->
    Phase = mafia_time:calculate_phase(GNum),
    {?current, Phase};
get_phase2(_GNum, PhaseStr, NumStr) ->
    gs_phase(PhaseStr, NumStr).

gs_phase("end", _) ->
    {?history, #phase{ptype = ?game_ended}};
gs_phase("day", Str) ->
    case conv_to_num(Str) of
        {ok, Num} ->
            {?history, #phase{num = Num, ptype = ?day}};
        {error, _HtmlErr} = E ->
            E
    end;
gs_phase("night", Str) ->
    case conv_to_num(Str) of
        {ok, Num} ->
            {?history, #phase{num = Num, ptype = ?night}};
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

vote_tracker2(GNum, DayStr, _) when DayStr /= "" ->
    try
        DayNum = list_to_integer(DayStr),
        [RK, VT] = mafia_tracker:web_vote_tracker([{?game_key, GNum},
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
        MsgKey = web:str2msg_key(MsgIdStr),
        show_message(?rgame(GNum), ?rmess(MsgKey), vote)
    catch _:_ ->
            {error,
             "<tr><td>"
             "Was not able to convert msg_id value"
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
    MsgKey = web:str2msg_key(MsgIdText),
    Msg = case ?rmess(MsgKey) of
              [] -> {no_msg_found, MsgIdText};
              [Msg2 | _] -> Msg2
          end,
    {HStart, Html} = msg2(?rgame(GNum), Msg, Variant, PQ),
    A = del_start(Sid, HStart, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    Args = make_args(PQ, ["var"]),
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
                 mafia_stats:print_stats([{?game_key, GNum},
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
    {ok, #phase{ptype = ?game_ended}};
stats2({"phase", "day"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, ptype = ?day}};
        {error, _HtmlErr} = E -> E
    end;
stats2({"phase", "night"},
       {"num", Str}) ->
    case conv_to_num(Str) of
        {ok, Num} -> {ok, #phase{num = Num, ptype = ?night}};
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
                 "</td></tr>",
                 "<tr><th align=center><br>"
                 "Time Zone Abbreviations Used"
                 "</th></tr>"
                 "<tr><td align=center><table>"
                 "<tr><td>"
                 "PST/PDT - Pacific USA<br>"
                 "MST/MDT - Mountain USA<br>"
                 "CST/CDT - Central USA<br>"
                 "EST/EDT - Eastern USA<br>"
                 "GMT/BST - UK<br>"
                 "CET/CEST - Central Europe<br>"
                 "EET/EEST - Eastern Europe"
                 "</td>"
                 "</tr></table></td>"
                 "</tr>"
                ];
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
users(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    GNumStr = get_arg(PQ, "g"),
    ?dbg(GNumStr),
    {Title, Html} =
        case get_gnum2(GNumStr) of
            ?none -> users_all();
            GNum -> users_game(GNum)
        end,
    A = del_start(Sid, Title, 0),
    B = web:deliver(Sid, Html),
    C = del_end(Sid),
    {A + B + C, ?none}.

users_all() ->
    {"Existing Users and Aliases in bot DB",
     ["<tr><td align=center>"
      "<font size=-1><i>A Player must have a user id in the user DB "
      "before they can replace a player in a running game.<br>\r\n"
      "The user id must also be registered on the same site "
      "(webDip or vDip) as where the game is played.<br>\r\n"
      "Ask replacement player to send one message in the game thread "
      "in case they are not listed here <br>\r\n"
      "and want to take over a position in your game.</i></font>"
      "</td></tr>\r\n",
      "<tr><td><pre>",
      mafia:show_all_users(?return_text),
      "</pre></td></tr>"]
    }.

users_game(GNum) ->
    {"Users and Aliases in game M"++?i2l(GNum),
     ["<tr><td><pre>",
      mafia:show_game_users(?return_text, GNum),
      "</pre></td></tr>"]
    }.

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
get_gnum(V) -> get_gnum2(V).

get_gnum2("m" ++ NumStr) ->
    get_gnum2(NumStr);
get_gnum2(NumStr) ->
    case catch ?l2i(NumStr) of
        Int when is_integer(Int) ->
            Int;
        _ ->
            ?dbg({illegal_game_number, NumStr}),
            ?none
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

pr2dig(I) when I > 9 -> ?i2l(I);
pr2dig(I) when I =< 9 -> "0" ++ ?i2l(I).
