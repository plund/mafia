-module(mafia_stats).

%% Manual API
-export([print_stats/0, print_stats/1, print_stats/2]).

%% mafia_print
-export([print_statsI/1,
         sort_link/1
        ]).

-import(mafia_print,
        [po/2,
         setup_pp/1,
         pr_phase_long/1,
         print_time_5d_str/2,
         object_rows/1,
         object_rows_text/2,
         ptype_arg/1,
         ul/2
        ]).

-import(mafia_lib, [bgcolor/1]).

-include("mafia.hrl").
-include("mafia_print.hrl").

%% Manual API
print_stats() -> print_stats_opts([]).

print_stats(e) -> print_stats_opts([{?phase, #phase{ptype = ?game_ended}}]);
print_stats(Opts) when is_list(Opts) -> print_stats_opts(Opts).

print_stats(Num, Ptype) ->
    Ptype2 = ptype_arg(Ptype),
    print_stats_opts([{?phase, #phase{num = Num, ptype = Ptype2}}]).

print_stats_opts(Opts) ->
    DefOpts = [{?game_key, ?getv(?game_key)},
               {?phase, #phase{num = 1, ptype = ?day}},
               {?dev, ?standard_io},
               {?sort, ?sort_normal}],
    PP = po(#pp{}, DefOpts),
    PP2 = po(PP, Opts),
    print_statsI(PP2).

%% API
print_statsI(PP)
  when PP#pp.game == ?undefined;
       PP#pp.day == ?undefined;
       PP#pp.players_vote == ?undefined
       -> print_statsI(setup_pp(PP));
print_statsI(PP) when PP#pp.match_expr == ?undefined ->
    print_stats_match(PP);
print_statsI(PP) ->
    do_print_stats(PP).

print_stats_match(PP) when PP#pp.phase == ?total_stats ->
    %% TOTAL stats
    Pattern = mnesia:table_info(stat, wild_pattern),
    MatchHead = Pattern#stat{key = {'$1', '$2'}},
    Guard = [{'==', '$2', PP#pp.game_key}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP) when (PP#pp.phase)#phase.ptype == ?game_ended ->
    %% END stats
    Pattern = mnesia:table_info(stat, wild_pattern),
    MatchHead = Pattern#stat{key = {'$1', '$2', '$3'}},
    Guard = [{'==', '$2', PP#pp.game_key},
             {'==', '$3', ?game_ended}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP) ->
    %% PHASE stats
    #phase{num = Day, ptype = Ptype} = PP#pp.phase,
    Pattern = mnesia:table_info(stat, wild_pattern),
    MatchHead = Pattern#stat{key = {'$1', '$2', {'$3', '$4'}}},
    Guard = [{'==', '$2', PP#pp.game_key},
             {'==', '$3', Day}, {'==', '$4', Ptype}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr}).

do_print_stats(PP) ->
    MatchExpr = PP#pp.match_expr,
    Stats = mnesia:dirty_select(stat, MatchExpr),
    PrStats = [mk_prstat(S, PP) || S <- Stats],
    do_print_stats(PP, PrStats).

-spec mk_prstat(#stat{}, #pp{}) -> #prstat{}.
mk_prstat(S = #stat{}, PP) ->
    #mafia_game{site = Site} = PP#pp.game,
    LastMsgId = lists:max(S#stat.msg_ids),
    case ?rmess({LastMsgId, Site}) of
        [M] ->
            #prstat{key = S#stat.key,
                    seq_no = 1,
                    msg_ids = S#stat.msg_ids,
                    num_chars = S#stat.num_chars,
                    num_words = S#stat.num_words,
                    num_postings = S#stat.num_postings,
                    words_per_post = S#stat.num_words / S#stat.num_postings,
                    last_msg = {M#message.time, LastMsgId}
                   };
        [] ->
            %% For some unknown reason the message referred to in the
            %% stat record is missing in DB. We should reread the messages
            %% into DB. Delete game and recreate it worked last time.
            throw({message_missing_in_db, {LastMsgId, Site}})
    end.

do_print_stats(PP, PrStats) ->
    SortFun =
        case PP#pp.sort of
            ?sort_normal ->
                fun(#prstat{num_postings = PA, num_words = WA},
                    #prstat{num_postings = PB, num_words = WB}) ->
                        if PA < PB -> true;
                           PA > PB -> false;
                           WA =< WB -> true;
                           true -> false
                        end
                end;
            ?sort_words ->
                fun(#prstat{num_words = WA},
                    #prstat{num_words = WB}) ->
                        WA =< WB
                end;
            ?sort_words_per_post ->
                fun(#prstat{words_per_post = WA},
                    #prstat{words_per_post = WB}) ->
                        WA =< WB
                end;
            ?sort_last_msg_time ->
                fun(#prstat{last_msg = MA},
                    #prstat{last_msg = MB}) ->
                        MA =< MB
                end
        end,
    StatsSorted = lists:sort(SortFun, PrStats),
    UserF = fun(S) -> element(1, S#prstat.key) end,
    Phase = PP#pp.phase,
    G = PP#pp.game,
    Site = G#mafia_game.site,
    CurrPhase = mafia_time:calculate_phase(G),
    GNum = G#mafia_game.game_num,
    PhaseRef =
        fun() ->
                case Phase of
                    ?total_stats -> "";
                    _ -> ["&part=", pr_phase_long(Phase)]
                end
        end,
    PhaseName =
        fun() ->
                case Phase of
                    ?total_stats -> "Global";
                    _ -> pr_phase_long(Phase)
                end
        end,
    TimeNow = mafia_time:utc_secs1970(),
    IsGameEnded = ?undefined /= G#mafia_game.game_end,
    EndTime = case G#mafia_game.game_end of
                  ?undefined -> ?undefined;
                  {ET, _} -> ET
              end,
    {RelativeWhatTime, RelativeStr} =
        if Phase == ?total_stats,
           IsGameEnded ->
                {relative_end_time, "End"};
           Phase /= ?total_stats,
           Phase /= CurrPhase ->
                {relative_dead_line, "DL"};
           true ->
                {relative_now, "Now"}
        end,
    TimeF =
        fun(MsgTime) ->
                case RelativeWhatTime of
                    relative_end_time ->
                        Suffix = if MsgTime < EndTime -> "*";
                                    true -> ""
                                 end,
                        print_time_5d_str(
                          ?stats,
                          mafia_time:hh_mm_to_time(MsgTime, EndTime))
                            ++ Suffix;
                    relative_dead_line ->
                        print_time_5d_stat(G, MsgTime);
                    relative_now ->
                        print_time_5d_str(
                          ?stats,
                          mafia_time:hh_mm_to_time(MsgTime, TimeNow))
                end
        end,
    PrFn = fun(tr, S) -> UserF(S);
              (link, S) -> ["<a href=\"/e/web/msgs?g=",
                            ?i2l(GNum),
                            PhaseRef(), "&user=",
                            UserF(S), "\">", UserF(S), "</a>"
                           ];
              (cell, _) -> "td";
              (bgcolor, S) -> bgcolor(element(1, S#prstat.key));
              (lastmsg, S) ->
                   {MsgTime, MsgId} = S#prstat.last_msg,
                   ["<a href=\"/e/web/msg"
                    "?g=", ?i2l(GNum),
                    "&id=", web:msg_key2str({MsgId, Site}),
                    "&var=last_msg",
                    "&user=", UserF(S),
                    "&phase=", PhaseName(),
                    "\">", TimeF(MsgTime), "</a>"
                   ];
              (_, _) -> []
           end,
    PlayersRem = PP#pp.players_rem,
    NonPosters = [PRem || PRem <- PlayersRem]
        -- [PrFn(tr, S) || S <- PrStats],
    HtmlHead =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\n"
                          "Posting statistics (~s)\n"
                          "------------------\n"
                          "~s ~s ~s ~s ~s\n",
                          [pr_phase_long(Phase),
                           "Posts", " Words", "  Chars", "Word/P", "Player"]),
                [];
           PP#pp.mode == ?html ->
                ArgBeg = sort_link_beg(PP),
                PostLn = ArgBeg ++ "normal",
                WordLn = ArgBeg ++ "words",
                WPostLn = ArgBeg ++ "words_per_post",
                LMsgLn = ArgBeg ++ "last_msg_time",

                Last = "Last (" ++ RelativeStr ++ ")",
                {PostTitle, WordTitle, WPostTitle, LMsgTitle} =
                    case PP#pp.sort of
                        ?sort_normal ->
                            {"Posts",
                             ["<a href=\"", WordLn, "\">Words</a>"],
                             ["<a href=\"", WPostLn, "\">W/Post</a>"],
                             ["<a href=\"", LMsgLn, "\">", Last, "</a>"]
                            };
                        ?sort_words ->
                            {["<a href=\"", PostLn, "\">Posts</a>"],
                             "Words",
                             ["<a href=\"", WPostLn, "\">W/Post</a>"],
                             ["<a href=\"", LMsgLn, "\">", Last, "</a>"]
                            };
                        ?sort_words_per_post ->
                            {["<a href=\"", PostLn, "\">Posts</a>"],
                             ["<a href=\"", WordLn, "\">Words</a>"],
                             "W/Post",
                             ["<a href=\"", LMsgLn, "\">", Last, "</a>"]
                            };
                        ?sort_last_msg_time ->
                            {["<a href=\"", PostLn, "\">Posts</a>"],
                             ["<a href=\"", WordLn, "\">Words</a>"],
                             ["<a href=\"", WPostLn, "\">W/Post</a>"],
                             Last
                            }
                    end,
                ["<tr><th colspan=7>",
                 "Posting statistics (",
                 pr_phase_long(Phase),
                 ")\n",
                 "</th></tr>",
                 "<tr><th></th>"
                 "<th align=\"right\">", PostTitle, "</th>"
                 "<th align=\"right\">", WordTitle, "</th>"
                 "<th align=\"right\">Chars</th>"
                 "<th align=\"right\">", WPostTitle, "</th>"
                 "<th align=\"left\">Player</th>"
                 "<th align=\"left\">", LMsgTitle, "</th>"
                 "</tr>"
                ]
        end,
    print_stat_div(PP),
    {SumStat, Html1} =
        lists:foldl(
          fun(S, {Sum, Html}) ->
                  Sum2 = mafia_data:sum_stat(S, Sum),
                  {Sum2,
                   Html ++ print_stat_row(PP, S, Sum2#prstat.seq_no, PrFn)}
          end,
          {#prstat{msg_ids = [],
                   seq_no = 0,
                   num_chars = 0,
                   num_words = 0,
                   num_postings = 0,
                   words_per_post = 0.0
                  },
           HtmlHead},
          ?lrev(StatsSorted)),
    print_stat_div(PP),
    Html2 = Html1 ++
        print_stat_row(PP,
                       SumStat,
                       sum,
                       fun(cell, _) -> "th";
                          (tr, _) -> "Total Counts";
                          (link, _) -> "Total Counts";
                          (_, _) -> []
                       end),
    HtmlStats = ["<br><table align=center ", ?BG_TURQUOISE, ">",
                 Html2, "</table>"],
    NonPostTitle =
        if NonPosters == [] -> "Non-posters: -";
           true -> "Non-posters"
        end,
    NPRows = mafia_lib:split_into_groups(?NumColsInGrp, NonPosters),
    if PP#pp.mode == ?text ->
            NPStr = object_rows_text(NPRows, fun(U) -> ?b2l(U) end),
            if NonPosters == [] ->
                    io:format(PP#pp.dev,
                              "\n~s\n",
                              [NonPostTitle]);
               true ->
                    io:format(PP#pp.dev,
                              "\n~s\n~s\n~s",
                              [NonPostTitle, ul($-, NonPostTitle),
                               NPStr])
            end,
            [[], []];
       PP#pp.mode == ?html ->
            HtmlNonPosters =
                ["<br><table align=center><tr>",
                 "<th colspan=", ?i2l(?NumColsInGrp), ">", NonPostTitle,
                 "</th></tr>",
                 object_rows(NPRows),
                 "</table>"],
            [HtmlStats, HtmlNonPosters]
    end.

sort_link(PP) when is_record(PP, pp) ->
    sort_link(PP, "normal").

sort_link(PP, Type) when is_record(PP,  pp) ->
    sort_link_beg(PP) ++ Type.

sort_link_beg(PP) ->
    G = PP#pp.game,
    GNum = G#mafia_game.game_num,
    "stats" ++
        "?g=" ++ ?i2l(GNum) ++
        "&" ++ phase_args(PP#pp.phase) ++
        "&sort=".

%% INTERNAL

phase_args(?total_stats) -> "phase=global";
phase_args(#phase{ptype = ?game_ended}) -> "phase=end";
phase_args(#phase{ptype = Ptype, num = DNum}) ->
    Ph = "phase=" ++ if Ptype == ?game_start -> "Game Start";
                        Ptype == ?day -> "day";
                        Ptype == ?night -> "night"
                     end,
    Num = "&num=" ++ ?i2l(DNum),
    Ph ++ Num.

print_time_5d_stat(G, Time) ->
    print_time_5d_str(
      ?stats,
      mafia_time:hh_mm_to_deadline(G, Time)).

print_stat_div(PP) when PP#pp.mode == ?text ->
    Line = "-----------",
    io:format(PP#pp.dev,
              "~5s ~6s ~7s ~6s ~s\n",
              [Line, Line, Line, Line, Line]
             );
print_stat_div(_PP) ->
    [].

print_stat_row(PP, S, _, PrFn) when PP#pp.mode == ?text ->
    io:format(PP#pp.dev,
              "~5s ~6s ~7s ~6.2f ~s\n",
              [i2l(S#prstat.num_postings, 5),
               i2l(S#prstat.num_words, 6),
               i2l(S#prstat.num_chars, 7),
               S#prstat.words_per_post,
               PrFn(tr, S)
              ]),
    [];
print_stat_row(PP, S, SeqNo, PrFn) when PP#pp.mode == ?html ->
    SeqNoStr =
        if SeqNo == sum -> "<td></td>";
           true -> ["<td align=\"center\">"
                    "<font size=-2><i>", ?i2l(SeqNo), "</i></font></td>"]
        end,
    %% PrFn(cell, S) :: "th" | "td"
    CBegR = ["<", PrFn(cell, S), " align=\"right\">"],
    CBegL = ["<", PrFn(cell, S), " align=\"left\">"],
    CEnd = ["</", PrFn(cell, S), ">"],
    ["<tr", PrFn(bgcolor, S), ">",
     SeqNoStr,
     CBegR, ?i2l(S#prstat.num_postings),
     CEnd, CBegR, ?i2l(S#prstat.num_words),
     CEnd, CBegR, ?i2l(S#prstat.num_chars),
     CEnd, CBegR, io_lib:format("~.2f", [S#prstat.words_per_post]),
     CEnd, CBegL, PrFn(link, S),
     CEnd, CBegL, PrFn(lastmsg, S),
     CEnd, "</tr>\r\n"].

i2l(Int, Size) -> string:right(?i2l(Int), Size).
