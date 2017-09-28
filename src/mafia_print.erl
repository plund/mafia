-module(mafia_print).

%% Manual API
-export([pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2
        ]).

%% API
-export([
         print_phase/1,
         print_messages/1,
         print_message_summary/1,
         print_message_full/1,
         print_time/1,
         print_time/2,

         print_pages_for_thread/0,
         print_pages_for_thread/1,

         html2txt/1,
         nbsp/1
        ]).

%% mafia_stats + mafia_print
-export([po/2,
         setup_pp/1,
         pr_phase_long/1,
         print_time_5d/2,
         print_time_5d_str/2,
         user_vote_timesort/1,
         split_into_groups/2,
         object_rows/1,
         object_rows_text/2,
         ptype_arg/1,
         ul/2
        ]).


-import(mafia_lib, [bgcolor/1]).

-include("mafia.hrl").
-include("mafia_print.hrl").

%% -----------------------------------------------------------------------------

po(P, [{?game_key, K} | T]) -> po(P#pp{game_key = K}, T);
po(P, [{?phase, Ph = #phase{}} | T]) -> po(P#pp{phase = Ph}, T);
po(P, [{?phase, Ph = ?total_stats} | T]) -> po(P#pp{phase = Ph}, T);
po(P, [{?dev, Fd} | T]) -> po(P#pp{dev = Fd}, T);
po(P, [{?mode, M} | T]) -> po(P#pp{mode = M}, T);
po(P, [{?t_mode, M} | T]) -> po(P#pp{t_mode = M}, T);
po(P, [{?period, M} | T]) -> po(P#pp{period = M}, T);
po(P, [{?use_time, V} | T]) -> po(P#pp{use_time = V}, T);
po(P, [{?time_zone, V} | T]) -> po(P#pp{time_zone = V}, T);
po(P, [{?dst, D} | T]) -> po(P#pp{dst = D}, T);
po(P, [{?sort, S} | T]) -> po(P#pp{sort = S}, T);
po(P, []) -> P.

%% -----------------------------------------------------------------------------

pp() ->
    ThId = ?getv(?thread_id),
    Page = case ?rgame(ThId) of
               [] -> ?getv(?page_to_read);
               [G] -> G#mafia_game.page_to_read
           end,
    pp(ThId, Page).

pp({ThId, Page}) ->
    pp(ThId, Page);
pp(Page) ->
    ThId = ?getv(?thread_id),
    pp(ThId, Page).

pp(GNum, Page) ->
    ppI(GNum, Page).

ppI(E = {?error, _}, _Page) -> E;
ppI(ThId, Page) ->
    %% Select MsgIds here
    MsgIds = msgids(ThId, Page),
    print_page(ThId, MsgIds, fun print_message_full/1).

%% -----------------------------------------------------------------------------

pps() ->
    ThId = ?getv(?thread_id),
    Page = case ?rgame(ThId) of
               [] -> ?getv(?page_to_read);
               [G] -> G#mafia_game.page_to_read
           end,
    pps(ThId, Page).

pps({ThId, Page}) ->
    pps(ThId, Page);
pps(Page) when is_integer(Page) ->
    ThId = ?getv(?thread_id),
    pps(ThId, Page).

pps(GNum, Page) ->
    ppsI(GNum, Page).

ppsI(E = {?error, _}, _Page) -> E;
ppsI(ThId, Page) ->
    %% Select MsgIds here
    MsgIds = msgids(ThId, Page),

    io:format("~-10s "
              "~-3s"
              " ~-11s "
              "~-7s "
              "~s\n",
              ["Player", "pg", "Date/Time", "Msg Id", "Message Text"]),
    print_page(ThId, MsgIds, fun print_message_summary/1).

%% -----------------------------------------------------------------------------

pm(MsgId) when is_integer(MsgId) ->
    pm(?standard_io, MsgId);
pm(PP = #pp{}) when PP#pp.message == ?undefined ->
    pm_rmess(PP);
pm(PP = #pp{}) ->
    print_message_full(PP).

pm(Fd, MsgId) when is_integer(MsgId) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    pm(#pp{dev = Fd, msg_id = MsgId, time_zone = TzH, dst = Dst}).

pm_rmess(PP) ->
    M = hd(?rmess(PP#pp.msg_id)),
    pm(PP#pp{message = M}).

%% -----------------------------------------------------------------------------

ptype_arg(Ptype) ->
    if Ptype == d; Ptype == day; Ptype == ?day -> ?day;
       Ptype == n; Ptype == night; Ptype == ?night -> ?night
       %% Ptype == e; Ptype == 'end'; Ptype == ?game_end -> ?game_ended
    end.

setup_pp(PP) when PP#pp.game == ?undefined ->
    setup_pp(PP#pp{game = hd(?rgame(PP#pp.game_key))});
setup_pp(PP) when PP#pp.day == ?undefined ->
    setup_pp(PP#pp{day = ?rday(PP#pp.game_key, PP#pp.phase)});

%% D2
%% rema : votecount/nonvote/non-post  Flum + Vecna (not GA)
%%        New + DeadAtEnd (not Dead+Old)
%% vote : votetracker Flum + Vecna + GA
%%        New + Dead + Old ()
setup_pp(PP) when PP#pp.players_rem == ?undefined ->
    #mafia_day{players_rem = PlayersRem,
               player_deaths = Deaths} = PP#pp.day,
    Phase = PP#pp.phase,
    PlayersRem2 =
        lists:foldl(
          %% Include IsEnd same phase
          fun(#death{player = DeadB,
                     phase = DPhase,
                     is_end = IsEnd,
                     is_deleted = false},
              Acc) ->
                  if Phase == DPhase andalso IsEnd
                     orelse
                     Phase < DPhase  ->
                          Acc ++ [DeadB];
                     true -> Acc
                  end;
             (#replacement{new_player = NewB,
                           replaced_player = RepB,
                           phase = RPhase},
              Acc) when Phase < RPhase->
                  (Acc ++ [RepB]) -- [NewB];
             (_, Acc) -> Acc
          end,
          PlayersRem,
          Deaths),
    setup_pp(PP#pp{players_rem = PlayersRem2});
setup_pp(PP) when PP#pp.players_vote == ?undefined ->
    #mafia_day{players_rem = PlayersRem,
               player_deaths = Deaths} = PP#pp.day,
    Phase = PP#pp.phase,
    AllPlayersB =
        lists:foldl(
          fun(#replacement{new_player = NewB,
                           replaced_player = RepB,
                           phase = RPhase
                           %% time = RTime
                          },
              Acc) ->
                  if Phase > RPhase ->
                          Acc;
                     Phase == RPhase ->
                          (Acc ++ [RepB]);
                     Phase < RPhase ->
                          (Acc ++ [RepB]) -- [NewB]
                  end;
             (#death{player = DeadB,
                     %% time = DTime,
                     phase = DPhase,
                     is_deleted = false}, Acc)
                when DPhase >= Phase ->
                  Acc ++ [DeadB];
             (_, Acc) -> Acc
          end,
          PlayersRem,
          Deaths),
    setup_pp(PP#pp{players_vote = AllPlayersB});
setup_pp(PP) when PP#pp.phase_type == ?undefined ->
    PhaseType = case PP#pp.phase of
                    #phase{ptype = ?game_start} -> ?game_start;
                    #phase{ptype = ?game_ended} -> ?game_ended;
                    #phase{ptype = Ptype} -> Ptype;
                    ?total_stats -> ?total_stats
                end,
    setup_pp(PP#pp{phase_type = PhaseType});
setup_pp(PP) -> PP.

%% -----------------------------------------------------------------------------

%% /0 human
print_votes() ->
    GameKey = ?getv(?game_key),
    Phase = mafia_time:calculate_phase(GameKey),
    print_votes([{?phase, Phase}]).

%% /2 human
print_votes(DayNum, Ptype) ->
    Ptype2 = ptype_arg(Ptype),
    print_votes([{?phase, #phase{num = DayNum, ptype = Ptype2}}]).

%% /1 generic
print_votes(Opts) ->
    DefOpts = [{?game_key, ?getv(?game_key)},
               {?mode, ?text},
               {?dev, ?standard_io}],
    PP = po(#pp{}, DefOpts),
    PP2 = po(PP, Opts),
    print_votesI(PP2).

print_votesI(PPin) ->
    PP = setup_pp(PPin),
    #pp{game = G, day = Day} = PP,
    RealRemPlayers = PP#pp.players_rem,
    PhaseType = PP#pp.phase_type,

    %% Part - Page heading - Print Game Name
    DoDispTime2DL = PhaseType /= ?game_ended andalso is_integer(PP#pp.use_time),
    ModMsgV = ?getv(?mod_msg),
    DoDispModMsg = is_integer(PP#pp.use_time)
        andalso is_list(ModMsgV) andalso ModMsgV /= "",
    GName = if G#mafia_game.name == ?undefined -> "(No Name)";
               true -> ?b2l(G#mafia_game.name)
            end,
    HTitle =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\n"
                          "~s\n"
                          "~s\n"
                          "\n"
                          "Game Masters: ~s\n"
                          "\n"
                          "Previous days found at ~s\n",
                          [GName, ul($=, GName),
                          string:join([?b2l(GM) || GM <- G#mafia_game.gms],
                                      " and "),
                          ?BotUrl]);
           PP#pp.mode == ?html ->
                LinkPhase = if is_integer(PP#pp.use_time) -> ?current;
                               true -> PP#pp.phase
                            end,
                ModMsg =
                    if DoDispModMsg ->
                            ["<tr><td align=center width=600>"
                             "<i>", ModMsgV, "</i>"
                             "</td></tr>\r\n"];
                       true -> ""
                    end,
                {Href, Link} =
                    mafia_file:game_link_and_text(PP#pp.game, LinkPhase),
                ["<tr><th>", GName, "</th></tr>\r\n",
                 ModMsg,
                 "<tr><td><table align=center cellpadding=4 cellspacing=6 >"
                 "<tr><td>Game Masters:<td>",
                 [["<td", bgcolor(GM), ">",
                   ?b2l(GM), "</td>"] || GM <- G#mafia_game.gms],
                 "</tr></table></td></tr>"
                 "<tr><td align=center>",
                 "Previous days found at "
                 "<a href=\"/\">", ?BotUrl, "</a>",
                 "</td></tr>\r\n",
                 "<tr><td align=center>",
                 "Text version of this page is found at "
                 "<a href=\"", Href, "\">",
                 Link, "</a>",
                 "</td></tr>"]
        end,

    %% Part - Display time Left to Deadline or display End of Game message
    HDeadLine =
        if DoDispTime2DL ->
                print_time_left_to_dl(PP);
           PhaseType == ?game_ended ->
                {EndTime, EndMsgId} = G#mafia_game.game_end,
                {TzH, Dst} = mafia_time:get_tz_dst(G, EndTime),
                EndPhase =
                    print_phase(
                      mafia_time:find_phase_with_time(G, EndTime)),
                EndTimeStr = print_time(EndTime, TzH, Dst, ?extensive),
                case PP#pp.mode of
                    ?text ->
                        io:format(
                          PP#pp.dev,
                          "\n"
                          "The GAME HAS ENDED after phase ~s "
                          "and the time was ~s\n",
                          [EndPhase, EndTimeStr]),
                        io:format(PP#pp.dev,
                                  "\n"
                                  "Game Master End Message\n"
                                  "-----------------------\n",
                                  []),
                        pm(PP#pp{msg_id = EndMsgId,
                                 time_zone = TzH,
                                 dst = Dst});
                    ?html ->
                        GmMessage = web_impl:show_msg(G, EndMsgId),
                        ["<tr><td align=center>",
                         "The GAME HAS ENDED after phase ",
                         EndPhase,
                         " and the time was ", EndTimeStr,
                         "</td></tr>"
                         "<tr><td align=center>"
                         "<table cellpadding=6 cellspacing=3>",
                         GmMessage, "</table></td></tr>"]
                end;
           true -> []
        end,

    %% Part - Thread Links
    HThreadLinks = pr_thread_links(PP, DoDispTime2DL),

    %% votes, from remaining players only
    HVoteCount =
        if PhaseType == ?day ->
                %% Part - Votes
                pr_votes(PP);
           true ->
                []
        end,

    %% Part - End votes
    EndVotes =
        if PhaseType == ?day ->
                EndVoters = Day#mafia_day.end_votes,
                EndVoteTitle =
                    if EndVoters == [] -> "End votes: -";
                       true -> "End votes: "
                    end,
                if PP#pp.mode == ?text ->
                        io:format(
                          PP#pp.dev,
                          "\n~s~s\n",
                          [EndVoteTitle,
                           string:join([?b2l(Ev) || Ev <- EndVoters], ", ")]);
                   PP#pp.mode == ?html ->
                        ["<tr><td><table align=center>",
                         "<tr><th>", EndVoteTitle, "</th>",
                         [["<td", bgcolor(Ev), ">", ?b2l(Ev), "</td>"]
                          || Ev <- EndVoters],
                         "</tr></table></td></tr>"]
                end;
           true -> []
        end,

    {VoteSumSort, InvalidVotes} = vote_summary(Day, PP#pp.players_rem),
    Votes = rem_play_votes(PP#pp.day, PP#pp.players_rem),
    NonVotes =
        if PhaseType == ?day ->
                %% Part - Non-votes
                ValidVoters = [ Pl || {Pl, _} <- Votes]
                    -- [User || {User, _} <- InvalidVotes],
                NonVoted = RealRemPlayers -- ValidVoters,
                NoVoteTitle = if NonVoted == [] -> "Non-votes: -";
                                 true -> "Non-votes"
                              end,
                NVRows = split_into_groups(?NumColsInGrp, NonVoted),
                if PP#pp.mode == ?text ->
                        if NonVoted == [] ->
                                io:format(PP#pp.dev,
                                          "\n~s\n",
                                          [NoVoteTitle]);
                           true ->
                                NVStr =
                                    object_rows_text(
                                      NVRows,
                                      fun(U) -> ?b2l(U) end),
                                io:format(PP#pp.dev,
                                          "\n~s\n~s\n~s",
                                          [NoVoteTitle,
                                           ul($-, NoVoteTitle),
                                           NVStr])
                        end;
                   PP#pp.mode == ?html ->
                        ["<tr><td><table align=center><tr><th colspan=",
                         ?i2l(?NumColsInGrp), ">",
                         NoVoteTitle, "</th></tr>",
                         object_rows(NVRows),
                         "</table></td></tr>"]
                end;
           true -> []
        end,

    %% Part - List remaining players
    DeathsUptoNow =
        [D#death.player
         || D = #death{phase = Ph, is_deleted = false}
                <- G#mafia_game.player_deaths,
            if (PP#pp.phase)#phase.ptype == ?game_ended -> true;
               true -> Ph =< PP#pp.phase
            end],
    %% split up remaining players into groups of ?NumColsInGrp (10)
    RPRows = split_into_groups(?NumColsInGrp, RealRemPlayers -- DeathsUptoNow),
    HRemPlayers =
        if PP#pp.mode == ?text ->
                RPStr = object_rows_text(RPRows, fun(U) -> ?b2l(U) end),
                io:format(PP#pp.dev,
                          "\nRemaining Players"
                          "\n-----------------\n~s\n",
                          [RPStr]),
                ok;
           PP#pp.mode == ?html ->
                ["<tr><td>"
                 "<table align=center cellpadding=2 cellspacing=2><tr>"
                 "<tr><th colspan=", ?i2l(?NumColsInGrp),
                 ">Remaining players"
                 "</th></tr>",
                 object_rows(RPRows),
                 "</table>"
                 "</td></tr>"]
        end,

    %% Part - Voting texts
    if PhaseType == ?day, PP#pp.mode == ?text ->
            ValidVotesS =
                lists:sort(
                  lists:foldl(fun({_, _, VoteInfos}, Acc) ->
                                      Acc ++ VoteInfos
                              end,
                              [],
                              VoteSumSort)),
            io:format(PP#pp.dev,
                      "\nVoting texts:\n"
                      "-------------\n",
                      []),
            [io:format(PP#pp.dev,
                       "~s ~s : \"~s\"\n",
                       [print_time_5d(G, VTime),
                        ?b2l(Voter),
                        rm_nl(?b2l(Raw))])
             || {VTime, Voter, Raw} <- ValidVotesS],

            %% Part - Invalid Vote text
            UserVotesTS = user_vote_timesort(Votes),
            InvUserVotesTS =
                [UV || UV = {_, #vote{valid = false}} <- UserVotesTS],
            io:format(PP#pp.dev,
                      "\n"
                      "Invalid Vote texts:\n"
                      "-------------------\n",
                      []),
            [io:format(PP#pp.dev,
                       "~s ~s: \"~s\"\n",
                       [print_time_5d(G, VTime),
                        Voter,
                        rm_nl(?b2l(Raw))])
             || {Voter, #vote{raw = Raw, time = VTime}} <- InvUserVotesTS],
            io:format(PP#pp.dev,
                      "\n"
                      "\"INVALID\" means that the this program did not "
                      "recognise the vote. But the GM may.)\n",
                      []);
       true -> ignore
    end,

    %% Part - Vote tracker
    [HTrackKey, HVoteTrack] =
        if PhaseType == ?day ->
                mafia_tracker:print_tracker(PP);
           true -> [[], []]
        end,

    %% Part - Posting stats
    PPstat = if (PP#pp.phase)#phase.ptype == ?game_ended ->
                     PP#pp{phase = ?total_stats};
                true -> PP
             end,
    [HStats, HNonPosters] = mafia_stats:print_statsI(PPstat),

    %% Part - Dead players
    DoReport =
        fun(DPhase) ->
                if (PP#pp.phase)#phase.ptype == ?game_ended -> true;
                   PP#pp.use_time == ?undefined -> DPhase == PP#pp.phase;
                   true -> DPhase =< PP#pp.phase
                end
        end,
    DeathsToReport =
        lists:foldr(
          fun(D = #death{phase = Ph,
                         is_deleted = false}, Acc) ->
                  case DoReport(Ph) of
                      true -> [D | Acc];
                      false -> Acc
                  end;
             (R = #replacement{phase = Ph}, Acc) ->
                  case DoReport(Ph) of
                      true -> [R | Acc];
                      false -> Acc
                  end
          end,
          [],
          G#mafia_game.player_deaths),
    IsZeroDeaths = [] == [D || D = #death{} <- DeathsToReport],
    IsZeroReplacements = [] == [R || R = #replacement{} <- DeathsToReport],
    PrFun =
        fun(IsEnd, Ph) ->
                " died " ++ pr_eo_ptype(IsEnd, Ph)
        end,
    PrRepFun =
        fun(Ph) ->
                " was replaced " ++ pr_eo_ptype(false, Ph) ++ " by "
        end,
    if PP#pp.mode == ?text ->
            if not IsZeroDeaths ->
                    {Fmt, Div} =
                        {"\n"
                         "Dead Players\n"
                         "------------\n"
                         "~s\n",
                         "\n"},
                    io:format(PP#pp.dev,
                              Fmt,
                              [string:join(
                                 [?b2l(DeadPl) ++ PrFun(IsEnd, Ph) ++
                                      if Com == ?undefined ->
                                              " - msg: " ++ ?i2l(MsgId);
                                         is_binary(Com) ->
                                              " - " ++ ?b2l(Com)
                                      end
                                  || #death{player = DeadPl,
                                            is_end = IsEnd,
                                            phase = Ph,
                                            msg_id = MsgId,
                                            comment = Com}
                                         <- DeathsToReport],
                                 Div)]);
               true -> ok
            end,
            if not IsZeroReplacements ->
                    Reps = [io_lib:format("~s~s~s\n",
                                          [?b2l(RepP), PrRepFun(Ph), ?b2l(NewP)])
                            || #replacement{new_player = NewP,
                                            replaced_player = RepP,
                                            phase = Ph}
                                   <- DeathsToReport],
                    io:format(PP#pp.dev,
                              "\nReplacements"
                              "\n------------\n~s\n",
                              [Reps]);
               true -> ok
            end,
            HtmlDeaths = ?dummy;
       PP#pp.mode == ?html ->
            HtmlDeaths =
                ["<table align=center>",
                 "<tr><th colspan=2 align=left><br>Dead or replaced players"
                 "</th></tr>\r\n",
                 if IsZeroDeaths, IsZeroReplacements ->
                         "<tr><th>(none)</th></tr>\r\n";
                    true ->
                         [case DoR of
                              #death{player = DeadPl,
                                     is_end = IsEnd,
                                     phase = Ph,
                                     msg_id = MsgId,
                                     comment = Com} ->
                                  ["<tr><td><table align=left><tr><td",
                                   bgcolor(DeadPl), ">", ?b2l(DeadPl), "</td>"
                                   "<td><a href=\"/e/web/msg"
                                   "?g=", ?i2l(G#mafia_game.game_num),
                                   "&id=", ?i2l(MsgId),
                                   "&player=", ?b2l(DeadPl),
                                   "&var=death\">",
                                   PrFun(IsEnd, Ph), "</a>",
                                   if Com == ?undefined ->
                                           " - msg: " ++ ?i2l(MsgId);
                                      is_binary(Com) ->
                                           " - " ++ ?b2l(Com)
                                   end,
                                   "</td></tr></table></td></tr>\r\n"];
                              #replacement{new_player = NewPl,
                                           replaced_player = RepPl,
                                           phase = Ph,
                                           msg_id = MsgId} ->
                                  ["<tr><td><table align=left><tr><td",
                                   bgcolor(RepPl), ">", ?b2l(RepPl), "</td>"
                                   "<td><a href=\"/e/web/msg"
                                   "?g=", ?i2l(G#mafia_game.game_num),
                                   "&id=", ?i2l(MsgId),
                                   "&player=", ?b2l(RepPl),
                                   "&var=replacement\">",
                                   PrRepFun(Ph), "</a></td>"
                                   "<td", bgcolor(NewPl), ">", ?b2l(NewPl),
                                   "</td></tr></table></td></tr>\r\n"]
                          end
                          || DoR <- DeathsToReport]
                 end,
                 "</table>"]
    end,

    %% Part - 4 Deadlines
    HDeadlines = print_num_dls(PP, DoDispTime2DL, 4),

    %% Part - Footer
    HFooter =
        if PP#pp.mode == ?text ->
                GameThId = (PP#pp.game)#mafia_game.thread_id,
                if is_integer(GameThId) ->
                        io:format(PP#pp.dev, "\n", []),
                        Per = PP#pp.period,
                        if is_number(Per) ->
                                io:format(
                                  PP#pp.dev,
                                  "Updates currently every ~s minutes "
                                  "(more often near deadlines).\n",
                                  [if is_integer(Per) -> ?i2l(Per);
                                      is_float(Per) ->
                                           float_to_list(
                                             Per,
                                             [{decimals, 2}])
                                   end]);
                           true -> ok
                        end,
                        io:format(
                          PP#pp.dev,
                          "Mafia game thread at: ~s\n",
                          [?UrlBeg ++ ?i2l(GameThId)]);
                   true -> ok
                end;
           PP#pp.mode == ?html ->
                GameThId = (PP#pp.game)#mafia_game.thread_id,
                [if is_integer(GameThId) ->
                         ThStr = ?i2l(GameThId),
                         GameLink = [?UrlBeg, ThStr, "#", ThStr],
                         Per = PP#pp.period,
                         ["<tr><td align=center><br>",
                          if is_number(Per) ->
                                  ["Updates currently every ",
                                   if is_integer(Per) -> ?i2l(Per);
                                      is_float(Per) ->
                                           float_to_list(
                                             Per,
                                             [{decimals, 2}])
                                   end,
                                   " minutes (more often near deadlines)."
                                   "<br>"];
                             true -> []
                          end,
                          ["Mafia game thread at: "
                           "<a href=\"", GameLink, "\">",
                           GameLink, "</a>"],
                          "</td></tr>"];
                    true -> ""
                 end,
                 if DoDispTime2DL ->
                         ["<tr><td align=center><br>",
                          "Server time offset: ",
                          mafia_web:get_ntp_offset(),
                          "</td></tr>"
                         ];
                    true -> ""
                 end
                ]
        end,
    if PP#pp.mode == ?text -> ok;
       PP#pp.mode == ?html ->
            [HTitle, "\r\n",
             HDeadLine, "\r\n",
             HThreadLinks, "\r\n",
             HVoteCount, "\r\n",
             EndVotes, "\r\n",
             NonVotes, "\r\n",
             HRemPlayers, "\r\n",
             ["<tr><td>\r\n", HTrackKey, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HVoteTrack, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HStats, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HNonPosters, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HtmlDeaths, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HDeadlines, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HFooter, "</td><tr>\r\n"]
            ]
    end.

object_rows_text(Rows, ToText) ->
    [[string:join([ToText(U) %%?b2l(U)
                   || U <- Row], ", "), "\n"]
     || Row <- Rows].

object_rows(Groups) ->
    [[ "<tr>",
       [["<td", bgcolor(U), "><center>", nbsp(U), "</center></td>"]
        || U <- Group],
       "</tr>"
     ]
     || Group <- Groups].

%% for mafia_lib...
split_into_groups(NumPerRow, Objects) ->
    {LastRow, Rows2} =
        lists:foldl(
          fun(U, {R, Rows1}) ->
                  if length(R) == NumPerRow ->
                          {[U], Rows1 ++ [R]};
                     true ->
                          {R ++[U], Rows1}
                  end
          end,
          {[], []},
          Objects),
    Rows2 ++ [LastRow].

print_time_left_to_dl(PP) ->
    {{Days, {HH, MM, _}},
     #dl{phase = Ph = #phase{}}} =
        mafia_time:get_next_deadline(PP#pp.game_key, PP#pp.use_time),
    DayStr = if Days == 0 -> "";
                Days == 1 -> ?i2l(Days) ++ " day, ";
                true -> ?i2l(Days) ++ " days, "
             end,
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev,
                      "\n"
                      "Remaining time to ~s:"
                      "  ~s~p hours, ~p minutes\n",
                      [print_phase_next(Ph),
                       DayStr, HH, MM]);
       PP#pp.mode == ?html ->
            ["<tr><th>",
             "<center>Remaining time to ",
             print_phase_next(Ph),
             ":"
             "  ", DayStr, ?i2l(HH), " hours, ", ?i2l(MM), " minutes\n",
             "</center></th></tr>"]
    end.

print_num_dls(PP, _, _) when PP#pp.phase_type == ?game_ended ->
    %% print all deadlines including EndOfGame, without relative times
    G = PP#pp.game,
    DLs = G#mafia_game.deadlines,
    {Time, _} = G#mafia_game.game_end,
    {GameDLs, _} =
        lists:partition(fun(DL) -> DL#dl.time =< Time end, DLs),
    %% Time = mafia_time:utc_secs1970(),
    DlInfo = prep_dl_info(G, Time, GameDLs),
    if PP#pp.mode == ?text ->
            print_dls_text(PP, DlInfo, "All Game Deadlines", false);
       PP#pp.mode == ?html ->
            ["<br>"
             "<table border=1 align=center ", ?BG_TURQUOISE, ">",
             print_dls_html(DlInfo, "All Game Deadlines", false),
             "</table>"]
    end;
print_num_dls(PP, DoDispTime2DL, Num) when DoDispTime2DL == true ->
    Time = mafia_time:utc_secs1970(),
    G = PP#pp.game,
    DLs = mafia_time:next_deadlines(G, Time, Num),
    {PastDLs, ComingDLs} =
        lists:partition(fun(DL) -> DL#dl.time < Time end, DLs),
    Past2 = prep_dl_info(G, Time, PastDLs),
    Coming2 = prep_dl_info(G, Time, ComingDLs),
    if PP#pp.mode == ?text ->
            print_dls_text(PP, Past2, "Deadlines in the past", true),
            print_dls_text(PP, Coming2, "Deadlines in the future", true);
       PP#pp.mode == ?html ->
            ["<br>"
             "<table border=1 align=center ", ?BG_TURQUOISE, ">",
             print_dls_html(Past2, "Deadlines in the past", true),
             print_dls_html(Coming2, "Deadlines in the future", true),
             "</table>"]
    end;
print_num_dls(_PP, DoDispTime2DL, _Num)
  when DoDispTime2DL == false -> [].

prep_dl_info(G, Time, DLs) ->
    [{if Ptype == ?game_start -> pr_ptype(Ptype);
         true -> [pr_ptype(Ptype), " ", ?i2l(N)]
      end,
      mafia_time:secs2day_hour_min_sec(Time - DLT),
      print_game_time(G, DLT, ?human)}
     || #dl{phase = #phase{num = N, ptype = Ptype}, time = DLT} <- DLs].

print_dls_text(_PP, DLs, _Title, _) when length(DLs) =< 0 -> ok;
print_dls_text(PP, DLs, Title, PrintRelative) ->
    Fmt = if PrintRelative ->
                  "~-8s ~-13s ~s\n";
             true ->
                  "~-8s ~s\n"
          end,
    Hdrs = if PrintRelative ->
                   ["Phase", "Relative now", "Absolute time",
                    ul($-, "Phase"), ul($-, "Relative now"), ul($-, 56)];
              true ->
                   ["Phase", "Absolute time",
                    ul($-, "Phase"), ul($-, 32)]
           end,
    io:format(PP#pp.dev,
              "\n"
              "~s\n"
              "~s\n"
              ++ Fmt ++ Fmt,
              [Title, ul($-, Title)] ++ Hdrs),
    [io:format(PP#pp.dev,
               Fmt,
               if PrintRelative ->
                       [PhaseStr,
                        [?i2l(Days), "D ",?i2l(HH), "H ", ?i2l(MM), "M"],
                        TimeStr];
                  true ->
                       [PhaseStr,
                        TimeStr]
               end)
     || {PhaseStr, {Days, {HH, MM, _}}, TimeStr} <- DLs].

ul(Char, N) when is_integer(N), N > 0 -> [Char|| _ <- lists:seq(1, N)];
ul(Char, Str) when is_list(Str) -> [Char || _ <- Str].

print_dls_html(DLs, _Title, _PrintRelative) when length(DLs) =< 0 -> [];
print_dls_html(DLs, Title, PrintRelative) ->
    ["<tr>"
     "<th colspan=" ++
         if PrintRelative -> "3";
            true -> "2"
         end ++
         " align=center>", Title, "</th>"
     "</tr>",
     "<tr>"
     "<th>Phase</th>" ++
         if PrintRelative -> "<th>Relative now</th>";
            true -> ""
         end ++
         "<th>Absolute time</th>"
     "</tr>",
     [["<tr><td align=center>", PhaseStr, "</td>" ++
           if PrintRelative ->
                   ["<td>", ?i2l(Days), "D ",?i2l(HH), "H ",
                    ?i2l(MM), "M</td>"];
              true -> ""
           end ++
           "<td>", TimeStr, "</td>"
       "</tr>"]
      || {PhaseStr, {Days, {HH, MM, _}}, TimeStr} <- DLs]].

pr_thread_links(PP, _DoDispTime2DL)
  when (PP#pp.phase)#phase.ptype == ?game_ended ->
    GameNumStr = integer_to_list((PP#pp.game)#mafia_game.game_num),
    Link = "<a href=\"/e/web/msgs?g=" ++ GameNumStr ++
        "&part=end\">Game End</a>",
    ["<tr><td align=center>Messages for this phase: ", Link,
     "</td></tr>"];
pr_thread_links(PP, DoDispTime2DL) ->
    %% Day4(p98-) p103-, p108-, p113-, p118-, p123-, p128-, last(p130)
    StartTime = mafia_time:get_time_for_prev_phase(PP#pp.game, PP#pp.phase),
    EndTime = mafia_time:get_time_for_phase(PP#pp.game, PP#pp.phase),
    ThId = (PP#pp.game)#mafia_game.thread_id,
    PageKeys = mafia_lib:all_page_keys(ThId),
    GameNumStr = integer_to_list((PP#pp.game)#mafia_game.game_num),
    case lists:foldl(
           fun(_PK, Acc = {done, _}) -> Acc;
              (PK = {_, P}, Acc) ->
                   #page_rec{message_ids= [MsgId|_]} =
                       hd(?rpage(PK)),
                   #message{time = MTime} = hd(?rmess(MsgId)),
                   case Acc of
                       {startpage, _} when MTime >= StartTime ->
                           {endpage, {P-1, P-1}};
                       {startpage, _} -> {startpage, {P, P}};
                       {endpage, {St, _}} when MTime >= EndTime ->
                           {done, {St, P-1}};
                       {endpage, {St, _}} -> {endpage, {St, P}}
                   end
           end,
           {startpage, none},
           PageKeys) of
        {_, {StartPage0, EndPage}} when is_integer(StartPage0),
                                        is_integer(EndPage) ->
            StartPage = max(StartPage0, 1),
            UrlPart = "/e/web/msgs?g=" ++ GameNumStr ++ "&part=p",
            PageNs = fun(PN) ->
                             EndPN = min(PN + 4, EndPage),
                             [?i2l(PN),"-", ?i2l(EndPN)]
                     end,
            Pages = fun(Sta, End) -> [?i2l(Sta),"-", ?i2l(End)] end,
            Links0 =
                [["<a href=\"", UrlPart, PageNs(PN),"\">p", PageNs(PN), "</a>"]
                 || PN <- lists:seq(StartPage, EndPage, 5) -- [EndPage]],
            LastPs = [?i2l(EndPage), "-", ?i2l(EndPage + 9)],
            LastLink =
                if DoDispTime2DL ->
                        ["<a href=\"", UrlPart, LastPs, "\">Last&More</a>"];
                   not DoDispTime2DL ->
                        [" or <a href=\"", UrlPart, Pages(StartPage, EndPage),
                         "\">complete ", print_phase(PP#pp.phase), "</a>"]
                end,
            Links = my_string_join(Links0 ++ [LastLink], " "),
            ["<tr><td align=center>Messages for this phase: ", Links,
             "</td></tr>"];
        _Other ->
            []
    end.

my_string_join([A, B | T], Sep) ->
    [A, Sep | my_string_join([B | T], Sep)];
my_string_join(Any, _Sep) -> Any.

%% Votes per user are time ordered (oldest first)
%% Users sorted time ordered after they oldest vote (first vote)
-spec pr_votes(PP :: #pp{}) -> term().
pr_votes(PP) ->
    Day = PP#pp.day,
    {VoteSumSort, _InvalidVotes} = vote_summary(Day, PP#pp.players_rem),
    Html =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\nVotes ~s\n"
                          "------------\n",
                          [pr_phase_long(PP#pp.phase)]),
                [begin
                     io:format(PP#pp.dev, "~s (~p): ", [?b2l(Vote), N]),
                     VoterNames = [?b2l(Voter)
                                   || {_Time, Voter, _Raw3} <- VoteInfos],
                     io:format(PP#pp.dev, "~s\n",
                               [string:join(VoterNames, ", ")])
                 end
                 || {Vote, N, VoteInfos} <- VoteSumSort],
                [];
           PP#pp.mode == ?html ->
                ["<tr><th>","<br>",
                 "Vote Count ", pr_phase_long(PP#pp.phase), "</th></tr>",
                 "<tr><td><table border=0 align=center ", ?BG_TURQUOISE, ">",
                 "<tr><th>Wagon</th><th>#</th><th>Voters</th></tr>",
                 [["<tr><td", bgcolor(Vote), " align=center>", nbsp(?b2l(Vote)),
                   "</td><td align=center>(", ?i2l(N), ")</td>",
                   "<td><table><tr>",
                   [["<td", bgcolor(Voter), ">", nbsp(?b2l(Voter)), "</td>"]
                    || {_Time, Voter, _Raw3} <- VoteInfos],
                   "</tr></table></td>",
                   "</tr>\r\n"] || {Vote, N, VoteInfos} <- VoteSumSort],
                 "</table></td></tr>"
                ]
        end,
    Html.

vote_summary(Day, Players) ->
    Votes = rem_play_votes(Day, Players),
    {VoteSummary, InvalidVotes} =
        lists:foldl(
          %% UserVotes are time ordered
          fun({User, UserVotes}, {Acc, Acc2}) ->
                  %% Look for vote when user starts to vote for end vote
                  case user_vote(UserVotes) of
                      #vote{valid = true} = V ->
                          {add_vote(V, User, Acc), Acc2};
                      #vote{valid = false} = V ->
                          {Acc, [{User, V} | Acc2]};
                      no_vote -> % no votes at all
                          {Acc, Acc2}
                  end
          end,
          {[], []},
          Votes),
    %% Sort votes in each wagons on time so we have oldest first
    VoteSum2 = [setelement(3, Wgn, lists:sort(VoteInfos))
                || Wgn = {_, _, VoteInfos} <- VoteSummary],

    %% Sort Wagons first on number of received votes, if equal
    %% second sort should be on having the oldest vote
    GtEq =
        fun(A, B) ->
                NumVotesA = element(2, A),
                NumVotesB = element(2, B),
                if NumVotesA /= NumVotesB ->
                        NumVotesA > NumVotesB;
                   true ->
                        %% Sort on oldest vote in wagon
                        element(3, A) =< element(3, B)
                end
        end,
    VoteSumSort = lists:sort(GtEq, VoteSum2),
    {VoteSumSort, InvalidVotes}.

rem_play_votes(Day, Players) ->
    Votes = Day#mafia_day.votes,
    [V || V <- Votes,
          lists:member(element(1, V), Players)].

%% find oldest vote in unbroken sequence, for ppl reiterating their last votes
-spec user_vote([#vote{}]) -> no_vote | #vote{}.
user_vote(UserVotes) ->
    case lists:foldr(
           fun %% no_vote state
               (V = #vote{valid = false}, no_vote) -> {inv, V};
               (V = #vote{valid = true},  no_vote) -> {val, V};
               %% invalid state
               (V = #vote{valid = true}, {inv, _V}) -> {val, V};
               %% valid state
               (V = #vote{valid = true}, {val, Vacc})
                 when V#vote.vote == Vacc#vote.vote ->
                   {val, V};
               (#vote{valid = true}, {val, Vacc}) ->
                   {pval, Vacc};  %% permanent valid
               %% inv, val or pval states
               (_V, Vacc) -> Vacc
           end,
           no_vote,  %% initial state
           UserVotes) of
        {_, Vote2Use} ->
            Vote2Use;
        no_vote -> no_vote
    end.


pr_eo_ptype(true, Phase = #phase{}) -> pr_eo_ptype(Phase);
pr_eo_ptype(false, Ph = #phase{}) -> print_phase(Ph).

pr_eo_ptype(#phase{num = Num, ptype = ?day}) -> "EoD"++ ?i2l(Num);
pr_eo_ptype(#phase{num = Num, ptype = ?night}) -> "EoN"++ ?i2l(Num);
pr_eo_ptype(#phase{ptype = ?game_ended}) -> "at end of game".

pr_phase_long(#phase{ptype = ?game_ended}) -> "Game End";
pr_phase_long(#phase{num = Num, ptype = Ptype}) ->
    pr_ptype(Ptype) ++ " " ++ ?i2l(Num);
pr_phase_long(?total_stats) -> "Game Global Statistics".

%%%%%%%%%%

%% [{Vote, Num, [{Time, User, Raw}]}]
add_vote(V, User, Acc) ->
    add_vote(V#vote.vote, V#vote.raw, V#vote.time, User, Acc).

add_vote(Vote, Raw, Time, User, Acc) ->
    case lists:keyfind(Vote, 1, Acc) of
        false ->
            [{Vote, 1, [{Time, User, Raw}]} | Acc];
        {_, _NumV, Voters} ->
            Voters2 = Voters ++ [{Time, User, Raw}],
            NumV2 = length(Voters2),
            lists:keystore(Vote, 1, Acc, {Vote, NumV2, Voters2})
    end.

%% -----------------------------------------------------------------------------

%% replace_space
nbsp(Bin) when is_binary(Bin) ->
    nbsp(?b2l(Bin));
nbsp(Str) ->
    [ if C ==$\s -> "&nbsp;";
         C ==$- -> "&#x2011;";
         true -> C end
      || C <- Str].

print_time_5d(G, Time) ->
    print_time_5d_str(
      ?normal,
      mafia_time:hh_mm_to_deadline(G, Time)).

print_time_5d_str(?stats, {HH, MM}) when HH > ?DayHours ->
    Days = HH div ?DayHours,
    HHRem = HH rem ?DayHours,
    if Days < 30 ->
            ?i2l(Days) ++ "d " ++ ?i2l(HHRem) ++ "h " ++ ?i2l(MM) ++ "m";
       true ->
            ?i2l(Days) ++ "days, " ++ ?i2l(HHRem) ++ "hours"
    end;
print_time_5d_str(_, {HH, MM}) ->
    p(HH) ++ ":" ++ p(MM).

%% Flatten a bit sort of plus time sort...
-spec user_vote_timesort([{User :: user(), [#vote{}]}])
                        ->[{User :: user(), #vote{}}].
user_vote_timesort(Votes) ->
    Votes2 =
        lists:foldl(
          fun({UserB, UVotes}, Acc) ->
                  [{?b2l(UserB), V} || V <- UVotes] ++ Acc
          end,
          [],
          Votes),
    SortF = fun({_, #vote{time = TimeA}},
                {_, #vote{time = TimeB}}) ->
                    TimeA =< TimeB
            end,
    lists:sort(SortF, Votes2).

%% -----------------------------------------------------------------------------

print_messages(User) when is_list(User) ->
    print_messages(?l2b(User));
print_messages(User) when is_binary(User) ->
    ThId = ?getv(?thread_id),
    AllMsgIds = mafia_lib:all_msgids(ThId),
    UserMsgIds =
        lists:filter(
          fun(MsgId) ->
                  case ?rmess(MsgId) of
                      [#message{user_name = U}]
                        when U == User -> true;
                      _ -> false
                  end
          end,
          AllMsgIds),
    print_page(ThId, UserMsgIds, fun print_message_summary/1).

%% -----------------------------------------------------------------------------

print_pages_for_thread() ->
    ThId = ?getv(?thread_id),
    print_pages_for_thread(ThId).

print_pages_for_thread(ThId) ->
    Pages = mafia:pages_for_thread(ThId),
    io:format("Thread ~p has stored Pages ~w\n", [ThId, Pages]).

%% -----------------------------------------------------------------------------

-define(MsgTime(M), M#message.time).

print_page(_ThId, [], _PrintFun) -> ok;
print_page(ThId, MsgIds, PrintFun) ->
    %% print starting line with current phase
    %% does this thread have a game?
    MsgsPage = read_msgs(MsgIds),
    case ?rgame(ThId) of
        [] ->
            [PrintFun(M) || M <- MsgsPage];
        [#mafia_game{deadlines = DLs} = G] ->
            TimeB = ?MsgTime((hd(MsgsPage))),
            TimeA = ?MsgTime((lists:last(MsgsPage))),

            %% Are more deadlines needed
            TimeLDl = (hd(DLs))#dl.time,
            DLs2 =
                if TimeLDl < TimeA ->
                        ?lrev(
                          mafia_time:update_deadlines(ThId));
                      true ->
                           DLs
                   end,

            DLsIn = [D || D <- DLs2,
                          TimeB - 3 * ?MinuteSecs < D#dl.time,
                          D#dl.time < TimeA + 3 * ?MinuteSecs],
            MixedSort = lists:sort(fun cmp_time/2, MsgsPage ++ DLsIn),
            case hd(MixedSort) of
                Msg = #message{} ->
                    FirstTime = time(Msg),
                    Phase = mafia_time:calculate_phase(G, FirstTime),
                    print_dl_div_line(Phase, "");
                _ -> ok
            end,
            [case E of
                 M = #message{} ->
                     PrintFun(M);
                 D ->
                     print_dl_div_line(D, "End of ")
             end || E <- MixedSort]
    end,
    ok.

msgids(ThId, PageNum) ->
    case ?rpage(ThId, PageNum) of
        [] -> [];
        [#page_rec{message_ids = MIds}] -> MIds
    end.

read_msgs(MsgIds) -> [hd(?rmess(MsgId)) || MsgId <- MsgIds].

cmp_time(A, B) -> time(A) =< time(B).

time(#dl{time = Time}) -> Time;
time(#message{time = Time}) -> Time.

print_dl_div_line(DL = #dl{}, Txt) ->
    print_dl_div_line(DL#dl.phase, Txt);
print_dl_div_line(Phase = #phase{}, Txt) ->
    print_dl_div_lineI(print_phase(Phase), Txt).

print_dl_div_lineI(PhText, Txt) ->
    io:format("-------------------------------- ~s~s "
              "--------------------------------\n",
              [Txt, PhText]).

print_phase(#phase{ptype = ?game_start}) -> "Game Start";
print_phase(#phase{ptype = ?game_ended}) -> "Game End";
print_phase(#phase{num = Num, ptype = Ptype}) ->
    pr_ptype(Ptype) ++ " " ++ ?i2l(Num).

print_phase_next(Ph = #phase{ptype = Ptype}) when Ptype == ?game_start ->
    print_phase(Ph);
print_phase_next(Ph) ->
    "next " ++ print_phase(Ph) ++ " deadline".

pr_ptype(?game_start) -> "Game Start";
pr_ptype(?game_ended) -> "Game End";
pr_ptype(?day) -> "Day";
pr_ptype(?night) -> "Night".

%% -----------------------------------------------------------------------------

print_message_full(Fd, M) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_message_full(#pp{message = M, dev = Fd, time_zone = TzH, dst = Dst}).

print_message_full(M = #message{}) ->
    print_message_full(?standard_io, M);
print_message_full(PP = #pp{}) ->
    #pp{message = M, dev = Fd} = PP,
    io:format(Fd,
              "User  : ~s\n"
              "Page  : ~s\n"
              "Time  : ~s\n"
              "Thread: ~s\n"
              "Msg id: ~s\n"
              "Wrote : \"~s\"\n"
              "\n",
              [?b2l(M#message.user_name),
               ?i2l(M#message.page_num),
               print_timeI(PP#pp{t_mode = ?long}),
               ?i2l(M#message.thread_id),
               ?i2l(M#message.msg_id),
               html2txt(?b2l(M#message.message))
              ]).

%% -----------------------------------------------------------------------------

print_message_summary(M = #message{}) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_message_summary(#pp{message = M, time_zone = TzH, dst = Dst});
print_message_summary(PP = #pp{}) ->
    #pp{message = M} = PP,
    Msg = rm_nl(html2txt(?b2l(M#message.message))),
    MsgLen = length(Msg),
    Max = 80,
    MsgShort = if MsgLen > Max ->
                       string:left(Msg, Max) ++ "...";
                  true -> Msg
               end,
    Str = io_lib:format("~-10s "
                        "~-3s"
                        " ~-11s "
                        "~-7s "
                        "~s\n",
                        [?b2l(M#message.user_name),
                         ?i2l(M#message.page_num),
                         print_timeI(PP#pp{t_mode = ?short}),
                         ?i2l(M#message.msg_id),
                         MsgShort
                        ]),
    io:format("~s", [Str]).

%% -----------------------------------------------------------------------------

print_game_time(G, Time, Mode) ->
    {TzH, Dst} = mafia_time:get_tz_dst(G, Time),
    print_time(Time, TzH, Dst, Mode).

%% /1
print_time(Opts) when is_list(Opts) ->
    print_timeI(Opts);
print_time(?console) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_timeI([{?use_time, mafia_time:utc_secs1970()},
                 {?time_zone, TzH},
                 {?dst, Dst},
                 {?t_mode, short}]).

%% /2
print_time(?console, Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_timeI([{?use_time, Time},
                 {?time_zone, TzH},
                 {?dst, Dst},
                 {?t_mode, short}]);
print_time(Time, Mode) when is_integer(Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_time(Time, TzH, Dst, Mode);
print_time(Time = {{_,_,_},{_,_,_}}, Mode) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_time(Time, TzH, Dst, Mode).

%% /4
print_time(Time, TzH, Dst, Mode) ->
    print_timeI([{?use_time, Time}, {?time_zone, TzH},
                 {?dst, Dst}, {?t_mode, Mode}]).

%% /1
print_timeI(Opts) when is_list(Opts) ->
    DefOpts = [{?time_zone, 0},
               {?dst, ?false},
               {?t_mode, ?long}],
    DefPP = po(#pp{}, DefOpts),
    print_timeI(po(DefPP, Opts));

%% half this fun should go to mafia_time
print_timeI(PP = #pp{}) when PP#pp.use_time == ?undefined ->
    Time = case PP#pp.message of
               ?undefined -> mafia_time:utc_secs1970();
               M -> M#message.time
           end,
    print_timeI(PP#pp{use_time = Time});
print_timeI(PP = #pp{}) ->
    #pp{use_time = Time,
        time_zone = TzH,
        dst = Dst,
        t_mode = Mode} = PP,
    try
        {{Y, M, D}, {HH, MM, SS}} =
            if is_integer(Time) ->
                    mafia_time:secs1970_to_local_datetime(Time, TzH, Dst);
               true -> Time
            end,
        DstStr = case {Dst, Mode} of
                     {?false, ?extensive} -> ", Normal Time";
                     {?true, ?extensive} -> ", Daylight Saving Time";
                     {?false, Mode} -> "";
                     {?true, Mode} -> " DST"
                 end,
        Char = case TzH of
                   0 -> "Z";
                   _ -> "T"
               end,
        Out = case Mode of
                  ?file_suffix ->
                      %% 170102Z235959
                      io_lib:format(
                        "~s~s~s~s~s~s~s",
                        [string:right(p(Y), 2), p(M), p(D),
                         Char, p(HH), p(MM), p(SS)]);

                  ?short ->
                      io_lib:format("~s-~s~s~s:~s",
                                    [p(M), p(D), Char, p(HH), p(MM)]);
                  ?long ->
                      %% 2017-01-18Z01:02:01 (0)
                      io_lib:format("~s-~s-~s~s~s:~s:~s (~s~s)",
                                    [p(Y), p(M), p(D), Char, p(HH), p(MM), p(SS),
                                     ?i2l(TzH), DstStr]);
                  ?local ->
                      io_lib:format("~s-~s-~s ~s:~s:~s",
                                    [p(Y), p(M), p(D), p(HH), p(MM), p(SS)]);
                  ?human ->
                      io_lib:format("~s-~s-~s ~s:~s:~s (TZ: ~s~s)",
                                    [p(Y), p(M), p(D), p(HH), p(MM), p(SS),
                                     ?i2l(TzH), DstStr]);
                  ?extensive ->
                      io_lib:format("~s-~s-~s ~s:~s:~s (Timezone: ~s~s)",
                                    [p(Y), p(M), p(D), p(HH), p(MM), p(SS),
                                     ?i2l(TzH), DstStr])
              end,
        lists:flatten(Out)
    catch _:_ -> ""
    end.

p(I) when I > 9 -> ?i2l(I);
p(I) -> string:right(?i2l(I), 2, $0).

rm_nl([$\n|T]) -> [$\s|rm_nl(T)];
rm_nl([H|T]) -> [H|rm_nl(T)];
rm_nl("") -> "".

%% skip unicode for a while
html2txt("&gt;" ++ T) -> [ $> | html2txt(T)];
html2txt("&lt;" ++ T) -> [ $< | html2txt(T)];
html2txt("&amp;" ++ T) -> [ $& | html2txt(T)];
html2txt("&acute;" ++ T) -> [ $´ | html2txt(T)];
html2txt("&lsquo;" ++ T) -> [ $' | html2txt(T)];
html2txt("&rsquo;" ++ T) -> [ $' | html2txt(T)];
html2txt("&ldquo;" ++ T) -> [ $\" | html2txt(T)];
html2txt("&rdquo;" ++ T) -> [ $\" | html2txt(T)];
html2txt("&hellip;" ++ T) -> [ $\., $\., $\. | html2txt(T)];
%% html2txt("&lsquo;" ++ T) -> [ $‘ | html2txt(T)];
%% html2txt("&rsquo;" ++ T) -> [ $’ | html2txt(T)];
%% html2txt("&ldquo;" ++ T) -> [ $“ | html2txt(T)];
%% html2txt("&rdquo;" ++ T) -> [ $” | html2txt(T)];
html2txt("<br />" ++ T) ->  [ $\n | html2txt(T)];
html2txt([H|T]) -> [H|html2txt(T)];
html2txt("") -> "".
