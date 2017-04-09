-module(mafia_print).

-define(NumColsInGrp, 6).

%% Manual API
-export([
         print_stats/0, print_stats/1, print_stats/2,
         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2,

         print_tracker/1
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

         web_vote_tracker/1,
         html2txt/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

%% print params
-record(
   pp,
   {game  :: ?undefined | #mafia_game{},
    day   :: ?undefined | #mafia_day{},
    players_rem :: ?undefined | [player()], %% for vote-count, non-votes,
                                                % non-posts
    players_vote :: ?undefined | [player()], %% for vote tracker
    game_key :: ?undefined | thread_id(),
    phase  :: ?undefined | #phase{} | ?total_stats,
    day_num :: ?undefined | integer(),
    message :: ?undefined | #message{},
    msg_id :: ?undefined | msg_id(),
    match_expr :: term(),
    dev = standard_io,
    mode = ?text :: ?text | ?html,
    t_mode = ?long :: ?short | ?long | ?extensive | ?file_suffix,
    period :: ?undefined | integer(),   %% Poll period
    use_time :: ?undefined | seconds1970(),
    %% use_time = time to next DL (current game status)
    %%time :: seconds1970(),
    time_zone = 0 :: integer(),
    dst = false :: boolean(),
    sort = ?normal :: ?normal | ?words | ?words_per_post
   }).

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

pp(Id, Page) ->
    ppI(?thid(Id), Page).

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

pps(Id, Page) ->
    ppsI(?thid(Id), Page).

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
    pm(standard_io, MsgId);
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

don_arg(DoN) ->
    if DoN == d; DoN == day; DoN == ?day -> ?day;
       DoN == n; DoN == night; DoN == ?night -> ?night
       %% DoN == e; DoN == 'end'; DoN == ?game_end -> ?game_ended
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
setup_pp(PP) -> PP.

%% -----------------------------------------------------------------------------

%% /0 human
print_votes() ->
    GameKey = ?getv(?game_key),
    Phase = mafia_time:calculate_phase(GameKey),
    print_votes([{?phase, Phase}]).

%% /2 human
print_votes(DayNum, DoN) ->
    DoN2 = don_arg(DoN),
    print_votes([{?phase, #phase{num = DayNum, don = DoN2}}]).

%% /1 generic
print_votes(Opts) ->
    DefOpts = [{?game_key, ?getv(?game_key)},
               {?mode, ?text},
               {?dev, standard_io}],
    PP = po(#pp{}, DefOpts),
    PP2 = po(PP, Opts),
    print_votesI(PP2).

print_votesI(PPin) ->
    PP = setup_pp(PPin),
    PhaseType = case PP#pp.phase of
                    #phase{don = ?game_ended} -> ?game_ended;
                    #phase{don = DoN} -> DoN
                end,
    #pp{game = G, day = Day} = PP,
    RealRemPlayers = PP#pp.players_rem,

    %% Idea:
    %% IsEnd -> EndTime in Game TZ for DL (4 last DLs passed)
    %% IsCurrentPhase -> Time to DL (next 4 DL)
    %% not IsCurrentPhase -> DLTime in Game TZ (No DLs)

    %% Part - Page heading - Print Game Name
    DoDispTime2DL = PhaseType /= ?game_ended andalso is_integer(PP#pp.use_time),
    GName = ?b2l(G#mafia_game.name),
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
                ModMsgV = ?getv(?mod_msg),
                ModMsg =
                    if DoDispTime2DL, is_list(ModMsgV), ModMsgV /= "" ->
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
                 "</td></tr>\r\n"]
        end,

    %% Part - Display time Left to Deadline or display End of Game message
    HDeadLine =
        if PP#pp.mode == ?text ->
                if DoDispTime2DL ->
                        print_time_left_to_dl(PP);
                   PhaseType == ?game_ended ->
                        {EndTime, EndMsgId} = G#mafia_game.game_end,
                        {TzH, Dst} = mafia_time:get_tz_dst(G, EndTime),
                        LastPhase =
                            print_phase((hd(G#mafia_game.deadlines))#dl.phase),
                        io:format(
                          PP#pp.dev,
                          "\n"
                          "The GAME HAS ENDED in phase ~s "
                          "and the time was ~s\n",
                          [LastPhase,
                           print_time(EndTime, TzH, Dst, ?extensive)]),
                        io:format(PP#pp.dev,
                                  "\n"
                                  "Game Master End Message\n"
                                  "-----------------------\n",
                                  []),
                        pm(PP#pp{msg_id = EndMsgId,
                                 time_zone = TzH,
                                 dst = Dst});
                   true -> ok
                end;
           PP#pp.mode == ?html ->
                if DoDispTime2DL ->
                        print_time_left_to_dl(PP);
                   PhaseType == ?game_ended ->
                        {EndTime, EndMsgId} = G#mafia_game.game_end,
                        {TzH, Dst} = mafia_time:get_tz_dst(G, EndTime),
                        LastPhase =
                            print_phase((hd(G#mafia_game.deadlines))#dl.phase),
                        GmMessage = web_impl:show_msg(EndMsgId),
                        ["<tr><td align=center>",
                         "The GAME HAS ENDED in phase ",
                         LastPhase,
                         " and the time was ",
                         print_time(EndTime, TzH, Dst, ?extensive),
                         "</td></tr>"
                         "<tr><td align=center>"
                         "<table cellpadding=6 cellspacing=3>",
                         GmMessage, "</table></td></tr>"];
                   true ->
                        []
                end
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
            if (PP#pp.phase)#phase.don == ?game_ended -> true;
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
                ["<td><tr>"
                 "<table align=center cellpadding=2 cellspacing=2><tr>"
                 "<tr><th colspan=", ?i2l(?NumColsInGrp),
                 ">Remaining players"
                 "</th></tr>",
                 object_rows(RPRows),
                 "</table></td></tr>"]
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
        if PhaseType == ?day -> print_tracker(PP);
           true -> [[], []]
        end,

    %% Part - Posting stats
    PPstat = if (PP#pp.phase)#phase.don == ?game_ended ->
                     PP#pp{phase = ?total_stats};
                true -> PP
             end,
    [HStats, HNonPosters] = print_statsI(PPstat),

    %% Part - Dead players
    DoReport =
        fun(DPhase) ->
                if (PP#pp.phase)#phase.don == ?game_ended -> true;
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
                " died " ++ pr_eodon(IsEnd, Ph)
        end,
    PrRepFun =
        fun(Ph) ->
                " was replaced " ++ pr_eodon(false, Ph) ++ " by "
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
                 "</th></tr>",
                 if IsZeroDeaths, IsZeroReplacements ->
                         "<tr><th>(none)</th></tr>";
                    true ->
                         [case DoR of
                              #death{player = DeadPl,
                                     is_end = IsEnd,
                                     phase = Ph,
                                     msg_id = MsgId,
                                     comment = Com} ->
                                  ["<tr><td><table align=left><tr><td",
                                   bgcolor(DeadPl), ">", ?b2l(DeadPl), "</td>"
                                   "<td><a href=\"/e/web/msg?id=",
                                   ?i2l(MsgId), "&player=", ?b2l(DeadPl),
                                   "&var=death\">",
                                   PrFun(IsEnd, Ph), "</a>",
                                   if Com == ?undefined ->
                                           " - msg: " ++ ?i2l(MsgId);
                                      is_binary(Com) ->
                                           " - " ++ ?b2l(Com)
                                   end,
                                   "</td></tr></table></td></tr>"];
                              #replacement{new_player = NewPl,
                                           replaced_player = RepPl,
                                           phase = Ph,
                                           msg_id = MsgId} ->
                                  ["<tr><td><table align=left><tr><td",
                                   bgcolor(RepPl), ">", ?b2l(RepPl), "</td>"
                                   "<td>",
                                   "<a href=\"/e/web/msg?id=",
                                   ?i2l(MsgId), "&player=", ?b2l(RepPl),
                                   "&var=replacement\">",
                                   PrRepFun(Ph), "</a></td>"
                                   "<td", bgcolor(NewPl), ">", ?b2l(NewPl),
                                   "</td></tr></table></td></tr>"]
                          end
                          || DoR <- DeathsToReport]
                 end,
                 "</table>"]
    end,

    %% Part - Deadlines
    HDeadlines =
        if DoDispTime2DL ->
                print_num_dls(PP, 4);
           true -> []
        end,

    %% Part - Footer
    HFooter =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev, "\n", []),
                if is_integer(PP#pp.period) ->
                        io:format(
                          PP#pp.dev,
                          "Updates currently every ~p minutes "
                          "(more often near deadlines).\n",
                          [PP#pp.period]);
                   true -> ok
                end,
                if is_integer(PP#pp.game_key) ->
                        io:format(
                          PP#pp.dev,
                          "Mafia game thread at: ~s\n",
                          [?UrlBeg ++ ?i2l(PP#pp.game_key)]);
                   true -> ok
                end;
           PP#pp.mode == ?html ->
                ["<tr><td align=center><br>",
                 if is_integer(PP#pp.period) ->
                         ["Updates currently every ", ?i2l(PP#pp.period),
                          " minutes (more often near deadlines)."
                          "<br>"];
                    true -> []
                 end,
                 if is_integer(PP#pp.game_key) ->
                         ["Mafia game thread at: ", ?UrlBeg,
                          ?i2l(PP#pp.game_key)];
                    true -> ""
                 end,
                 "</td></tr>"]
        end,
    if PP#pp.mode == ?text -> ok;
       PP#pp.mode == ?html ->
            [HTitle, "\r\n",
             HDeadLine, "\r\n",
             HThreadLinks, "\r\n",
             HVoteCount, "\r\n",
             EndVotes, "\r\n",
             NonVotes, "\r\n",
             "<br>",
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
     #dl{phase = #phase{num = Num, don = DoN}}} =
        mafia_time:get_next_deadline(PP#pp.game_key, PP#pp.use_time),
    DayStr = if Days == 0 -> "";
                true -> ?i2l(Days) ++ " day, "
             end,
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev,
                      "\n"
                      "Remaining time to next ~s ~p deadline:"
                      "  ~s~p hours, ~p minutes\n",
                      [pr_don(DoN), Num, DayStr, HH, MM]);
       PP#pp.mode == ?html ->
            ["<tr><th>",
             "<center>Remaining time to next ",
             pr_don(DoN), " ", ?i2l(Num), " deadline:"
             "  ", DayStr, ?i2l(HH), " hours, ", ?i2l(MM), " minutes\n",
             "</center></th></tr>"]
    end.

print_num_dls(PP, Num) ->
    Time = mafia_time:utc_secs1970(),
    G = PP#pp.game,
    DLs = mafia_time:next_deadlines(G, Time, Num),
    {PastDLs, ComingDLs} =
        lists:partition(fun(DL) -> DL#dl.time < Time end, DLs),
    Past2 = prep_dl_info(G, Time, PastDLs),
    Coming2 = prep_dl_info(G, Time, ComingDLs),
    if PP#pp.mode == ?text ->
            print_past_dls_text(PP, Past2, "Deadlines in the past"),
            print_past_dls_text(PP, Coming2, "Deadlines in the future");
       PP#pp.mode == ?html ->
            ["<br>"
             "<table border=1 align=center ", ?BG_TURQUOISE, ">",
             print_past_dls(Past2, "Deadlines in the past"),
             print_past_dls(Coming2, "Deadlines in the future"),
             "</table>"]
    end.

print_past_dls_text(_PP, DLs, _Title) when length(DLs) =< 0 -> ok;
print_past_dls_text(PP, DLs, Title) ->
    Fmt = "~-8s ~-13s ~s\n",
    io:format(PP#pp.dev,
              "\n"
              "~s\n"
              "~s\n"
              ++ Fmt ++ Fmt,
              [Title, ul($-, Title),
               "Phase", "Relative now", "Absolute time",
               ul($-, "Phase"), ul($-, "Relative now"), ul($-, 56)]),
    [io:format(PP#pp.dev,
               Fmt,
               [[DoNStr, " ", Nstr],
                [?i2l(Days), "D ",?i2l(HH), "H ", ?i2l(MM), "M"],
                TimeStr])
     || {Nstr, DoNStr, {Days, {HH, MM, _}}, TimeStr} <- DLs].

ul(Char, N) when is_integer(N), N > 0 -> [Char|| _ <- lists:seq(1, N)];
ul(Char, Str) when is_list(Str) -> [Char || _ <- Str].

prep_dl_info(G, Time, DLs) ->
    [{?i2l(N), pr_don(DoN),
      mafia_time:secs2day_hour_min_sec(Time - DLT),
      print_game_time(G, DLT, ?extensive)}
     || #dl{phase = #phase{num = N, don = DoN}, time = DLT} <- DLs].

print_past_dls(DLs, _Title) when length(DLs) =< 0 -> [];
print_past_dls(DLs, Title) ->
    ["<tr>"
     "<th colspan=3 align=center>", Title, "</th>"
     "</tr>",
     "<tr>"
     "<th>Phase</th><th>Relative now</th><th>Absolute time</th>"
     "</tr>",
     [["<tr><td>", DoNStr, " ", Nstr, "</td>"
       "<td>", ?i2l(Days), "D ",?i2l(HH), "H ", ?i2l(MM), "M","</td>"
       "<td>", TimeStr, "</td>"
       "</tr>"]
      || {Nstr, DoNStr, {Days, {HH, MM, _}}, TimeStr} <- DLs]].


pr_thread_links(PP, _DoDispTime2DL)
  when (PP#pp.phase)#phase.don == ?game_ended ->
    Link = "<a href=\"/e/web/msgs?part=end\">Game End</a>",
    ["<tr><td align=center>Messages for this phase: ", Link,
     "</td></tr>"];
pr_thread_links(PP, DoDispTime2DL) ->
    %% Day4(p98-) p103-, p108-, p113-, p118-, p123-, p128-, last(p130)
    StartTime = mafia_time:get_time_for_prev_phase(PP#pp.game, PP#pp.phase),
    EndTime = mafia_time:get_time_for_phase(PP#pp.game, PP#pp.phase),
    PageKeys = mafia_lib:all_page_keys(PP#pp.game_key),
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
            UrlPart = "/e/web/msgs?part=p",
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
            Links = my_string_join( Links0 ++ [LastLink], " "),
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
                     io:format(PP#pp.dev, "~p ~s - ", [N, ?b2l(Vote)]),
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


pr_eodon(true, Phase = #phase{}) ->
    pr_eodon(Phase);
pr_eodon(false, #phase{num = Num, don = DoN}) ->
    pr_don(DoN) ++ " " ++ ?i2l(Num).

pr_eodon(#phase{num = Num, don = ?day}) -> "EoD"++ ?i2l(Num);
pr_eodon(#phase{num = Num, don = ?night}) -> "EoN"++ ?i2l(Num);
pr_eodon(#phase{don = ?game_ended}) -> "at end of game".

pr_phase_long(#phase{don = ?game_ended}) -> "Game End";
pr_phase_long(#phase{num = Num, don = DoN}) -> pr_don(DoN) ++ " " ++ ?i2l(Num);
pr_phase_long(?total_stats) -> "Game Global Statistics".

%% Manual API
print_stats() -> print_stats_opts([]).

print_stats(e) -> print_stats_opts([{?phase, #phase{don = ?game_ended}}]);
print_stats(Opts) when is_list(Opts) -> print_stats_opts(Opts).

print_stats(Num, DoN) ->
    DoN2 = don_arg(DoN),
    print_stats_opts([{?phase, #phase{num = Num, don = DoN2}}]).

print_stats_opts(Opts) ->
    DefOpts = [{?game_key, ?getv(?game_key)},
               {?phase, #phase{num = 1, don = ?day}},
               {?dev, standard_io},
               {?sort, ?normal}],
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
    MatchHead = #stat{key = {'$1', '$2'}, _='_'},
    Guard = [{'==', '$2', PP#pp.game_key}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP) when (PP#pp.phase)#phase.don == ?game_ended ->
    %% END stats
    MatchHead = #stat{key = {'$1', '$2', '$3'}, _='_'},
    Guard = [{'==', '$2', PP#pp.game_key},
             {'==', '$3', ?game_ended}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP) ->
    %% PHASE stats
    #phase{num = Day, don = DoN} = PP#pp.phase,
    MatchHead = #stat{key = {'$1', '$2', {'$3', '$4'}}, _='_'},
    Guard = [{'==', '$2', PP#pp.game_key},
             {'==', '$3', Day}, {'==', '$4', DoN}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr}).

do_print_stats(PP) ->
    MatchExpr = PP#pp.match_expr,
    Stats = mnesia:dirty_select(stat, MatchExpr),
    PrStats = [mk_prstat(S) || S <- Stats],
    do_print_stats(PP, PrStats).

-spec mk_prstat(#stat{}) -> #prstat{}.
mk_prstat(S = #stat{}) ->
    #prstat{key = S#stat.key,
            msg_ids = S#stat.msg_ids,
            num_chars = S#stat.num_chars,
            num_words = S#stat.num_words,
            num_postings = S#stat.num_postings,
            words_per_post = S#stat.num_words / S#stat.num_postings
           }.

do_print_stats(PP, PrStats) ->
    SortFun =
        case PP#pp.sort of
            ?normal ->
                fun(#prstat{num_postings = PA, num_words = WA},
                    #prstat{num_postings = PB, num_words = WB}) ->
                        if PA < PB -> true;
                           PA > PB -> false;
                           WA =< WB -> true;
                           true -> false
                        end
                end;
            ?words ->
                fun(#prstat{num_words = WA},
                    #prstat{num_words = WB}) ->
                        WA =< WB
                end;
            ?words_per_post ->
                fun(#prstat{words_per_post = WA},
                    #prstat{words_per_post = WB}) ->
                        WA =< WB
                end
        end,
    StatsSorted = lists:sort(SortFun, PrStats),
    PrFn = fun(tr, S) -> element(1, S#prstat.key);
              %% transl(element(1, S#prstat.key));
              (cell, _) -> "td";
              (bgcolor, S) -> bgcolor(element(1, S#prstat.key)
                                      %% transl(element(1, S#prstat.key))
                                     );
              (_, _) -> []
           end,
    PlayersRem = PP#pp.players_rem,
    NonPosters = [PRem || PRem <- PlayersRem]
        -- [PrFn(tr, S) || S <- PrStats],
    Phase = PP#pp.phase,
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
                ArgBeg = "stats?" ++ phase_args(PP#pp.phase) ++ "&sort=",
                PostLn = ArgBeg ++ "normal",
                WordLn = ArgBeg ++ "words",
                WPostLn = ArgBeg ++ "words_per_post",
                {PostTitle, WordTitle, WPostTitle} =
                    case PP#pp.sort of
                        ?normal ->
                            {"Posts",
                             ["<a href=\"", WordLn, "\">Words</a>"],
                             ["<a href=\"", WPostLn, "\">W/Post</a>"]
                            };
                        ?words ->
                            {["<a href=\"", PostLn, "\">Posts</a>"],
                             "Words",
                             ["<a href=\"", WPostLn, "\">W/Post</a>"]
                            };
                        ?words_per_post ->
                            {["<a href=\"", PostLn, "\">Posts</a>"],
                             ["<a href=\"", WordLn, "\">Words</a>"],
                             "W/Post"
                            }
                    end,
                ["<tr><th colspan=\"5\">",
                 "Posting statistics (",
                 pr_phase_long(Phase),
                 ")\n",
                 "</th></tr>",
                 "<tr><th align=\"right\">", PostTitle, "</th>"
                 "<th align=\"right\">", WordTitle, "</th>"
                 "<th align=\"right\">Chars</th>"
                 "<th align=\"right\">", WPostTitle, "</th>"
                 "<th align=\"left\">Player</th></tr>"
                ]
        end,
    print_stat_div(PP),
    {SumStat, Html1} =
        lists:foldl(
          fun(S, {Sum, Html}) ->
                  {mafia_data:sum_stat(S, Sum),
                   Html ++ print_stat_row(PP, S, PrFn)}
          end,
          {#prstat{msg_ids = [],
                   num_chars = 0,
                   num_words = 0,
                   num_postings = 0,
                   words_per_post = 0.0
                  },
           HtmlHead},
          ?lrev(StatsSorted)),
    print_stat_div(PP),
    Html2 = Html1 ++
        print_stat_row(PP, SumStat, fun(cell, _) -> "th";
                                       (tr, _) -> "Total Counts";
                                       (_, _) -> []
                                    end),
    HtmlStats = ["<br><table align=center ", ?BG_TURQUOISE, ">",
                 Html2, "</table>"],
    NonPostTitle =
        if NonPosters == [] -> "Non-posters: -";
           true -> "Non-posters"
        end,
    NPRows = split_into_groups(?NumColsInGrp, NonPosters),
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

phase_args(?total_stats) -> "phase=total";
phase_args(#phase{don = ?game_ended}) -> "phase=end";
phase_args(#phase{don = DoN, num = DNum}) ->
    Ph = "phase=" ++ if DoN == ?day -> "day";
                        DoN == ?night -> "night"
                     end,
    Num = "&num=" ++ ?i2l(DNum),
    Ph ++ Num.

print_stat_div(PP) when PP#pp.mode == ?text ->
    Line = "-----------",
    io:format(PP#pp.dev,
              "~5s ~6s ~7s ~6s ~s\n",
              [Line, Line, Line, Line, Line]
             );
print_stat_div(_PP) ->
    [].

print_stat_row(PP, S, PrFn) when PP#pp.mode == ?text ->
    io:format(PP#pp.dev,
              "~5s ~6s ~7s ~6.2f ~s\n",
              [i2l(S#prstat.num_postings, 5),
               i2l(S#prstat.num_words, 6),
               i2l(S#prstat.num_chars, 7),
               S#prstat.words_per_post,
               PrFn(tr, S)
              ]),
    [];
print_stat_row(PP, S, PrFn) when PP#pp.mode == ?html ->
    CBegR = ["<", PrFn(cell, S), " align=\"right\">"],
    CBegL = ["<", PrFn(cell, S), " align=\"left\">"],
    CEnd = ["</", PrFn(cell, S), ">"],
    ["<tr", PrFn(bgcolor, S), ">",
     CBegR, ?i2l(S#prstat.num_postings),
     CEnd, CBegR, ?i2l(S#prstat.num_words),
     CEnd, CBegR, ?i2l(S#prstat.num_chars),
     CEnd, CBegR, io_lib:format("~.2f", [S#prstat.words_per_post]),
     CEnd, CBegL, PrFn(tr, S),
     CEnd, "</tr>\r\n"].

%% Get user name as stored normal case string
%% transl(UserUB) ->
%%     UInfo = hd(?ruserUB(UserUB)),
%%     ?b2l(UInfo#user.name).

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

%% human
web_vote_tracker(DayNum) ->
    GameKey = ?getv(?game_key),
    Phase = #phase{num = DayNum, don = ?day},
    PP = #pp{game_key = GameKey,
             day_num = DayNum,
             phase = Phase,
             mode = ?html},
    web_vote_tracker(PP, ?rgame(GameKey), ?rday(GameKey, Phase)).

web_vote_tracker(_PP, [], _) -> ok;
web_vote_tracker(PP, [Game], Day) ->
    PP2 = PP#pp{game = Game,
                day = Day},
    PP3 = setup_pp(PP2),
    print_tracker(PP3).

print_tracker(PP) when PP#pp.day_num == ?undefined ->
    print_tracker(PP#pp{day_num = (PP#pp.phase)#phase.num});
print_tracker(PP) ->
    %% player_deaths contains players dying in the middle of the day.
    AllPlayersB = PP#pp.players_vote,
    Abbrs = mafia_name:get_abbrevs(AllPlayersB),
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev, "\n", []);
       true -> ok
    end,
    RKhtml = print_read_key(PP, Abbrs),
    VThtml = print_tracker_tab(PP, Abbrs, AllPlayersB),
    [RKhtml, VThtml].

%% Record definitions for fun print_tracker_tab/3
%% iterated vote per user: User and Vote
-record(iv, {n, u, ub, v, vlong}).
%% u = UserStr, ub = UserBin, v=Abbr, vlong = FullUserName

%% Acc record when iterating over all votes in vote tracker
-record(ra, {ivs, html}).

print_tracker_tab(PP, Abbrs, AllPlayersB) ->
    #mafia_day{votes = Votes0, player_deaths = Deaths} = PP#pp.day,
    Votes = [V || V <- Votes0,
                  lists:member(element(1, V), AllPlayersB)],
    Votes3 = user_vote_timesort(Votes),
    PrAbbrF = fun("---") -> "---";
                 ("INV") -> "INV";
                 (V) -> mafia_name:get3l(V, Abbrs, "***")
              end,
    IterVotes = [non_ivote(UserB) || UserB <- AllPlayersB],
    FmtVoter = " ~s Time |Voter, Move|Vote Count per Vote\n",
    FmtTime  = " ~s =====|===========|==========================\n",
    Head =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\n"
                          "Vote tracker\n"
                          "------------\n",
                          []),
                io:format(PP#pp.dev,
                          FmtVoter,
                          [pr_ivs_user(IterVotes, PrAbbrF)]),
                io:format(PP#pp.dev,
                          FmtTime,
                          [pr_ivs_user(IterVotes, fun(_) -> "===" end)]);
           PP#pp.mode == ?html ->
                ["<table ", ?BG_TURQUOISE, ">", pr_head_html(IterVotes, PrAbbrF)]
        end,

    #ra{html = Html} =
        lists:foldl(
          fun({User, V = #vote{}}, RA = #ra{ivs = IVs0}) ->
                  IVs = reset_dead_votes(IVs0, V#vote.time, Deaths),
                  {NewIVs, PrIVs} =
                      if V#vote.valid ->
                              VFull = ?b2l(V#vote.vote),
                              NewVote = PrAbbrF(VFull),
                              IVs2 = set_ivote(IVs, User, NewVote, VFull),
                              {IVs2, IVs2};
                         not V#vote.valid ->
                              VFull = "INVALID",
                              NewVote = "INV",
                              IVs2 = set_ivote(IVs, User, NewVote, VFull),
                              {IVs, IVs2}
                      end,
                  %% calc vote move
                  OVote = case lists:keyfind(User, #iv.u, IVs) of
                              false -> "";
                              #iv{v = "---"} -> "";
                              #iv{v = V2} -> V2
                          end,
                  NVote = case lists:keyfind(User, #iv.u, NewIVs) of
                              false -> "";
                              #iv{v = "---"} -> "";
                              #iv{v = V3} -> V3
                          end,
                  VoteMove = {OVote, NVote},
                  %% calc standing
                  Stand =
                      ?lrev(
                         lists:sort(
                           lists:foldl(
                             fun(Iv, A) when Iv#iv.v /= "---",
                                             Iv#iv.v /= "Unv" ->
                                     %% first use of #iv.n
                                     case lists:keyfind(Iv#iv.v, #iv.v, A) of
                                         false -> [Iv#iv{n = 1} | A];
                                         Cnt = #iv{n = N} ->
                                             lists:keyreplace(
                                               Iv#iv.v, #iv.v, A,
                                               Cnt#iv{n = N + 1})
                                     end;
                                (_, A) -> A
                             end,
                             [],
                             NewIVs))),
                  PrStand = lists:sublist(Stand, 4),
                  TimeStr = print_time_5d(PP#pp.game, V#vote.time),
                  if PP#pp.mode == ?text ->
                          io:format(PP#pp.dev,
                                    "~s~s~s\n",
                                    [pr_ivs_vote_txt(PrIVs, User),
                                     TimeStr,
                                     pr_stand_txt(User, VoteMove,
                                                  Abbrs, PrStand)
                                    ]),
                          RA#ra{ivs = NewIVs};
                     PP#pp.mode == ?html ->
                          RA#ra{ivs = NewIVs,
                                html =
                                    [RA#ra.html|
                                     ["<tr>",
                                      pr_ivs_vote_html(PrIVs, User, V#vote.id),
                                      "<td>", TimeStr, "</td>",
                                      pr_stand_html(User, V#vote.id, VoteMove,
                                                    Abbrs, PrStand),
                                      "</tr>\r\n"]]
                               }
                  end
          end,
          #ra{ivs = IterVotes, html = []},
          Votes3),
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev,
                      FmtTime,
                      [pr_ivs_user(IterVotes, fun(_) -> "===" end)]),
            io:format(PP#pp.dev,
                      FmtVoter,
                      [pr_ivs_user(IterVotes, PrAbbrF)]);
       PP#pp.mode == ?html ->
            Tab2 = [Head, Html,
                    [pr_head_html(IterVotes, PrAbbrF),
                     "</table>\r\n"]],
            ["<br><table align=center>",
             "<tr><th>Vote Tracker (Day ", ?i2l(PP#pp.day_num), ")</th></tr>",
             "<tr><td>", Tab2, "</td></tr>",
             "</table>"]
    end.

set_ivote(IVs, User, NewVote, VFull) ->
    IV = lists:keyfind(User, #iv.u, IVs),
    lists:keyreplace(User, #iv.u, IVs,
                     IV#iv{v = NewVote, vlong = VFull}).

non_ivote(UserB) -> #iv{u = ?b2l(UserB), ub = UserB, v = "---", vlong = ""}.

reset_dead_votes(IVs, VoteTime, Deaths) ->
    %% check if someone in Deaths has died by V#vote.time
    %% if so replace with NonIVote(User)
    DeathsAtVote = [D#death.player || D = #death{time = DTime} <- Deaths,
                                      DTime =< VoteTime],
    [case lists:member(UserB, DeathsAtVote) of
         true -> IV#iv{v = "---", vlong = ""};
         false -> IV
     end || IV = #iv{ub = UserB} <- IVs].

pr_head_html(IterVotes, PrAbbrF) ->
    ["<tr>",
     %% "<th align=\"right\">Voter</th>"
     %% "<th>Time</th>",
     pr_ivs_user_html(IterVotes, PrAbbrF),
     "<th>Time</th>"
     "<th>Voter</th>",
     "<th>Move</th>",
     "<th colspan=4 align=\"left\">",
     nbsp("Vote Count per Vote"),
     "</th>"
     "</tr>\r\n"].

%% replace_space
nbsp(Bin) when is_binary(Bin) ->
    nbsp(binary_to_list(Bin));
nbsp(Str) ->
    [ if C ==$\s -> "&nbsp;"; true -> C end
      || C <- Str].

print_time_5d(G, Time) ->
    {HH, MM} = mafia_time:hh_mm_to_deadline(G, Time),
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

-define(ReadKeyCols, 5).

print_read_key(PP, Abbrs) when PP#pp.mode == ?html ->
    ["<center><table>\r\n",
     "<tr><th colspan=\"" ++ ?i2l(?ReadKeyCols) ++ "\">",
     "<br>", "Reading Key - Vote Tracker",
     "</th></tr>\r\n",
     prk_html(PP, Abbrs),
     "</table></center>"];
print_read_key(PP, Abbrs) ->
    NumCols = 19,
    CFmt = "~-" ++ ?i2l(NumCols) ++ "s",
    AbbrStrs = [A ++ " " ++ Pl || {_, Pl, A, _} <- Abbrs],
    io:format(PP#pp.dev,
              "Read Key\n"
              "--------\n",
              []),
    prk(PP, CFmt, AbbrStrs).

prk(_PP, _CFmt, []) -> ok;
prk(PP, CFmt, Abbrs) ->
    NumAbbr = length(Abbrs),
    NumPrint = if NumAbbr >= 4 -> 4; true -> NumAbbr end,
    AbbrsPrint = mafia_lib:my_string_substr(Abbrs, 1, NumPrint),
    AbbrsRem = lists:nthtail(NumPrint, Abbrs),
    Fmt = string:join([CFmt || _ <- AbbrsPrint], " ") ++ "\n",
    io:format(PP#pp.dev, Fmt, AbbrsPrint),
    prk(PP, CFmt, AbbrsRem).

prk_html(_PP, []) -> [];
prk_html(PP, Abbrs) ->
    NumAbbr = length(Abbrs),
    NumPrint = if NumAbbr >= ?ReadKeyCols -> ?ReadKeyCols; true -> NumAbbr end,
    AbbrsPrint = mafia_lib:my_string_substr(Abbrs, 1, NumPrint),
    AbbrsRem = lists:nthtail(NumPrint, Abbrs),
    [["<tr>", [["<td", bgcolor(Pl), ">",A, " = ", Pl,"</td>"]
               || {_, Pl, A, _} <- AbbrsPrint],
      "</tr>\r\n"]
     | prk_html(PP, AbbrsRem)].

pr_ivs_user_html(IVs, A) ->
    [["<th", bgcolor(U), ">", A(U), "</th>"] || #iv{u = U} <- IVs].

pr_ivs_user(IVs, A) ->
    string:join([A(U) || #iv{u = U} <- IVs], " ").

pr_ivs_vote_html(IVs, User, MsgId) ->
    [if U == User ->
             ["<td", bgcolor(VF), ">",
              "<b><a href=\"/e/web/msg?id=", ?i2l(MsgId),
              "&player=", User, "&var=vote\">",
              V, "</a></b>"
              "</td>"];
        true ->
             ["<td", bgcolor(VF), ">", V, "</td>"]
     end
     || #iv{u = U, v = V, vlong = VF} <- IVs].

pr_stand_txt(User, {OldVote, NewVote}, Abbrs, PrStand) ->
    Old = if OldVote == "" -> "..."; true -> OldVote end,
    New = if NewVote == "" -> "..."; true -> NewVote end,
    UserA = mafia_name:get3l(User, Abbrs, "***"),
    "|" ++ UserA ++ " " ++ Old ++ ">" ++ New ++ "|" ++
        string:join([?i2l(N) ++ "-" ++ Vote
                     || #iv{n = N, v = Vote} <- PrStand],
                    ", ").

pr_stand_html(User, MsgId, {OldVote, NewVote}, Abbrs, PrStand) ->
    UserA = mafia_name:get3l(User, Abbrs, "***"),
    Voter = ["<td align=center", bgcolor(User),">", UserA, "</td>",
             "<td align=center>", OldVote, ">",
             ["<a href=\"/e/web/msg?id=", ?i2l(MsgId),
              "&player=", User, "&var=vote\">",
              NewVote, "</a>"],
             "</td>"],
    VCnt = fun(N, Vote) -> [?i2l(N), nbsp(" "), Vote] end,
    Stand = [["<td", bgcolor(VLong),">",
              if Vote /= NewVote ->
                      VCnt(N, Vote);
                 Vote == NewVote ->
                      ["<a href=\"/e/web/msg?id=", ?i2l(MsgId),
                       "&player=", User, "&var=vote\">",
                       VCnt(N, Vote), "</a>"]
              end,
              "</td>"]
             || #iv{n = N, v = Vote, vlong = VLong} <- PrStand],
    [Voter|Stand].

bgcolor(V) -> mafia_lib:bgcolor(V).

pr_ivs_vote_txt(IVs, User) ->
    pr_ivs_vote(IVs, User, "").

pr_ivs_vote([], _User, Acc) ->
    Acc;
%% Second last matches
pr_ivs_vote([#iv{u = U, v = V}, #iv{v = V2}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++?l2u(V)++"]" ++ V2 ++ " ");
%% Last matches
pr_ivs_vote([#iv{u = U, v = V}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++?l2u(V)++"]");
%% Match
pr_ivs_vote([#iv{u = U, v = V}, #iv{v = V2} | T], User, Acc) when U == User ->
    pr_ivs_vote(T, User, Acc ++ "["++?l2u(V)++"]"++V2);
pr_ivs_vote([#iv{v = V}], User, Acc) ->
    pr_ivs_vote([], User, Acc ++ " "++ V ++" ");
pr_ivs_vote([#iv{v = V} | T], User, Acc) ->
    pr_ivs_vote(T, User, Acc ++ " " ++ V).

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

print_phase(#phase{don = ?game_ended}) ->
    "Game has Ended";
print_phase(#phase{num = Num, don = DoN}) ->
    pr_don(DoN) ++ " " ++ ?i2l(Num).

pr_don(?day) -> "Day";
pr_don(?night) -> "Night".

%% -----------------------------------------------------------------------------

print_message_full(Fd, M) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_message_full(#pp{message = M, dev = Fd, time_zone = TzH, dst = Dst}).

print_message_full(M = #message{}) ->
    print_message_full(standard_io, M);
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
    print_time(Time, TzH, Dst, Mode).

%% /4
print_time(Time, TzH, Dst, Mode) when is_integer(Time) ->
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
            mafia_time:secs1970_to_local_datetime(Time, TzH, Dst),
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

i2l(Int, Size) -> string:right(?i2l(Int), Size).

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
