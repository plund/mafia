-module(mafia_print).

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

-import(mafia,
        [
         getv/1,
         l2u/1,
         lrev/1,
         rgame/0,
         rgame/1,
         rday/2
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

pp() ->
    Page = getv(?page_to_read),
    pp(Page).

pp({ThId, Page}) ->
    pp(ThId, Page);
pp(Page) ->
    ThId = getv(?thread_id),
    pp(ThId, Page).

pp(ThId, Page) ->
    %% Select MsgIds here
    MsgIds = msgids(ThId, Page),
    print_page(ThId, MsgIds, fun print_message_full/1).

pps() ->
    ThId = getv(?thread_id),
    pps(ThId).

pps({ThId, Page}) ->
    pps(ThId, Page);
pps(ThId) when is_integer(ThId) ->
    LastPage = lists:max(mafia:find_pages_for_thread(ThId)),
    pps(ThId, LastPage).

pps(ThId, Page) ->
    %% Select MsgIds here
    MsgIds = msgids(ThId, Page),

    io:format("~-10s "
              "~-3s"
              " ~-11s "
              "~-7s "
              "~s\n",
              ["Player", "pg", "Date/Time", "Msg Id", "Message Text"]),
    print_page(ThId, MsgIds, fun print_message_summary/1).

pm(MsgId) ->
    pm(standard_io, MsgId).

pm(Fd, MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [Msg] -> print_message_full(Fd, Msg);
        [] -> io:format(Fd, "Message ID ~p not found\n", [MsgId])
    end.

%% -----------------------------------------------------------------------------
%% print params
-record(pp, {game  :: #mafia_game{},
             day   :: #mafia_day{},
             game_key :: thread_id(),
             phase  :: phase(),
             day_num :: integer(),
             match_expr :: ?undefined | term(),
             dev = standard_io,
             mode = ?text :: ?text | ?html,
             period :: integer(),   %% Poll period
             time2dl :: false | seconds1970() %% time to next DL (status page)
            }).

po(P, [{?game_key, K} | T]) -> po(P#pp{game_key = K}, T);
po(P, [{?phase, Ph} | T]) -> po(P#pp{phase = Ph}, T);
po(P, [{?dev, Fd} | T]) -> po(P#pp{dev = Fd}, T);
po(P, [{?mode, M} | T]) -> po(P#pp{mode = M}, T);
po(P, [{?period, M} | T]) -> po(P#pp{period = M}, T);
po(P, [{?time, T} | T]) -> po(P#pp{time2dl = T}, T);
po(P, []) -> P.

don_arg(DoN) ->
    if DoN == d; DoN == day; DoN == ?day -> ?day;
       DoN == n; DoN == night; DoN == ?night -> ?night
       %% DoN == e; DoN == 'end'; DoN == ?game_end -> ?game_ended
    end.

%% /0 human
print_votes() ->
    GameKey = getv(?game_key),
    Phase = mafia_time:calculate_phase(GameKey),
    print_votes([{?phase, Phase}]).

%% /2 human
print_votes(DayNum, DoN) ->
    DoN2 = don_arg(DoN),
    print_votes([{?phase, {DayNum, DoN2}}]).

%% /1 generic
print_votes(Opts) ->
    DefOpts = [{?game_key, getv(?game_key)},
               {?mode, ?text},
               {?dev, standard_io}],
    PP = po(#pp{}, DefOpts),
    PP2 = po(PP, Opts),
    print_votesI(PP2).

print_votes_game(_PP, []) -> ok;
print_votes_game(PP, [G]) -> print_votesI(PP#pp{game = G}).

print_votes_day(_PP, []) -> ok;
print_votes_day(PP, [D]) -> print_votesI(PP#pp{day = D}).

print_votesI(PP) when PP#pp.game == ?undefined ->
    print_votes_game(PP, rgame(PP#pp.game_key));
print_votesI(PP) when PP#pp.day == ?undefined ->
    print_votes_day(PP, rday(PP#pp.game_key, PP#pp.phase));
print_votesI(#pp{game = G,
                 day = Day
                } = PP) ->
    PhaseType = case PP#pp.phase of
                    ?game_ended -> ?game_ended;
                    _ -> element(2, PP#pp.phase)
                end,
    Day = PP#pp.day,
    RemPlayers = Day#mafia_day.players_rem,
    %% Part - Page heading
    %% Print Game Name
    GName = ?b2l(G#mafia_game.name),
    HTitle =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\n"
                          "~s\n"
                          "~s\n"
                          "Previous days found at http://mafia.peterlund.se/\n",
                          [GName, [$= || _ <- GName]]);
           PP#pp.mode == ?html ->
                ["<tr><th>", GName, "</th></tr>\r\n",
                 "<tr><td align=center>",
                 "Previous days found at "
                 "<a href=\"http://mafia.peterlund.se/\">"
                 "http://mafia.peterlund.se/</a>",
                 "</td></tr>\r\n"]
        end,

    %% Part - Time Left to Deadline or game end message
    HDeadLine =
        if PP#pp.mode == ?text ->
                if PhaseType /= ?game_ended andalso is_integer(PP#pp.time2dl) ->
                        print_time_left_to_dl(PP);
                   PhaseType == ?game_ended ->
                        {EndTime, EndMsgId} = G#mafia_game.game_end,
                        {TzH, Dst} = mafia_time:get_tz_dst(G, EndTime),
                        io:format(PP#pp.dev,
                                  "\n"
                                  "The GAME HAS ENDED ~s\n",
                                  [print_time(EndTime, TzH, Dst, extensive)]),
                        io:format(PP#pp.dev,
                                  "\n"
                                  "Game Master End Message\n"
                                  "-----------------------\n",
                                  []),
                        pm(PP#pp.dev, EndMsgId);
                   true -> ok
                end;
           PP#pp.mode == ?html ->
                if PhaseType /= ?game_ended andalso is_integer(PP#pp.time2dl) ->
                        print_time_left_to_dl(PP);
                   PhaseType == ?game_ended ->
                        {EndTime, _EndMsgId} = G#mafia_game.game_end,
                        {TzH, Dst} = mafia_time:get_tz_dst(G, EndTime),
                        ["<tr><td>",
                         "The GAME HAS ENDED ",
                         print_time(EndTime, TzH, Dst, extensive),
                         "</td></tr>"];
                   true ->
                        []
                end
        end,

    %% votes, from remaining players only
    {HVoteCount, VoteSumSort, InvalidVotes} =
        if PhaseType == ?day ->
                %% Part - Votes
                pr_votes(PP);
           true ->
                {[], na, na}
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

    Votes = rem_play_votes(PP),
    NonVotes =
        if PhaseType == ?day ->
                %% Part - Non-votes
                ValidVoters = [ Pl || {Pl, _} <- Votes]
                    -- [User || {User, _} <- InvalidVotes],
                Unvoted = RemPlayers -- ValidVoters,
                NoVoteTitle = if Unvoted == [] -> "Non-votes: -";
                                 true -> "Non-votes: "
                              end,
                if PP#pp.mode == ?text ->
                        io:format(
                          PP#pp.dev,
                          "\n~s~s\n",
                          [NoVoteTitle,
                           string:join([?b2l(U) || U <- Unvoted], ", ")]);
                   PP#pp.mode == ?html ->
                        ["<tr><td><table align=center><tr><th>", NoVoteTitle,
                         "</th>",
                         [["<td", bgcolor(U), ">", ?b2l(U), "</td>"]
                          || U <- Unvoted],
                         "</table></td></tr>"]
                end;
           true -> []
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
                        %%print_time(VTime, short),
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
    [HStats, HNonPosters] = print_statsI(PP),

    %% Part - Dead players
    DeathsToReport =
        ?lrev(
          [D || D = #death{phase = Ph} <- G#mafia_game.player_deaths,
                if PhaseType == ?game_ended -> true;
                   PP#pp.time2dl == false -> Ph == PP#pp.phase;
                   true -> Ph =< PP#pp.phase
                end]),
    PrFun = fun(IsEnd, Ph) ->
                    pr_eodon(IsEnd, Ph)
            end,
    if PP#pp.mode == ?text ->
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
                         Div)]),
            if is_integer(PP#pp.period) ->
                    io:format(
                      PP#pp.dev,
                      "\n"
                      "Updates currently every ~p minutes "
                      "(more often near deadlines).\n"
                      "Mafia game thread at: ~s\n",
                      [PP#pp.period, ?UrlBeg ++ ?i2l(PP#pp.game_key)]);
               true -> ok
            end;
       PP#pp.mode == ?html ->
            HtmlDeaths =
                ["<table align=center>",
                 "<tr><th colspan=2 align=left><br>Dead Players</th></tr>",
                 [["<tr><td><table align=left><tr><td", bgcolor(DeadPl), ">",
                   ?b2l(DeadPl), "</td><td>", PrFun(IsEnd, Ph),
                   if Com == ?undefined ->
                           " - msg: " ++ ?i2l(MsgId);
                      is_binary(Com) ->
                           " - " ++ ?b2l(Com)
                   end,
                   "</td></tr></table></td></tr>"]
                  || #death{player = DeadPl,
                           is_end = IsEnd,
                           phase = Ph,
                           msg_id = MsgId,
                           comment = Com}
                         <- DeathsToReport],
                 "</table>"],
            [HTitle, HDeadLine, HVoteCount, EndVotes, NonVotes, "\r\n",
             ["<tr><td>\r\n", HTrackKey, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HVoteTrack, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HStats, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HNonPosters, "</td><tr>\r\n"],
             ["<tr><td>\r\n", HtmlDeaths, "</td><tr>\r\n"]
            ]
    end.

print_time_left_to_dl(PP) ->
    {{Days, {HH, MM, _}}, {Num, DoN, _}} =
        mafia_time:get_next_deadline(PP#pp.game_key, PP#pp.time2dl),
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
            ["<tr><td>",
             "Remaining time to next ",
             pr_don(DoN), " ", ?i2l(Num), " deadline:"
             "  ", DayStr, ?i2l(HH), "hours, ", ?i2l(MM), " minutes\n",
             "</td></tr>"]
    end.

%% Votes per user are time ordered (oldest first)
%% Users sorted time ordered after they oldest vote (first vote)
-spec pr_votes(PP :: #pp{}) -> term().
pr_votes(PP) ->
    Votes = rem_play_votes(PP),
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

    %% Sort summary first on number of received votes, if equal
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
                 "<tr><td><table border=0 align=center bgcolor=\"#dfffdf\">",
                 "<tr><th>Wagon</th><th>#</th><th>Voters</th></tr>",
                 [["<tr><td", bgcolor(Vote), " align=center>", ?b2l(Vote),
                   "</td><td>(", ?i2l(N), ")</td>",
                   "<td><table><tr>",
                   [["<td", bgcolor(Voter), ">", ?b2l(Voter), "</td>"]
                    || {_Time, Voter, _Raw3} <- VoteInfos],
                   "</tr></table></td>",
                   "</tr>\r\n"] || {Vote, N, VoteInfos} <- VoteSumSort],
                 "</table></td></tr>"
                ]
        end,
    {Html, VoteSumSort, InvalidVotes}.

rem_play_votes(PP) ->
    Day = PP#pp.day,
    Votes0 = Day#mafia_day.votes,
    RemPlayers = Day#mafia_day.players_rem,
    [V || V <- Votes0,
          lists:member(element(1, V), RemPlayers)].

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


pr_eodon(true, Phase) -> " died " ++ pr_eodon(Phase);
pr_eodon(false, {Num, DoN}) -> " died " ++ pr_don(DoN) ++ " " ++ ?i2l(Num).

pr_eodon({Num, ?day}) -> "EoD"++ ?i2l(Num);
pr_eodon({Num, ?night}) -> "EoN"++ ?i2l(Num);
pr_eodon(?game_ended) -> "at end of game".

pr_phase_long({Num, DoN}) -> pr_don(DoN) ++ " " ++ ?i2l(Num);
pr_phase_long(?game_ended) -> "Game End";
pr_phase_long(?total_stats) -> "Game Global Statistics".

%% Manual API
print_stats() -> print_stats_opts([]).

print_stats(e) -> print_stats_opts([{?phase, ?game_ended}]);
print_stats(Opts) when is_list(Opts) -> print_stats_opts(Opts).

print_stats(Num, DoN) ->
    DoN2 = don_arg(DoN),
    print_stats_opts([{?phase, {Num, DoN2}}]).

print_stats_opts(Opts) ->
    DefOpts = [{?game_key, 1420289},
               {?phase, {1, ?day}},
               {?dev, standard_io}],
    PP = po(#pp{}, DefOpts),
    PP2 = po(PP, Opts),
    print_statsI(PP2).

%% API
print_statsI(PP) when PP#pp.game == ?undefined ->
    print_stats_game(PP, rgame(PP#pp.game_key));
print_statsI(PP) when PP#pp.match_expr == ?undefined ->
    Phase = if PP#pp.mode == ?text, PP#pp.phase == ?game_ended ->
                    ?total_stats;
               true -> PP#pp.phase
            end,
    print_stats_match(PP, PP#pp.game_key, Phase);
print_statsI(PP) ->
    do_print_stats(PP).

%% stats support funs
print_stats_game(_PP, []) -> ok;
print_stats_game(PP, [G]) -> print_statsI(PP#pp{game = G}).

print_stats_match(PP, GameKey, ?total_stats) ->
    %% TOTAL stats
    MatchHead = #stat{key = {'$1', '$2'}, _='_'},
    Guard = [{'==', '$2', GameKey}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP, GameKey, ?game_ended) ->
    %% END stats
    MatchHead = #stat{key = {'$1', '$2', '$3'}, _='_'},
    Guard = [{'==', '$2', GameKey},
             {'==', '$3', ?game_ended}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr});
print_stats_match(PP, GameKey, {Day, DoN}) ->
    %% PHASE stats
    MatchHead = #stat{key = {'$1', '$2', {'$3', '$4'}}, _='_'},
    Guard = [{'==', '$2', GameKey},
             {'==', '$3', Day}, {'==', '$4', DoN}],
    Result = '$_',
    MatchExpr = [{MatchHead, Guard, [Result]}],
    print_statsI(PP#pp{match_expr = MatchExpr}).

do_print_stats(PP) ->
    #pp{game = G, phase = Phase, match_expr = MatchExpr} = PP,
    Stats = mnesia:dirty_select(stat, MatchExpr),
    LE = fun(#stat{num_postings = PA, num_words = WA},
             #stat{num_postings = PB, num_words = WB}) ->
                 if PA < PB -> true;
                    PA > PB -> false;
                    WA =< WB -> true;
                    true -> false
                 end
         end,
    StatsSorted = lists:sort(LE, Stats),
    PrFn = fun(tr, S) -> transl(element(1, S#stat.key));
              (cell, _) -> "td";
              (bgcolor, S) -> bgcolor(transl(element(1, S#stat.key)));
              (_, _) -> []
           end,
    NonPosters = [?b2l(PRem) || PRem <- G#mafia_game.players_rem]
        -- [PrFn(tr, S) || S <- Stats],
    HtmlHead =
        if PP#pp.mode == ?text ->
                io:format(PP#pp.dev,
                          "\n"
                          "Posting statistics (~s)\n"
                          "------------------\n"
                          "~s ~s ~s ~s\n",
                          [pr_phase_long(Phase),
                           "Posts", " Words", "  Chars", "Player"]),
                [];
           PP#pp.mode == ?html ->
                ["<tr><th colspan=\"4\">",
                 "<br>", "Posting statistics (",
                 pr_phase_long(Phase),
                 ")\n",
                 "</th></tr>",
                 "<tr><th align=\"right\">Posts</th>"
                 "<th align=\"right\">Words</th>"
                 "<th align=\"right\">Chars</th>"
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
          {#stat{msg_ids = [],
                num_chars = 0,
                num_words = 0,
                num_postings = 0
               },
           HtmlHead},
          ?lrev(StatsSorted)),
    print_stat_div(PP),
    Html2 = Html1 ++
        print_stat_row(PP, SumStat, fun(cell, _) -> "th";
                                       (tr, _) -> "Total Counts";
                                       (_, _) -> []
                                    end),
    HtmlStats = ["<table align=center>", Html2, "</table>"],
    NonPostTitle =
        case NonPosters of
            [] -> "Non-posters: -";
            _ -> "Non-posters: "
        end,
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev,
                      "\n~s~s\n",
                      [NonPostTitle, string:join(NonPosters, ", ")]),
            [[], []];
       PP#pp.mode == ?html ->
            HtmlNonPosters =
                ["<table align=center><tr>",
                 "<th colspan=\"4\">", NonPostTitle, "</th>",
                 [["<td", bgcolor(NP), ">", NP, "</td>"] || NP <- NonPosters],
                 "</tr></table>"],
            [HtmlStats, HtmlNonPosters]
    end.

print_stat_div(PP) when PP#pp.mode == ?text ->
    io:format(PP#pp.dev,
              "~s ~s ~s ~s\n",
              ["-----", "------", "-------", "-----------"]);
print_stat_div(_PP) ->
    [].

print_stat_row(PP, S, PrFn) when PP#pp.mode == ?text ->
    io:format(PP#pp.dev,
              "~s ~s ~s ~s\n",
              [i2l(S#stat.num_postings, 5),
               i2l(S#stat.num_words, 6),
               i2l(S#stat.num_chars, 7),
               PrFn(tr, S)
              ]),
    [];
print_stat_row(PP, S, PrFn) when PP#pp.mode == ?html ->
    CBegR = ["<", PrFn(cell, S), " align=\"right\">"],
    CBegL = ["<", PrFn(cell, S), " align=\"left\">"],
    CEnd = ["</", PrFn(cell, S), ">"],
    ["<tr", PrFn(bgcolor, S), ">",
     CBegR, ?i2l(S#stat.num_postings),
     CEnd, CBegR, ?i2l(S#stat.num_words),
     CEnd, CBegR, ?i2l(S#stat.num_chars),
     CEnd, CBegL, PrFn(tr, S),
     CEnd, "</tr>\r\n"].

%% Get user name as stored normal case string
transl(UserUB) ->
    UInfo = hd(mnesia:dirty_read(user, UserUB)),
    ?b2l(UInfo#user.name).

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
    GameKey = getv(?game_key),
    Phase = {DayNum, ?day},
    PP = #pp{game_key = GameKey,
             day_num = DayNum,
             phase = Phase,
             mode = ?html},
    web_vote_tracker(PP, rgame(GameKey), rday(GameKey, Phase)).

web_vote_tracker(_PP, [], _) -> ok;
web_vote_tracker(_PP, _, []) -> ok;
web_vote_tracker(PP, [Game], [Day]) ->
    PP2 = PP#pp{game = Game,
                day = Day},
    print_tracker(PP2).

print_tracker(PP) when PP#pp.day_num == ?undefined ->
    print_tracker(PP#pp{day_num = element(1, PP#pp.phase)});
print_tracker(PP) ->
    #mafia_day{players_rem = PlayersRem,
               player_deaths = Deaths} = PP#pp.day,
    %% player_deaths contains players dying in the middle of the day.
    AllPlayersB = PlayersRem ++ [DeadB || #death{player = DeadB} <- Deaths],
    Abbrs = mafia_name:get_abbrevs(AllPlayersB),
    if PP#pp.mode == ?text ->
            io:format(PP#pp.dev, "\n", []);
       true -> ok
    end,
    RKhtml = print_read_key(PP, Abbrs),
    VThtml = print_tracker_tab(PP, Abbrs, AllPlayersB),
    [RKhtml, VThtml].

print_tracker_tab(PP, Abbrs, AllPlayersB) ->
    #mafia_day{votes = Votes0} = PP#pp.day,
    Votes = [V || V <- Votes0,
                  lists:member(element(1, V), AllPlayersB)],
    Votes3 = user_vote_timesort(Votes),
    PrAbbrF = fun("---") -> "---";
                 ("INV") -> "INV";
                 (V) -> case lists:keyfind(V, 2, Abbrs) of
                            false ->
                                %%io:format(PP#pp.dev, "~s\n", [V]),
                                "***";
                            {_, _, Abbr, _} -> Abbr
                       end
              end,
    Users = [?b2l(UserB) || UserB <- AllPlayersB],
    IterVotes = [{User, "---", ""} || User <- Users],
    FmtVoter = "Voter ~s\n",
    FmtTime = "Time  ~s\n",
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
                ["<table><tr>"
                 "<th align=\"right\">Voter</th>"
                 "<th>Time</th>",
                 pr_ivs_user_html(IterVotes, PrAbbrF),
                 "<th>Time</th>"
                 "<th align=\"left\">Voter</th>"
                 "</tr>\r\n"]
        end,
    {_, Html} =
        lists:foldl(
          fun({User, V = #vote{}}, {IVs, Html}) ->
                  {NewIVs, PrIVs} =
                      if V#vote.valid ->
                              VFull = ?b2l(V#vote.vote),
                              NewVote = PrAbbrF(VFull),
                              IVs2 =
                                  lists:keyreplace(User, 1, IVs,
                                                   {User, NewVote, VFull}),
                              {IVs2, IVs2};
                         not V#vote.valid ->
                              VFull =  "INVALID",
                              NewVote =  "INV",
                              IVs2 =
                                  lists:keyreplace(User, 1, IVs,
                                                   {User, NewVote, VFull}),
                              {IVs, IVs2}
                      end,
                  TimeStr = print_time_5d(PP#pp.game, V#vote.time),
                  if PP#pp.mode == ?text ->
                          io:format(PP#pp.dev,
                                    "~s~s~s\n",
                                    [TimeStr,
                                     pr_ivs_vote(PrIVs, User),
                                     TimeStr
                                    ]),
                          {NewIVs, []};
                     PP#pp.mode == ?html ->
                          {NewIVs,
                           [Html|
                            ["<tr>",
                             "<td", bgcolor(User), " align=\"right\">",
                             User, "</td>",
                             "<td>", TimeStr, "</td>",
                             pr_ivs_vote_html(PrIVs, User, V#vote.id),
                             "<td>", TimeStr, "</td>",
                             "<td", bgcolor(User), " align=\"left\">",
                             User, "</td>",
                             "</tr>\r\n"]]}
                  end
          end,
          {IterVotes, []},
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
                    ["<tr>"
                     "<th align=\"right\">Voter</th>",
                     "<th>Time</th>",
                     pr_ivs_user_html(IterVotes, PrAbbrF),
                     "<th>Time</th>",
                     "<th align=\"left\">Voter</th>",
                     "</tr></table>\r\n"]],
            ["<table>",
             "<tr><th>Vote Tracker (Day ", ?i2l(PP#pp.day_num), ")</th></tr>",
             "<tr><td>", Tab2, "</td></tr>",
             "</table>"]
    end.

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
    AbbrsPrint = string:substr(Abbrs, 1, NumPrint),
    AbbrsRem = lists:nthtail(NumPrint, Abbrs),
    Fmt = string:join([CFmt || _ <- AbbrsPrint], " ") ++ "\n",
    io:format(PP#pp.dev, Fmt, AbbrsPrint),
    prk(PP, CFmt, AbbrsRem).

prk_html(_PP, []) -> [];
prk_html(PP, Abbrs) ->
    NumAbbr = length(Abbrs),
    NumPrint = if NumAbbr >= ?ReadKeyCols -> ?ReadKeyCols; true -> NumAbbr end,
    AbbrsPrint = string:substr(Abbrs, 1, NumPrint),
    AbbrsRem = lists:nthtail(NumPrint, Abbrs),
    [["<tr>", [["<td", bgcolor(Pl), ">",A, " = ", Pl,"</td>"]
               || {_, Pl, A, _} <- AbbrsPrint],
      "</tr>\r\n"]
     | prk_html(PP, AbbrsRem)].

pr_ivs_user_html(IVs, A) ->
    [["<th", bgcolor(U), ">", A(U), "</th>"] || {U, _V, _} <- IVs].

pr_ivs_user(IVs, A) ->
    string:join([A(U) || {U, _V, _} <- IVs], " ").

pr_ivs_vote_html(IVs, User, MsgId) ->
    [if U == User ->
             ["<td", bgcolor(VF), ">",
              "<b><a href=\"?msg_id=", ?i2l(MsgId), "\">",
              V, "</a></b>"
              "</td>"];
        true ->
             ["<td", bgcolor(VF), ">", V, "</td>"]
     end
     || {U, V, VF} <- IVs].

bgcolor(Str) when is_list(Str) ->
    bgcolor(?l2b(Str));
bgcolor(Bin) when is_binary(Bin) ->
    Hash = erlang:phash2(Bin, 16#1000000),
    Color = Hash bor 16#C0C0C0,
    [" bgcolor=\"#", integer_to_list(Color, 16), "\""].

pr_ivs_vote(IVs, User) ->
    pr_ivs_vote(IVs, User, "").

pr_ivs_vote([], _User, Acc) ->
    Acc;
%% Second last matches
pr_ivs_vote([{U, V, _}, {_, V2, _}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++?l2u(V)++"]" ++ V2 ++ " ");
%% Last matches
pr_ivs_vote([{U, V, _}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++?l2u(V)++"]");
%% Match
pr_ivs_vote([{U, V, _}, {_, V2, _} | T], User, Acc) when U == User ->
    pr_ivs_vote(T, User, Acc ++ "["++?l2u(V)++"]"++V2);
pr_ivs_vote([{_, V, _ }], User, Acc) ->
    pr_ivs_vote([], User, Acc ++ " "++ V ++" ");
pr_ivs_vote([{_, V, _} | T], User, Acc) ->
    pr_ivs_vote(T, User, Acc ++ " " ++ V).

%% -----------------------------------------------------------------------------

print_messages(User) when is_list(User) ->
    print_messages(?l2b(User));
print_messages(User) when is_binary(User) ->
    ThId = getv(?thread_id),
    Pages = lists:sort(mafia:find_pages_for_thread(ThId)),
    AllMsgIds =
        lists:foldl(
          fun(Page, Acc) ->
                  case mnesia:dirty_read(page_rec, {ThId, Page}) of
                      [#page_rec{message_ids = PMids}] -> Acc ++ PMids;
                      [] -> Acc
                  end
          end,
          [],
          Pages),
    UserMsgIds =
        lists:filter(
          fun(MsgId) ->
                  case mnesia:dirty_read(message, MsgId) of
                      [#message{user_name = U}]
                        when U == User -> true;
                      _ -> false
                  end
          end,
          AllMsgIds),
    print_page(ThId, UserMsgIds, fun print_message_summary/1).

%% -----------------------------------------------------------------------------

print_pages_for_thread() ->
    ThId = getv(?thread_id),
    print_pages_for_thread(ThId).

print_pages_for_thread(ThId) ->
    Pages = mafia:find_pages_for_thread(ThId),
    io:format("Thread ~p has stored Pages ~w\n", [ThId, Pages]).

%% -----------------------------------------------------------------------------

-define(TMsg(M), M#message.time).
-define(TDl(D), element(3, D)).
-define(MINUTE, 60).

print_page(_ThId, [], _PrintFun) -> ok;
print_page(ThId, MsgIds, PrintFun) ->
    %% print starting line with current phase
    %% does this thread have a game?
    MsgsPage = r_msgs(MsgIds),
    case rgame(ThId) of
        [] ->
            [PrintFun(M) || M <- MsgsPage];
        [#mafia_game{deadlines = DLs} = G] ->
            TimeB = ?TMsg((hd(MsgsPage))),
            TimeA = ?TMsg((lists:last(MsgsPage))),

            %% Are more deadlines needed
            TimeLDl = ?TDl(hd(DLs)),
            DLs2 =
                if TimeLDl < TimeA ->
                        ?lrev(
                          mafia_time:update_deadlines(ThId));
                      true ->
                           DLs
                   end,

            DLsIn = [D || D <- DLs2,
                          TimeB - 3*?MINUTE < ?TDl(D),
                          ?TDl(D) < TimeA + 3*?MINUTE],
            MixedSort = lists:sort(fun cmp_time/2, MsgsPage ++ DLsIn),
            case hd(MixedSort) of
                Msg = #message{} ->
                    FirstTime = time(Msg),
                    Phase = mafia_time:calculate_phase(G, FirstTime),
                    print_dl(Phase, "");
                _ -> ok
            end,
            [case E of
                 M = #message{} ->
                     PrintFun(M);
                 D ->
                     print_dl(D, "End of ")
             end || E <- MixedSort]
    end,
    ok.

msgids(ThId, PageNum) ->
    case mnesia:dirty_read(page_rec, {ThId, PageNum}) of
        [] -> [];
        [#page_rec{message_ids = MIds}] -> MIds
    end.

r_msgs(MsgIds) ->
    [hd(mnesia:dirty_read(message, MsgId)) || MsgId <- MsgIds].

cmp_time(A, B) -> time(A) =< time(B).

time({_,_,Time}) -> Time;
time(#message{time = Time}) -> Time.

print_dl({Num, DorN, _}, Txt) ->
    print_dl({Num, DorN}, Txt);
print_dl({Num, DorN}, Txt) ->
    io:format("-------------------------------- ~s~s ~p "
              "--------------------------------\n",
              [Txt, pr_don(DorN), Num]).

pr_don(?day) -> "Day";
pr_don(?night) -> "Night".

%% -----------------------------------------------------------------------------

print_message_full(M) ->
    print_message_full(standard_io, M).

print_message_full(Fd, M) ->
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
               print_time(M#message.time),
               ?i2l(M#message.thread_id),
               ?i2l(M#message.msg_id),
               html2txt(?b2l(M#message.message))
              ]).

%% -----------------------------------------------------------------------------

print_message_summary(M) ->
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
                         print_time(M#message.time, short),
                         ?i2l(M#message.msg_id),
                         MsgShort
                        ]),
    io:format("~s", [Str]).

%% -----------------------------------------------------------------------------

print_time(Time) ->
    print_time(Time, long).

print_time(current_time, Mode) ->
    Time = mafia_time:utc_secs1970(),
    print_time(Time, Mode);
print_time(Time, Mode) when is_integer(Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    lists:flatten(print_time(Time, TzH, Dst, Mode)).

%% half this fun should go to mafia_time
print_time(Time, TzH, Dst, Mode) when is_integer(Time) ->
    try
        {{Y, M, D}, {HH,MM,SS}} =
            mafia_time:secs1970_to_local_datetime(Time, TzH, Dst),
        DstStr = case {Dst, Mode} of
                     {false, extensive} -> ", Normal Time";
                     {true, extensive} -> ", Daylight Saving Time";
                     {false, Mode} -> "";
                     {true, Mode} -> " DST"
                 end,
        Char = case TzH of
                   0 -> "Z";
                   _ -> "T"
               end,
        case Mode of
            short ->
                io_lib:format("~s-~s~s~s:~s",
                              [p(M), p(D), Char, p(HH), p(MM)]);
            long ->
                io_lib:format("~s-~s-~s~s~s:~s:~s (~s~s)",
                              [p(Y), p(M), p(D), Char, p(HH), p(MM), p(SS),
                               ?i2l(TzH), DstStr]);
            extensive ->
                io_lib:format("~s-~s-~s ~s:~s:~s (TZ: ~s~s)",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS),
                               ?i2l(TzH), DstStr])
        end
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
