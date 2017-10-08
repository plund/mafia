-module(mafia_tracker).

-export([web_vote_tracker/1,
         print_tracker/1
        ]).

-include("mafia.hrl").
-include("mafia_print.hrl").

-import(mafia_print,
        [
         setup_pp/1,
         user_vote_timesort/1,
         print_time_5d/2,
         nbsp/1
        ]).

-import(mafia_lib, [bgcolor/1]).

%% human
web_vote_tracker(DayNum) when is_integer(DayNum) ->
    web_vote_tracker([{?day, DayNum}]);
web_vote_tracker(Opts) ->
    {_, DayNum} = lists:keyfind(?day, 1, Opts),
    GameKey = case lists:keyfind(?game_key, 1, Opts) of
                  {_, GK} -> GK;
                  false -> ?getv(?game_key)
              end,
    Phase = #phase{num = DayNum, ptype = ?day},
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
    Votes0 = (PP#pp.day)#mafia_day.votes,
    Deaths = (PP#pp.day)#mafia_day.player_deaths,
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
                ["<table ", ?BG_TURQUOISE, ">",
                 pr_head_html(IterVotes, PrAbbrF)]
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
                                      pr_ivs_vote_html(PP#pp.game, PrIVs,
                                                       User, V#vote.msg_key),
                                      "<td>", TimeStr, "</td>",
                                      pr_stand_html(User, V#vote.msg_key,
                                                    VoteMove, Abbrs, PrStand),
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
    %% check if someone in Deaths has died or been replaced by V#vote.time
    %% if so reset the vote
    DeathsAtVote =
        lists:foldl(
          fun(#death{time = DTime, player = Pl}, Acc)
                when DTime =< VoteTime -> [Pl|Acc];
             (#replacement{replaced_player = Pl, time = RTime}, Acc)
                when RTime =< VoteTime -> [Pl|Acc];
             (_, Acc) -> Acc
          end,
          [],
          Deaths),
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
    [["<tr>", [["<td", bgcolor(Pl), ">", nbsp(A), nbsp(" = "), nbsp(Pl),"</td>"]
               || {_, Pl, A, _} <- AbbrsPrint],
      "</tr>\r\n"]
     | prk_html(PP, AbbrsRem)].

pr_ivs_user_html(IVs, A) ->
    [["<th", bgcolor(U), ">", A(U), "</th>"] || #iv{u = U} <- IVs].

pr_ivs_user(IVs, A) ->
    string:join([A(U) || #iv{u = U} <- IVs], " ").

pr_ivs_vote_html(G, IVs, User, MsgKey) ->
    [if U == User ->
             ["<td", bgcolor(VF), ">",
              "<b><a href=\"/e/web/msg"
              "?g=", ?i2l(G#mafia_game.game_num),
              "&id=", web:msg_key2str(MsgKey),
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

pr_stand_html(User, MsgKey, {OldVote, NewVote}, Abbrs, PrStand) ->
    UserA = mafia_name:get3l(User, Abbrs, "***"),
    Voter = ["<td align=center", bgcolor(User),">", UserA, "</td>",
             "<td align=center>", OldVote, ">",
             ["<a href=\"/e/web/msg?id=", web:msg_key2str(MsgKey),
              "&player=", User, "&var=vote\">",
              NewVote, "</a>"],
             "</td>"],
    VCnt = fun(N, Vote) -> [?i2l(N), nbsp(" "), Vote] end,
    Stand = [["<td", bgcolor(VLong),">",
              if Vote /= NewVote ->
                      VCnt(N, Vote);
                 Vote == NewVote ->
                      ["<a href=\"/e/web/msg?id=", web:msg_key2str(MsgKey),
                       "&player=", User, "&var=vote\">",
                       VCnt(N, Vote), "</a>"]
              end,
              "</td>"]
             || #iv{n = N, v = Vote, vlong = VLong} <- PrStand],
    %% 1 elem in Stand get very wide, but adding empty cells did not fix it
    %% Stand2 = Stand ++ ["<td></td>" || _ <- lists:seq(1, 4 - length(Stand))],
    [Voter|Stand].

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
