-module(mafia_print).

-export([
         print_stats/0, print_stats/3,
         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2,
         print_votes/3,

         print_tracker/0,

         print_messages/1,
         print_message_summary/1,
         print_message_full/1,

         print_pages_for_thread/0,
         print_pages_for_thread/1
        ]).

-import(mafia,
        [
         getv/1,
         b2l/1,
         l2b/1,
         i2l/1,
         l2u/1,
         lrev/1,
         rgame/0,
         rgame/1,
         rday/2
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

pp() ->
    Page = getv(page_to_read),
    pp(Page).

pp({ThId, Page}) ->
    pp(ThId, Page);
pp(Page) ->
    ThId = getv(thread_id),
    pp(ThId, Page).

pp(ThId, Page) ->
    %% Select MsgIds here
    MsgIds = msgids(ThId, Page),
    print_page(ThId, MsgIds, fun print_message_full/1).

pps() ->
    ThId = getv(thread_id),
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

pm(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [Msg] -> print_message_full(Msg);
        [] -> io:format("Message ID ~p not found\n", [MsgId])
    end.

%% -----------------------------------------------------------------------------

%% /0 human
print_votes() ->
    print_votes([{fd, standard_io}]).

%% /1 mafia_web
print_votes(Opts) ->
    %% Calculate time for last game message
    %% LastMsgTime = time_for_last_msg(),
    Time = mafia_time:utc_secs1970(),
    Opts2 = [{time, Time} | Opts],
    ThId = getv(thread_id),
    Phase = mafia_time:calculate_phase(ThId, Time),
    print_votesI3(ThId, Phase, Opts2).

%% /2 human
print_votes(DayNum, DoN) ->
    print_votes(DayNum, DoN, [{fd, standard_io}]).

%% /3 mafia_web
print_votes(DayNum, DoN, Opts) ->
    DoN2 = if DoN == d; DoN == day; DoN == ?day -> ?day;
              DoN == n; DoN == night; DoN == ?night -> ?night
           end,
    print_votesI2({DayNum, DoN2}, Opts).

print_votesI2(DayNum, Opts) when is_integer(DayNum) ->
    print_votesI2({DayNum, ?day}, Opts);
print_votesI2(Phase = {_, _}, Opts) ->
    ThId = getv(thread_id),
    Opts2 = [{time, false} | Opts],
    print_votesI3(ThId, Phase, Opts2).

print_votesI3(ThId, Phase, Opts) ->
    print_votesI4(Phase,
                 rgame(ThId),
                 rday(ThId, Phase),
                 Opts).

%% print params
-record(pp, {game,
             day,
             phase,
             dev,
             next :: integer(),
             time :: false | seconds1970()
            }).

print_votesI4(_Phase, [], _, _Opts) -> ok;
print_votesI4(_Phase, _, [], _Opts) -> ok;
print_votesI4(Phase,
             [#mafia_game{} = G],
             [#mafia_day{} = Day],
             Opts
            ) ->
    P = #pp{game = G,
            day = Day,
            phase = Phase
           },
    print_votesI(po(P, Opts)).

po(P, [{time, Time} | T]) -> po(P#pp{time = Time}, T);
po(P, [{fd,   Fd  } | T]) -> po(P#pp{dev = Fd}, T);
po(P, [{next, Mins} | T]) -> po(P#pp{next = Mins}, T);
po(P, []) -> P.

print_votesI(#pp{game = G,
                 day = Day,
                 phase = Phase,
                 time = LastMsgTime
                } = P) ->
    ThId = G#mafia_game.key,
    #mafia_day{votes = Votes0,
               players_rem = RemPlayers} = Day,

    IsDay = element(2, Phase) == ?day,

    %% Part - Page heading
    %% Print Game Name
    GName = b2l(G#mafia_game.name),
    io:format(P#pp.dev,
              "\n"
              "~s\n"
              "~s\n"
              "Previous days found at http://mafia.peterlund.se/\n",
              [GName, [$= || _ <- GName]]),

    if is_integer(LastMsgTime) ->
            print_time_left_to_dl(P, ThId, LastMsgTime);
       true -> no_print
    end,

    %% votes, from remaining players only
    Votes = [V || V <- Votes0,
                  lists:member(element(1, V), RemPlayers)],
    UserVotesTS = user_vote_timesort(Votes),
    InvUserVotesTS = [UV || UV = {_, #vote{valid = false}} <- UserVotesTS],
    {VoteSumSort, InvalidVotes} =
        if IsDay ->
                %% Part - Votes
                pr_votes(P, Votes, Phase);
           true ->
                {na, na}
        end,
    if IsDay ->
            %% Part - End votes
            EndVoters = Day#mafia_day.end_votes,
            EndVoteStr =
                if EndVoters == [] -> "-";
                   true ->
                        string:join([b2l(Ev) || Ev <- EndVoters], ", ")
                end,
            io:format(P#pp.dev, "\nEnd votes: ~s\n", [EndVoteStr]),

            %% Part - Non-votes
            ValidVoters = [ Pl || {Pl, _} <- Votes]
                -- [User || {User, _} <- InvalidVotes],
            Unvoted = RemPlayers -- ValidVoters,
            NoVoteStr =
                if Unvoted == [] -> "-";
                   true ->
                        string:join([b2l(U) || U <- Unvoted], ", ")
                end,
            io:format(P#pp.dev, "\nNon-votes: ~s\n", [NoVoteStr]);
       true -> ignore
    end,

    if IsDay ->
            %% Part - Voting texts
            ValidVotesS =
                lists:sort(
                  lists:foldl(fun({_, _, VoteInfos}, Acc) ->
                                      Acc ++ VoteInfos
                              end,
                              [],
                              VoteSumSort)),
            io:format(P#pp.dev,
                      "\nVoting texts:\n"
                      "-------------\n",
                      []),
            [io:format(P#pp.dev,
                       "~s ~s : \"~s\"\n",
                       [print_time_5d(G, VTime),
                        b2l(Voter),
                        rm_nl(b2l(Raw))])
             || {VTime, Voter, Raw} <- ValidVotesS],

            %% Part - Invalid Vote text
            io:format(P#pp.dev,
                      "\n"
                      "Invalid Vote texts:\n"
                      "-------------------\n",
                      []),
            [io:format(P#pp.dev,
                       "~s ~s: \"~s\"\n",
                       [print_time_5d(G, VTime),
                        Voter,
                        rm_nl(b2l(Raw))])
             || {Voter, #vote{raw = Raw, time = VTime}} <- InvUserVotesTS],
            io:format(P#pp.dev,
                      "\n"
                      "\"INVALID\" means that the this program did not "
                      "recognise the vote. But the GM may.)\n",
                      []);
       true -> ignore
    end,

    %% Part - Vote tracker
    print_tracker(P, ThId, Phase),

    %% Part - Posting stats
    print_stats(P, ThId, Phase),

    %% Part - Dead players
    DeathsToReport =
        lrev(
          [D || D = #death{phase = Ph} <- G#mafia_game.player_deaths,
                if LastMsgTime == false -> Ph == Phase;
                   true -> Ph =< Phase
                end]),
    {Fmt, Div, PrFun} =
        if LastMsgTime == false ->
                {"\nDead Players " ++ pr_phase_long(Phase) ++ ":\n~s\n",
                 "\n",
                 fun(_, _) -> "" end};
           true ->
                {"\n"
                 "Dead Players\n"
                 "------------\n"
                 "~s\n",
                 "\n",
                 fun(IsEnd, Ph) -> pr_eodon(IsEnd, Ph) end}
        end,
    io:format(P#pp.dev,
              Fmt,
              [string:join(
                 [b2l(DeadPl) ++ PrFun(IsEnd, Ph) ++
                      if Com == undefined ->
                              " - msg: " ++ i2l(MsgId);
                         is_binary(Com) ->
                              " - " ++ b2l(Com)
                      end
                  || #death{player = DeadPl,
                            is_end = IsEnd,
                            phase = Ph,
                            msg_id = MsgId,
                            comment = Com}
                         <- DeathsToReport],
                 Div)]),
    if is_integer(P#pp.next) ->
            io:format(P#pp.dev,
                      "\n"
                      "Updates currently every ~p minutes "
                      "(more often near deadlines).\n"
                      "Mafia game thread at: ~s\n",
                      [P#pp.next, ?UrlBeg ++ i2l(ThId)]);
       true -> ok
    end.

print_time_left_to_dl(P, ThId, LastMsgTime) ->
    {{Days, {HH, MM, _}}, {Num, DoN, _}} =
        mafia_time:get_next_deadline(ThId, LastMsgTime),
    DayStr = if Days == 0 -> "";
                true -> i2l(Days) ++ " day, "
             end,
    io:format(P#pp.dev,
              "\n"
              "Remaining time to next ~s ~p deadline:"
              "  ~s~p hours, ~p minutes\n",
              [pr_don(DoN), Num, DayStr, HH, MM]).

pr_votes(P, Votes, Phase) ->
    %% [{Vote, Num, [{Time, User, Raw}]}]
    {VoteSummary, InvalidVotes} =
        lists:foldl(
          fun({User, UserVotes}, {Acc, Acc2}) ->
                  case lists:dropwhile(
                         fun(V) -> not V#vote.valid end,
                         UserVotes) of
                      [V|_] ->
                          Vote = V#vote.vote,
                          Raw = V#vote.raw,
                          {add_vote(Vote, Raw, V#vote.time, User, Acc),
                           Acc2};
                      [] ->
                          %% No valid vote found
                          case UserVotes of
                              [LastInvalidVote | _] ->
                                  {Acc, [{User, LastInvalidVote} | Acc2]};
                              [] -> % no votes at all
                                  {Acc, Acc2}
                          end
                  end
          end,
          {[], []},
          Votes),
    %% Sort summary on number of received votes
    GtEq = fun(A, B) -> element(2, A) >= element(2, B) end,
    VoteSumSort = lists:sort(GtEq, VoteSummary),

    io:format(P#pp.dev,
              "\nVotes ~s\n"
              "------------\n",
              [pr_phase_long(Phase)]),
    [begin
         Voters = [{Voter, Raw}
                   || {_VoteTime, Voter, Raw}
                          <- lists:sort(VoteInfos)],
         io:format(P#pp.dev, "~p ~s - ", [N, b2l(Vote)]),
         VotersInTimeOrder =
             [b2l(Voter) || {Voter, _Raw3} <- Voters],
         io:format(P#pp.dev,
                   "~s\n",
                   [string:join(VotersInTimeOrder, ", ")])
     end || {Vote, N, VoteInfos} <- VoteSumSort],
    {VoteSumSort, InvalidVotes}.

time_for_last_msg() ->
    ThId = getv(thread_id),
    LastPage = lists:last(
                 lists:sort(
                   mafia:find_pages_for_thread(ThId))),
    LastMsgId = lists:last(
                  (hd(mnesia:dirty_read(page_rec, {ThId, LastPage})))
                  #page_rec.message_ids),
    (hd(mnesia:dirty_read(message, LastMsgId)))#message.time.

pr_eodon(true, Phase) -> " died " ++ pr_eodon(Phase);
pr_eodon(false, {Num, DoN}) -> " died " ++ pr_don(DoN) ++ " " ++ i2l(Num).

pr_eodon({Num, ?day}) -> "EoD"++ i2l(Num);
pr_eodon({Num, ?night}) -> "EoN"++ i2l(Num).

pr_phase_long({Num, DoN}) -> pr_don(DoN) ++ " " ++ i2l(Num).

print_stats() ->
    print_stats(#pp{}, 1420289, {1, ?day}).

print_stats(P, ThId, Phase) ->
    print_stats(P, ThId, Phase, rgame(ThId)).

print_stats(_P, _ThId, _Phase, []) -> ok;
print_stats(P, ThId, Phase = {Day, DoN}, [G]) ->
    MatchHead = #stat{key = {'$1', '$2', {'$3', '$4'}}, _='_'},
    Guard = [{'==', '$2', ThId}, {'==', '$3', Day}, {'==', '$4', DoN}],
    Result = '$_',
    Stats = mnesia:dirty_select(stat, [{MatchHead, Guard, [Result]}]),
    LE = fun(#stat{num_postings = PA, num_words = WA},
             #stat{num_postings = PB, num_words = WB}) ->
                 if PA < PB -> true;
                    PA > PB -> false;
                    WA =< WB -> true;
                    true -> false
                 end
         end,
    StatsSorted = lists:sort(LE, Stats),
    UserU = fun(#stat{key = {U,_,_}}) -> transl(U) end,
    NonPosters = [b2l(PRem) || PRem <- G#mafia_game.players_rem]
        -- [UserU(S) || S <- Stats],
    io:format(P#pp.dev,
              "\n"
              "Posting statistics (~s)\n"
              "------------------\n"
              "~s ~s ~s ~s\n",
              [pr_phase_long(Phase), %%pr_don(DoN), Day,
               "Posts", "Words", " Chars", "Player"]),
    print_stat_div(P),
    SumStat =
        lists:foldl(
          fun(S, Sum) ->
                  print_stat_row(P, S, UserU),
                  mafia_data:sum_stat(S, Sum)
          end,
          #stat{msg_ids = [],
                num_chars = 0,
                num_words = 0,
                num_postings = 0
               },
          lrev(StatsSorted)),
    print_stat_div(P),
    print_stat_row(P, SumStat, fun(_) -> "Total Counts" end),
    io:format(P#pp.dev,
              "\nNon-posters: ~s\n",
              [case string:join(NonPosters, ", ") of
                   "" -> "-";
                   Str -> Str
               end]).

print_stat_div(P) ->
    io:format(P#pp.dev,
              "~s ~s ~s ~s\n",
              ["-----", "-----", "------", "-----------"]).

print_stat_row(P, S, UserU) ->
    io:format(P#pp.dev,
              "~s ~s ~s ~s\n",
              [i2l(S#stat.num_postings, 5),
               i2l(S#stat.num_words, 5),
               i2l(S#stat.num_chars, 6),
               UserU(S)
              ]).

%% Get user name as stored normal case string
transl(UserUB) ->
    UInfo = hd(mnesia:dirty_read(user, UserUB)),
    b2l(UInfo#user.name).

%% [{Vote, Num, [{Time, User, Raw}]}]
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

print_tracker() ->
    print_tracker(rgame()).

print_tracker(_P, _ThId, {_, ?night}) -> ok;
print_tracker(P, ThId, Phase) ->
    print_trackerI(P, rgame(ThId), rday(ThId, Phase)).

print_tracker([G]) ->
    ThId = G#mafia_game.key,
    LastMsgTime = time_for_last_msg(),
    Phase = mafia_time:calculate_phase(G, LastMsgTime),
    print_trackerI(#pp{}, G, rday(ThId, Phase)).

print_trackerI(P, [G], Day) -> print_trackerI(P, G, Day);
print_trackerI(P, G, [#mafia_day{votes = Votes0,
                                 players_rem = PlayersRem,
                                 player_deaths = Deaths}]) ->
    AllPlayers = PlayersRem ++ [DeadB || #death{player = DeadB} <- Deaths],
    Votes = [V || V <- Votes0,
                  lists:member(element(1, V), AllPlayers)],
    Votes3 = user_vote_timesort(Votes),
    %%io:format(P#pp.dev, "Rem: ~p\n", [AllPlayers]),
    Abbrs = mafia_name:get_abbrevs(AllPlayers),
    io:format(P#pp.dev, "\n", []),
    print_read_key(P, Abbrs),
    A = fun("---") -> "---";
           ("INV") -> "INV";
           (V) -> case lists:keyfind(V, 2, Abbrs) of
                      false ->
                          io:format(P#pp.dev, "~s\n", [V]),
                          "***";
                      {_, _, Abbr, _} -> Abbr
                  end
        end,
    Users = [b2l(UserB) || UserB <- AllPlayers],
    IterVotes = [{User, "---"} || User <- Users],

    io:format(P#pp.dev,
              "\n"
              "Vote tracker\n"
              "------------\n",
              []),
    FmtVoter = "Voter ~s\n",
    FmtTime = "Time  ~s\n",
    io:format(P#pp.dev, FmtVoter, [pr_ivs_user(IterVotes, A)]),
    io:format(P#pp.dev,
              FmtTime,
              [pr_ivs_user(IterVotes, fun(_) -> "===" end)]),
    lists:foldl(
      fun({User, V = #vote{}}, IVs) ->
              {NewIVs, PrIVs} =
                  if V#vote.valid ->
                          NewVote = A(b2l(V#vote.vote)),
                          IVs2 =
                              lists:keyreplace(User, 1, IVs, {User, NewVote}),
                          {IVs2, IVs2};
                     not V#vote.valid ->
                          NewVote =  "INV",
                          IVs2 =
                              lists:keyreplace(User, 1, IVs, {User, NewVote}),
                          {IVs, IVs2}
                  end,
              TimeStr = print_time_5d(G, V#vote.time),
              io:format(P#pp.dev,
                        "~s~s~s\n",
                        [TimeStr,
                         pr_ivs_vote(PrIVs, User),
                         TimeStr
                        ]),
              NewIVs
      end,
      IterVotes,
      Votes3),
    Votes3,
    io:format(P#pp.dev,
              FmtTime,
              [pr_ivs_user(IterVotes, fun(_) -> "===" end)]),
    io:format(P#pp.dev,
              FmtVoter,
              [pr_ivs_user(IterVotes, A)]),
    ok.

print_time_5d(G, Time) ->
    {HH, MM} = mafia_time:hh_mm_to_deadline(G, Time),
    p(HH) ++ ":" ++ p(MM).

user_vote_timesort(Votes) ->
    Votes2 =
        lists:foldl(
          fun({UserB, UVotes}, Acc) ->
                  [{b2l(UserB), V} || V <- UVotes] ++ Acc
          end,
          [],
          Votes),
    SortF = fun({_, #vote{time = TimeA}},
                {_, #vote{time = TimeB}}) ->
                    TimeA =< TimeB
            end,
    lists:sort(SortF, Votes2).

print_read_key(P, Abbrs) ->
    NumCols = 19,
    CFmt = "~-" ++ i2l(NumCols) ++ "s",
    AbbrStrs = [A ++ " " ++ Pl || {_, Pl, A, _} <- Abbrs],
    io:format(P#pp.dev,
              "Read Key\n"
              "--------\n",
              []),
    prk(P, CFmt, AbbrStrs).

prk(_P, _CFmt, []) -> ok;
prk(P, CFmt, Abbrs) ->
    NumAbbr = length(Abbrs),
    NumPrint = if NumAbbr >= 4 -> 4; true -> NumAbbr end,
    AbbrsPrint = string:substr(Abbrs, 1, NumPrint),
    AbbrsRem = lists:nthtail(NumPrint, Abbrs),
    Fmt = string:join([CFmt|| _ <- AbbrsPrint], " ") ++ "\n",
    io:format(P#pp.dev, Fmt, AbbrsPrint),
    prk(P, CFmt, AbbrsRem).

pr_ivs_user(IVs, A) ->
    string:join([A(U) || {U, _V} <- IVs], " ").

pr_ivs_vote(IVs, User) ->
    pr_ivs_vote(IVs, User, "").

pr_ivs_vote([], _User, Acc) ->
    Acc;
%% Second last matches
pr_ivs_vote([{U, V}, {_, V2}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++l2u(V)++"]" ++ V2 ++ " ");
%% Last matches
pr_ivs_vote([{U, V}], User, Acc) when U == User ->
    pr_ivs_vote([], User, Acc ++ "["++l2u(V)++"]");
%% Match
pr_ivs_vote([{U, V}, {_, V2} | T], User, Acc) when U == User ->
    pr_ivs_vote(T, User, Acc ++ "["++l2u(V)++"]"++V2);
pr_ivs_vote([{_, V}], User, Acc) ->
    pr_ivs_vote([], User, Acc ++ " "++ V ++" ");
pr_ivs_vote([{_, V} | T], User, Acc) ->
    pr_ivs_vote(T, User, Acc ++ " " ++ V).

%% -----------------------------------------------------------------------------

print_messages(User) when is_list(User) ->
    print_messages(l2b(User));
print_messages(User) when is_binary(User) ->
    ThId = getv(thread_id),
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
    ThId = getv(thread_id),
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
                        lrev(
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
    io:format("User  : ~s\n"
              "Page  : ~s\n"
              "Time  : ~s\n"
              "Thread: ~s\n"
              "Msg id: ~s\n"
              "Wrote : \"~s\"\n"
              "\n",
              [b2l(M#message.user_name),
               i2l(M#message.page_num),
               print_time(M#message.time),
               i2l(M#message.thread_id),
               i2l(M#message.msg_id),
               b2l(M#message.message)
              ]).

%% -----------------------------------------------------------------------------

print_message_summary(M) ->
    Msg = rm_nl(b2l(M#message.message)),
    MsgLen = length(Msg),
    Max = 80,
    MsgShort = if MsgLen > Max -> string:left(Msg, Max) ++ "...";
                  true -> Msg
               end,
    Str =
        io_lib:format("~-10s "
                      "~-3s"
                      " ~-11s "
                      "~-7s "
                      "~s\n",
                      [b2l(M#message.user_name),
                       i2l(M#message.page_num),
                       print_time(M#message.time, short),
                       i2l(M#message.msg_id),
                       MsgShort
                      ]),
    io:format("~s", [Str]).

%% -----------------------------------------------------------------------------

print_time(Time) ->
    print_time(Time, long).

print_time(Time, Mode) when is_integer(Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_time(Time, TzH, Dst, Mode).

%% half this fun should go to mafia_time
print_time(Time, TzH, Dst, Mode) when is_integer(Time) ->
    try
        {{Y, M, D}, {HH,MM,SS}} =
            mafia_time:secs1970_to_local_datetime(Time, TzH, Dst),
        case {TzH, Dst, Mode} of
            {0, false, long} ->
                io_lib:format("~s-~s-~sZ~s:~s:~s",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS)]);
            {0, false, short} ->
                io_lib:format("~s-~sZ~s:~s",
                              [p(M), p(D), p(HH), p(MM)]);
            {_, _, long} ->
                DstStr = case Dst of false -> "N"; true -> "DST" end,
                io_lib:format("~s-~s-~sT~s:~s:~s (~s ~s)",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS),
                               i2l(TzH), DstStr]);
            {_, _, short} ->
                io_lib:format("~s-~sT~s:~s",
                              [p(M), p(D), p(HH), p(MM)])
        end
    catch _:_ -> ""
    end.

p(I) when I > 9 -> i2l(I);
p(I) -> string:right(i2l(I), 2, $0).

i2l(Int, Size) -> string:right(i2l(Int), Size).

rm_nl([$\n|T]) -> [$\s|rm_nl(T)];
rm_nl([H|T]) -> [H|rm_nl(T)];
rm_nl("") -> "".
