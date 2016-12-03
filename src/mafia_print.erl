-module(mafia_print).

-export([
         print_stats/0, print_stats/3,
         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2,
         print_messages/1,

         print_message_summary/1,

         print_pages_for_thread/0,
         print_pages_for_thread/1
        ]).

-import(mafia,
        [
         getv/1,
         b2l/1,
         l2b/1,
         i2l/1
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
    print_page(ThId, MsgIds, fun print_message_summary/1).

pm(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [Msg] -> print_message_full(Msg);
        [] -> io:format("Message ID ~p not found\n", [MsgId])
    end.

%% -----------------------------------------------------------------------------

print_votes() ->
    %% Calculate time for last game message
    LastMsgTime = time_for_last_msg(),
    ThId = getv(thread_id),
    Phase = mafia_time:calculate_phase(ThId, LastMsgTime),
    print_votes(ThId, Phase, LastMsgTime).

print_votes(DayNum, DoN) ->
    DoN2 = if DoN == d; DoN == day -> ?day;
              DoN == n; DoN == night -> ?night
           end,
    print_votes({DayNum, DoN2}).

print_votes(DayNum) when is_integer(DayNum) ->
    print_votes({DayNum, ?day});
print_votes(Phase = {_, _}) ->
    ThId = getv(thread_id),
    LastMsgTime = false,
    print_votes(ThId, Phase, LastMsgTime).

print_votes(ThId, Phase, LastMsgTime) ->
    print_votes(Phase, LastMsgTime,
                mnesia:dirty_read(mafia_game, ThId),
                mnesia:dirty_read(mafia_day, {ThId, element(1, Phase)})).

print_votes(_Phase, _LastMsgTime, [], _) -> ok;
print_votes(_Phase, _LastMsgTime, _, []) -> ok;
print_votes(Phase, LastMsgTime,
            [#mafia_game{key = ThId,
                         players_rem = RemPlayers} = G],
            [#mafia_day{votes = Votes}]
           ) ->

    IsDay = element(2, Phase) == ?day,

    %% Part 1 - Page heading
    %% Print Game Name
    GName = b2l(G#mafia_game.name),
    io:format("\n~s\n~s\n", [GName,
                            [$= || _ <- GName]]),

    if is_integer(LastMsgTime) ->
            {{Days, {HH, MM, _}}, {Num, DoN, _}} =
                mafia_time:get_next_deadline(ThId, LastMsgTime),
            io:format("Remaining time to next ~s ~p deadline: "
                      "~p days ~p hours and ~p minutes\n"
                      "\n",
                      [pr_don(DoN), Num, Days, HH, MM]);
       true -> no_print
    end,

    {VoteSumSort, InvalidVotes} =
        if IsDay ->
                %% Part 2 - Votes
                pr_votes(Votes, Phase);
           true ->
                {na, na}
        end,

    if IsDay ->
            %% Part 3 - No vote
            ValidVoters = [ Pl || {Pl, _} <- Votes]
                -- [User || {User, _} <- InvalidVotes],
            Unvoted = RemPlayers -- ValidVoters,
            NoVoteStr =
                if Unvoted == [] -> "-";
                   true ->
                        string:join([b2l(U) || U <- Unvoted], ", ")
                end,
            io:format("No vote: ~s\n\n", [NoVoteStr]);
       true -> ignore
    end,

    %% Part 4 - Dead players
    DeadToReport =
        lists:reverse(
          [D || D = {_, {_, Ph}} <- G#mafia_game.players_dead,
                if LastMsgTime == false -> Ph == Phase;
                   true -> Ph =< Phase
                end]),
    {Fmt, Div, PrFun} =
        if LastMsgTime == false ->
                {"Dead Players " ++ pr_phase_long(Phase) ++ ": ~s\n\n", ", ",
                 fun(_, _) -> "" end};
           true ->
                {"Dead Players\n"
                 "------------\n"
                 "~s\n\n",
                 "\n",
                 fun(IsEnd, Ph) -> pr_eodon(IsEnd, Ph) end}
        end,
    io:format(
      Fmt,
      [string:join(
         [b2l(DeadPl) ++ PrFun(IsEnd, Ph)
          || {DeadPl, {IsEnd, Ph={_DoNNum, _DN}}} <- DeadToReport],
         Div)]),

    if IsDay ->
            %% Part 4 - Voting texts
            io:format("Voting texts:\n"
                      "-------------\n"),
            [[io:format(b2l(Voter) ++ ": \"" ++ rm_nl(b2l(Raw)) ++ "\"\n")
              || {_VoteTime, Voter, Raw} <- VoteInfos]
             || {_Vote, _N, VoteInfos} <- VoteSumSort],

            %% Part 5 - Invalid Vote text
            io:format("\n"
                      "Invalid Vote texts:\n"
                      "-------------------\n"),
            [io:format(b2l(Voter) ++ ": \"" ++ rm_nl(b2l(Raw)) ++ "\"\n")
             || {Voter, #vote{raw = Raw}} <- InvalidVotes],
            io:format("\n");
       true -> ignore
    end,

    %% Part 6
    print_stats(ThId, Phase),
    ok.

pr_votes(Votes, Phase) ->
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

    io:format("Votes ~s\n"
              "------------\n", [pr_phase_long(Phase)]),
    [begin
         Voters = [{Voter, Raw}
                   || {_VoteTime, Voter, Raw}
                          <- lists:sort(VoteInfos)],
         io:format("~s - ~p - ", [b2l(Vote), N]),
         VotersInTimeOrder =
             [b2l(Voter) || {Voter, _Raw3} <- Voters],
         io:format("~s\n", [string:join(VotersInTimeOrder, ", ")])
     end || {Vote, N, VoteInfos} <- VoteSumSort],
    io:format("\n"),
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
    print_stats(1420289, {1, ?day}).

print_stats(ThId, Phase) ->
    print_stats(ThId, Phase, mnesia:dirty_read(mafia_game, ThId)).

print_stats(_ThId, _Phase, []) -> ok;
print_stats(ThId, Phase = {Day, DoN}, [G]) ->
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
    UserU = fun(#stat{key = {U,_,_}}) -> tr(U) end,
    NonPosters = [b2l(PRem) || PRem <- G#mafia_game.players_rem]
        -- [UserU(S) || S <- Stats],
    io:format("Posting statistics (~s)\n"
              "------------------\n"
              "~s ~s ~s ~s\n",
              [pr_phase_long(Phase), %%pr_don(DoN), Day,
               "Posts", "Words", " Chars", "Player"]),
    print_stat_div(),
    SumStat =
        lists:foldl(
          fun(S, Sum) ->
                  print_stat_row(S, UserU),
                  mafia_data:sum_stat(S, Sum)
          end,
          #stat{msg_ids = [],
                num_chars = 0,
                num_words = 0,
                num_postings = 0
               },
          lists:reverse(StatsSorted)),
    print_stat_div(),
    print_stat_row(SumStat, fun(_) -> "Total Counts" end),
    io:format("\nNon-posters: ~s\n",
              [case string:join(NonPosters, ", ") of
                   "" -> "-";
                   Str -> Str
               end]).

print_stat_div() ->
    io:format("~s ~s ~s ~s\n",
              ["-----", "-----", "------", "-----------"]).

print_stat_row(S, UserU) ->
    io:format("~s ~s ~s ~s\n",
              [i2l(S#stat.num_postings, 5),
               i2l(S#stat.num_words, 5),
               i2l(S#stat.num_chars, 6),
               UserU(S)
              ]).

tr(UserUB) ->
    UInfo = hd(mnesia:dirty_read(user, UserUB)),
    b2l(UInfo#user.name).

i2l(Int, Size) -> string:right(i2l(Int), Size).

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
    case mnesia:dirty_read(mafia_game, ThId) of
        [] ->
            [PrintFun(M) || M <- MsgsPage];
        [#mafia_game{deadlines = DLs} = G] ->
            TimeB = ?TMsg((hd(MsgsPage))),
            TimeA = ?TMsg((lists:last(MsgsPage))),

            %% Are more deadlines needed
            TimeLDl = ?TDl(hd(DLs)),
            DLs2 =
                if TimeLDl < TimeA ->
                        lists:reverse(
                          mafia_time:update_deadlines(ThId, 10));
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
    Max = 30,
    MsgShort = if MsgLen > Max -> string:left(Msg, Max) ++ "...";
                  true -> Msg
               end,
    Str =
        io_lib:format("~s, "
                      "p ~s, "
                      " ~s, "
                      "id: ~s, "
                      "\"~s\"\n",
                      [string:left(b2l(M#message.user_name), 12),
                       i2l(M#message.page_num),
                       print_time(M#message.time),
                       i2l(M#message.msg_id),
                       MsgShort
                      ]),
    io:format("~s", [Str]).

%% -----------------------------------------------------------------------------

%% half this fun should go to mafia_time
print_time(Time) when is_integer(Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_time(Time, TzH, Dst).

print_time(Time, TzH, Dst) when is_integer(Time) ->
    try
        {{Y, M, D}, {HH,MM,SS}} =
            mafia_time:local_datetime_for_secs1970(Time, TzH, Dst),
        case {TzH, Dst} of
            {0, false} ->
                io_lib:format("~s-~s-~sZ~s:~s:~s",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS)]);
            _ ->
                DstStr = case Dst of false -> "N"; true -> "DST" end,
                io_lib:format("~s-~s-~sT~s:~s:~s (~s ~s)",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS),
                               i2l(TzH), DstStr])
        end
    catch _:_ -> ""
    end.

p(I) when I > 9 -> i2l(I);
p(I) -> string:right(i2l(I), 2, $0).

rm_nl([$\n|T]) -> [$\s|rm_nl(T)];
rm_nl([H|T]) -> [H|rm_nl(T)];
rm_nl("") -> "".