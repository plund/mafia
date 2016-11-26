-module(mafia_print).

-export([
         pp/0, pp/1, pp/2,
         pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_message_summary/1,

         print_pages_for_thread/0,
         print_pages_for_thread/1
        ]).

-import(mafia,
        [
         getv/1,
         b2l/1,
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
    print_page(ThId, Page, fun print_message_full/1).

pps({ThId, Page}) ->
    pps(ThId, Page).

pps(ThId, Page) ->
    print_page(ThId, Page, fun print_message_summary/1).

pm(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [Msg] ->
            mafia_print:print_message_full(Msg);
        [] -> io:format("Message ID ~p not found\n", [MsgId])
    end.

%% -----------------------------------------------------------------------------

print_votes() ->
    print_votes(1).

print_votes(DayNum) ->
    ThId = getv(thread_id),
    print_votes(ThId, DayNum).

print_votes(ThId, DayNum) ->
    print_votes(DayNum,
                mnesia:dirty_read(mafia_game, ThId),
                mnesia:dirty_read(mafia_day, {ThId, DayNum})).

print_votes(_DayNum, [], _) -> ok;
print_votes(_DayNum, _, []) -> ok;
print_votes(DayNum,
            [#mafia_game{players_rem = RemPlayers}],
            [#mafia_day{votes = Votes}]
           ) ->
    %% [{Vote, Num, [{Time, User, Raw}]}]
    VoteSummary =
        lists:foldl(
          fun({User, UserVotes}, Acc) ->
                  case lists:dropwhile(
                         fun(V) -> not V#vote.valid end,
                         UserVotes) of
                      [V|_] ->
                          Vote = V#vote.vote,
                          Raw = V#vote.raw,
                          add_vote(Vote, Raw, V#vote.time, User, Acc);
                      [] -> Acc
                  end
          end,
          [],
          Votes),
    %% Sort summary on number of received votes
    GtEq = fun(A, B) -> element(2, A) >= element(2, B) end,
    VoteSum2 = lists:sort(GtEq, VoteSummary),
    io:format("Votes day ~p\n"
              "------------\n", [DayNum]),
    [begin
         Voters = [{Voter, Raw}
                   || {_VoteTime, Voter, Raw}
                          <- lists:sort(VoteInfos)],
         io:format("~s - ~p - ", [b2l(Vote), N]),
         VotersInTimeOrder =
             [b2l(Voter) || {Voter, _Raw3} <- Voters],
         io:format("~s\n", [string:join(VotersInTimeOrder, ", ")])
     end || {Vote, N, VoteInfos} <- VoteSum2],

    %% Part 2
    Unvoted = RemPlayers -- [ Pl || {Pl, _} <- Votes],
    io:format("\nUnvoted: ~s\n",
              [string:join([b2l(U) || U <- Unvoted], ",") ] ),

    %% Part 3
    io:format("\n"
              "Voting texts:\n"
              "-------------\n"),
    [[io:format(b2l(Voter) ++ ": \"" ++ rm_nl(b2l(Raw)) ++ "\"\n")
      || {_VoteTime, Voter, Raw} <- VoteInfos]
     || {_Vote, _N, VoteInfos} <- VoteSum2],
    ok.

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

print_pages_for_thread() ->
    ThId = getv(thread_id),
    print_pages_for_thread(ThId).

print_pages_for_thread(ThId) ->
    Pages = mafia:find_pages_for_thread(ThId),
    io:format("Thread ~p has stored Pages ~w\n", [ThId, Pages]).

%% -----------------------------------------------------------------------------

-define(TMsg(M), M#message.time).
-define(TDl(D), element(3, D)).

print_page(ThId, PageNum, PrintFun) ->
    print_page(ThId, PageNum, PrintFun, msgids(ThId, PageNum)).

print_page(_ThId, _PageNum, _PrintFun, []) -> ok;
print_page(ThId, PageNum, PrintFun, MsgIds) ->
    {Bef, MIdBefore} = getone(lists:reverse(msgids(ThId, PageNum-1))),
    %% MsgIds = msgids(ThId, PageNum),
    {Aft, MIdAfter} = getone(msgids(ThId, PageNum+1)),
    %% print starting line with current phase
    %% does this thread have a game?
    Msgs =
        (_MsgB = r_msgs(MIdBefore)) ++
        (MsgsPage = r_msgs(MsgIds)) ++
        (_MsgA = r_msgs(MIdAfter)),
    TimeB = ?TMsg((hd(Msgs))),
    TimeA = ?TMsg((lists:last(Msgs))),
    case mnesia:dirty_read(mafia_game, ThId) of
        [] ->
            [PrintFun(M) || M <- MsgsPage];
        [#mafia_game{deadlines = DLs} = G] ->
            TimeLDl = ?TDl(hd(DLs)),
            DLs2 =
                if TimeLDl < TimeA ->
                        lists:reverse(
                          mafia_time:update_deadlines(ThId, 10));
                      true ->
                           DLs
                   end,
            DLsIn = [D || D <- DLs2,
                          TimeB < ?TDl(D),
                          ?TDl(D) < TimeA],
            MixedSort = lists:sort(fun cmp_time/2, Msgs ++ DLsIn),
            MS2 = if Bef -> tl(MixedSort);
                     true -> MixedSort
                  end,
            MS3 = if Aft -> lists:reverse(tl(lists:reverse(MS2)));
                     true -> MS2
                  end,
            FirstTime = time(hd(MS2)),
            Phase = mafia_time:calculate_phase(G, FirstTime),
            print_dl(Phase, ""),
            [case E of
                 M = #message{} ->
                     PrintFun(M);
                 D ->
                     print_dl(D, "End of ")
             end || E <- MS3]
    end,
    ok.

getone([]) -> {false, []};
getone([One|_]) -> {true, [One]}.

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
              [Txt, pr(DorN), Num]).

pr(?day) -> "Day";
pr(?night) -> "Night".

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
