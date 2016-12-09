-module(mafia_vote).

-export([check_for_vote/1]).

-import(mafia,
        [b2l/1,
         l2b/1,
         lrev/1,
         rgame/1,
         rday/2
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

check_for_vote(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [] -> ignore;
        [Msg] -> check_for_vote(Msg)
    end;
check_for_vote(M = #message{}) ->
    check_for_vote(M, rgame(M#message.thread_id)).

check_for_vote(_M, []) -> ignore;
check_for_vote(M, [G = #mafia_game{}]) ->
    G2 = check_for_deathI(M, G),
    check_for_voteI(M, G2).

%% Removes player from Game if dead
check_for_deathI(M, G) ->
    case author_gm(M, G) of
        false -> G;
        true ->
            check_for_deathI2(b2ul(M#message.message), M, G)
    end.

check_for_deathI2(MsgUC, M, G) ->
    %% find "has died" on line
    SearchU1 = "DIED",
    SearchU2 = "DEAD",
    SearchU3 = "BEEN LYNCHED",
    case {mafia_data:find_pos_and_split(MsgUC, SearchU1),
          mafia_data:find_pos_and_split(MsgUC, SearchU2),
          mafia_data:find_pos_and_split(MsgUC, SearchU3)} of
        {{0,_,_}, {0,_,_}, {0,_,_}} -> %% no-one has died
            G;
        {Pos1, Pos2, Pos3} ->
            {_, HStr, TStr} =
                if element(1, Pos1) /= 0 -> Pos1;
                   element(1, Pos2) /= 0 -> Pos2;
                   true -> Pos3
                end,
            %% find any remaining players before on the same line.
            RevStr = lrev(HStr),
            RevStr2 = case string:tokens(RevStr, ".\n") of
                          [] -> "";
                          [Head | _] -> Head
                      end,
            LineU = lrev(RevStr2),
            RemUsersU = [b2ul(PremB)
                         || PremB <- G#mafia_game.players_rem],
            case lists:dropwhile(
                   fun(UserU) ->
                           case mafia_data:find_pos_and_split(LineU, UserU) of
                               {0, _, _} -> true;
                               {_, HStr2, TStr2} ->
                                   not is_last_non_letter(HStr2) orelse
                                       not is_first_non_letter(TStr2)
                           end
                   end,
                   RemUsersU) of
                [] -> %% no match
                    check_for_deathI2(TStr, M, G);
                [KilledUserU|_] ->
                    %% remove player from _rem lists.
                    OldRems = G#mafia_game.players_rem,
                    NewRems =
                        lists:filter(
                          fun(OldPlB) -> b2ul(OldPlB) /= KilledUserU
                          end,
                          OldRems),
                    if NewRems /= OldRems ->
                            DeadB = hd(OldRems -- NewRems),
                            DeadStr = b2l(DeadB),
                            OldDeads = G#mafia_game.players_dead,
                            io:format("Player ~s died\n", [DeadStr]),
                            DeadInfo = {DeadB, phase_10min_ago(M, G)},
                            NewDeads = [DeadInfo | OldDeads],
                            update_day_rec(M, G, DeadInfo),
                            G2 = G#mafia_game{players_rem = NewRems,
                                              players_dead = NewDeads},
                            mnesia:dirty_write(G2),
                            check_for_deathI2(TStr, M, G2);
                       true ->
                            check_for_deathI2(TStr, M, G)
                    end
            end
    end.

-spec phase_10min_ago(M :: #message{}, G :: #mafia_game{})
                     -> {IsEnd :: boolean(), phase()}.
phase_10min_ago(M, G) ->
    TimeMsg = M#message.time,
    PhaseMsg = mafia_time:calculate_phase(G, TimeMsg),
    Time10m = TimeMsg - 10 * ?MinuteSecs,
    Phase10m = mafia_time:calculate_phase(G, Time10m),
    IsEnd = PhaseMsg /= Phase10m,
    {IsEnd, Phase10m}.

%% in case someone votes before GM annouce dead, the day record
%% will have too many remaining players
update_day_rec(M, G, Dead) ->
    TimeMsg = M#message.time,
    case mafia_time:calculate_phase(G, TimeMsg) of
        {DayNum, ?day} -> %% New day appears with new day record.
            [D] = rday(G, DayNum),
            case Dead of
                {DeadB, {true, _Phase}} ->
                    NewRems = D#mafia_day.players_rem -- [DeadB],
                    mnesia:dirty_write(
                      D#mafia_day{players_rem = NewRems});
                {DeadB, {false, _Phase}} ->
                    %% For the "Jamie" case  when Vig kills in mid day
                    NewDeads = [Dead|D#mafia_day.players_dead],
                    NewRems = D#mafia_day.players_rem -- [DeadB],
                    mnesia:dirty_write(
                      D#mafia_day{players_rem = NewRems,
                                  players_dead = NewDeads})
            end;
        _ -> ok
    end.

is_last_non_letter(HStr) ->
    is_first_non_letter(lrev(HStr)).

is_first_non_letter([]) -> true;
is_first_non_letter([H|_]) ->
    lists:member(H, " ,.;:!\"#€%7&/()=+?´`<>-_\t\r\n").

check_for_voteI(M, G) ->
    verify_user(M),
    Msg = b2l(M#message.message),
    MsgUC = string:to_upper(Msg),
    Players = G#mafia_game.players_rem,
    author_user(M, G),
    Players2 = add_nolynch_and_aliases(Players),
    VoteStr = "##VOTE",
    UnvoteStr = "##UNVOTE",
    case {mafia_data:rm_to_after_pos(MsgUC, VoteStr),
          string:str(MsgUC, UnvoteStr)} of
        {{0, ""}, 0} -> ignore;
        {{0, ""}, _} ->
            Vote = l2b(?Unvote),
            reg_vote(M, G, Vote, Vote, true);
        {{Pos, RestUC}, _} ->
            RawVote =
                l2b(string:strip(
                      string:left(
                        mafia_data:get_after_pos(
                          Pos, length(VoteStr), Msg),
                        15))),
            %%io:format("DBG ~p\n", [Players2]),
            case rank_options(Players2, RestUC) of
                [{NumV, TopP}] when NumV >= 2; NumV >= length(TopP) ->
                    reg_vote(M, G, TopP, RawVote, true);
                [{NumV1, TopP}, {NumV2, _}|_]
                  when NumV1 > NumV2 andalso
                       (NumV1 >= 2 orelse
                        NumV1 >= length(TopP)) ->
                    reg_vote(M, G, TopP, RawVote, true);
                _ ->
                    Vote = l2b("-"),
                    reg_vote(M, G, Vote, RawVote, false)
            end
    end,
    EndStr = "##END",
    UnendStr = "##UNEND",
    case {string:str(MsgUC, EndStr),
          string:str(MsgUC, UnendStr)} of
        {0, 0} ->
            ok;
        {_, 0} -> %% add end
            reg_end_vote(add, M);
        {0, _} -> %% remove end
            reg_end_vote(remove, M);
        _ -> ok
    end.

reg_end_vote(Op, M) ->
    case mafia_time:calculate_phase(M#message.thread_id, M#message.time) of
        {DayNum, ?day} ->
            case rday(M#message.thread_id, DayNum) of
                [Day] ->
                    User = M#message.user_name,
                    OldEndVotes = Day#mafia_day.end_votes,
                    NewEndVotes =
                        case Op of
                            add ->
                                case lists:member(User, OldEndVotes) of
                                    false -> OldEndVotes ++ [User];
                                    true ->  OldEndVotes
                                end;
                            remove ->
                                OldEndVotes -- [M#message.user_name]
                        end,
                    mnesia:dirty_write(Day#mafia_day{end_votes = NewEndVotes});
                _ -> ok
            end;
        _ ->
            ok
    end.

%% -----------------------------------------------------------------------------

verify_user(M = #message{user_name = User}) ->
    UserU = b2ub(User),
    CheckRes =
        case mnesia:dirty_read(user, UserU) of
            [#user{verification_status = ?verified}] -> ok;
            [#user{name = User} = U] ->
                {user, U#user{verification_status = ?verified}};
            [#user{} = U] ->
                {user_game,
                 U#user{name = User,
                        verification_status = ?verified}};
            [] ->
                io:format("Warning: created new user ~p\n", [User]),
                {user, #user{name_upper = b2ub(User),
                             name = User,
                             verification_status = ?verified}}
        end,
    case CheckRes of
        ok -> ok;
        {Type, UserRec} ->
            mnesia:dirty_write(UserRec),
            if Type == user_game ->
                    auto_correct_case(b2l(User), M#message.thread_id);
               true -> ok
            end
    end.

%% -----------------------------------------------------------------------------

auto_correct_case(CcUser, GId) when is_integer(GId) ->
    case rgame(GId) of
        [] -> ok;
        [G] -> auto_correct_case(CcUser, G)
    end;
auto_correct_case(CcUser, G = #mafia_game{}) ->
    io:format("Correcting case for user ~p in game M~p\n",
              [CcUser, G#mafia_game.game_num]),
    CorrectF = correct_case_fun2(),
    PsOrigL = [b2l(P) || P <- G#mafia_game.players_orig],
    PsOrigL2 = [CorrectF(CcUser, P) || P <- PsOrigL],
    if PsOrigL2 /= PsOrigL ->
            PsOrigB = [l2b(P) || P <- PsOrigL2],
            PsRemL = [b2l(P) || P <- G#mafia_game.players_rem],
            PsRemL2 = [CorrectF(CcUser, P) || P <- PsRemL],
            if PsRemL2 /= PsRemL ->
                    PsRemB = [l2b(P) || P <- PsRemL2],
                    mnesia:dirty_write(
                      G#mafia_game{players_orig = PsOrigB,
                                   players_rem = PsRemB});
               true ->
                    mnesia:dirty_write(
                      G#mafia_game{players_orig = PsOrigB})
            end;
       true -> ok
    end.

correct_case_fun2() ->
    fun (CorrectCaseL, ExistingL) ->
            CcUC = string:to_upper(CorrectCaseL),
            ExUC = string:to_upper(ExistingL),
            if CcUC == ExUC ->
                    CorrectCaseL;
               true -> ExistingL
            end
    end.

%% -----------------------------------------------------------------------------

author_gm(M, G) ->
    authorI(M, G#mafia_game.gms).

author_user(M, G) ->
    #mafia_game{gms = GMs,
                players_rem = Players} = G,
    UsersB = GMs ++ Players,
    case authorI(M, UsersB) of
        true -> ok;
        false ->
            User = b2l(M#message.user_name),
            io:format("Message sent by non-player ~p\n",
                      [User])
    end.

authorI(M, UsersB) ->
    User = b2l(M#message.user_name),
    UserU = string:to_upper(User),
    UsersU = [b2ul(U) || U <- UsersB],
    lists:member(UserU, UsersU).

%% -----------------------------------------------------------------------------

add_nolynch_and_aliases(Players) ->
    Players2 = [b2l(P) || P <- Players] ++ ?Extra,
    lists:foldl(
      fun({Na, Als}, PlAcc) ->
              PlAcc ++
                  case lists:member(Na, Players2) of
                      true ->
                          [{Na, A} || A <- Als];
                      false -> []
                  end
      end,
      Players2,
      ?Aliases).

%% -----------------------------------------------------------------------------

rank_options(Players, RestUC) ->
    RestUCW = waste_spaces(RestUC),
    F = fun(P) ->
                PorA = select_alias(P),
                PlayerUCW = string:to_upper(waste_spaces(PorA)),
                r_count(PlayerUCW, RestUCW, 0)
        end,
    lrev(lists:sort([{F(P), l2b(select_name(P))} || P <- Players])).

waste_spaces(L) -> [E || E <- L, E /= $\s].

select_name({Name, _Alias}) -> Name;
select_name(Name) -> Name.

select_alias({_Name, Alias}) -> Alias;
select_alias(Name) -> Name.

r_count([Hp|Tp], [Hr|Tr], N) when Hp == Hr ->
    r_count(Tp, Tr, N+1);
r_count([_Hp|_Tp], [], N) ->
    N;
r_count([], [_Hr|_Tr], N) ->
    N + 1;
r_count([Hp|_Tp], [Hr|_Tr], N) when Hp /= Hr ->
    N;
r_count([], [], N) ->
    N + 1.

%% -----------------------------------------------------------------------------

reg_vote(M, G, Vote, RawVote, IsOkVote) ->
    case is_remaining_player(
           M#message.user_name,
           G#mafia_game.players_rem) of
        true ->
            vote2(M, G, Vote, RawVote, IsOkVote);
        false ->
            io:format("Warning ~s tried to vote in game\n",
                      [b2l(M#message.user_name)]),
            ignore
    end.


-spec is_remaining_player(User :: player(),
                          Remain :: [player()]) -> boolean().
is_remaining_player(User, Rem) ->
    UserL = b2l(User),
    UserU = string:to_upper(UserL),
    RemainsU = [b2ul(R) || R <- Rem],
    lists:member(UserU, RemainsU).

vote2(M, G, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        {DayNum, ?day} ->
            io:format(
              "Register Vote: ~s votes ~p ~s\n",
              [b2l(M#message.user_name), b2l(RawVote),
               if IsOkVote -> "Approved"; true -> "Rejected" end]),
            User = M#message.user_name,
            NewVote = #vote{time = M#message.time,
                            id = M#message.msg_id,
                            page = M#message.page_num,
                            vote = Vote,
                            raw = RawVote,
                            valid = IsOkVote
                           },
            Day = hd(rday(M#message.thread_id, DayNum)),
            Votes = Day#mafia_day.votes,
            Votes2 = case lists:keyfind(User, 1, Votes) of
                         false ->
                             [{User, [NewVote]} | Votes];
                         {User, UVotes} ->
                             UVotes2 = [NewVote | UVotes],
                             lists:keystore(User, 1, Votes, {User, UVotes2})
                     end,
            mnesia:dirty_write(Day#mafia_day{votes = Votes2});
        _ ->
            ignore
    end.

b2ub(Binary) -> l2b(b2ul(Binary)).

b2ul(Binary) -> string:to_upper(b2l(Binary)).
