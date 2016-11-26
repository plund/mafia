-module(mafia_vote).

-export([check_for_vote/1]).

-import(mafia,
        [b2l/1,
         l2b/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

check_for_vote(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [] -> ignore;
        [Msg] -> check_for_vote(Msg)
    end;
check_for_vote(M = #message{}) ->
    verify_user(M),
    Msg = b2l(M#message.message),
    MsgUC = string:to_upper(Msg),
    case mnesia:dirty_read(mafia_game, M#message.thread_id) of
        [] -> ignore;
        [#mafia_game{players_rem = Players} = G] ->
            author_user(M, G),
            Players2 = add_nolynch_and_aliases(Players),
            SearchStr = "##VOTE",
            case mafia_data:rm_to_after_pos(MsgUC, SearchStr) of
                {0, ""} -> ignore;
                {Pos, RestUC} ->
                    RawVote =
                        l2b(string:strip(
                              string:left(
                                mafia_data:get_after_pos(
                                  Pos, length(SearchStr), Msg),
                                15))),
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
            end
    end.

%% -----------------------------------------------------------------------------

verify_user(M = #message{user_name = User}) ->
    UserU = l2b(string:to_upper(b2l(User))),
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
                {user, #user{name_upper = l2b(string:to_upper(b2l(User))),
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
    case mnesia:dirty_read(mafia_game, GId) of
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

author_user(M, G) ->
    User = b2l(M#message.user_name),
    UserU = string:to_upper(User),
    #mafia_game{gms = GMs,
                players_rem = Players} = G,
    UsersU = [string:to_upper(b2l(U))|| U <- GMs ++ Players],
    case lists:member(UserU, UsersU) of
        true -> ok;
        false ->
            io:format("Message sent by non-player ~p\n", [User])
    end.

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
    lists:reverse(lists:sort([{F(P), l2b(select_name(P))} || P <- Players])).

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
    RemainsU = [string:to_upper(b2l(R)) || R <- Rem],
    lists:member(UserU, RemainsU).

vote2(M, G, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        {DayNum, ?day} ->
            Key = {M#message.thread_id, DayNum},
            User = M#message.user_name,
            NewVote = #vote{time = M#message.time,
                            id = M#message.msg_id,
                            page = M#message.page_num,
                            vote = Vote,
                            raw = RawVote,
                            valid = IsOkVote
                           },
            Day =
                case mnesia:dirty_read(mafia_day, Key) of
                    [] ->
                        #mafia_day{key = Key,
                                   thread_id = M#message.thread_id,
                                   day = DayNum,
                                   votes = [],
                                   complete = false
                                  };
                    [Day2] -> Day2
                end,
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
