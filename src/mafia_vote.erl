-module(mafia_vote).

-export([check_for_vote/1,
         check_for_vote/2,
         verify_user/1,
         print_verify_user/1,

         kill_player/4
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

check_for_vote(MsgId) -> check_for_vote(unused_state, MsgId).

check_for_vote(S, MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [] -> ignore;
        [Msg] -> check_for_vote(S, Msg)
    end;
check_for_vote(S, M = #message{}) ->
    mafia_data:update_stat(M),
    check_for_vote(S, M, ?rgame(M#message.thread_id)).

check_for_vote(_S, _M, []) -> ignore;
check_for_vote(S, M, [G = #mafia_game{}]) ->
    check_for_vote(S, M, G);
check_for_vote(S, M, G = #mafia_game{}) ->
    DoCheck = case G#mafia_game.game_end of
                  ?undefined -> true;
                  {EndTime, _MsgId} ->
                      M#message.time =< EndTime
              end,
    if DoCheck ->
            case player_type(M, G) of
                ?gm -> check_for_gm_cmds(S, M, G);
                ?player -> check_for_votes(S, M, G);
                ?dead_player -> log_unallowed_msg(?dead_player, M);
                ?other -> log_unallowed_msg(?other, M)
            end;
       true ->
            ignore
    end,
    ok.

log_unallowed_msg(Type, M) ->
    MTime = M#message.time,
    User = ?b2l(M#message.user_name),
    MsgId = M#message.msg_id,
    ?dbg(MTime, {Type, sent_message, MsgId, User}).

%% Removes player from Game if dead
check_for_gm_cmds(_S, M, G) ->
    MsgText = mafia_print:html2txt(?b2l(M#message.message)),
    G2 = check_for_deaths(MsgText, M, G),

    %% if time is 0 - 15 min after a deadline generate a history page
    Time = M#message.time,
    {RelTimeSecs, DL} = mafia_time:nearest_deadline(G2, Time),
    if RelTimeSecs >= 0,
       RelTimeSecs =< ?MAX_GM_DL_MINS * ?MinuteSecs ->
            mafia_web:regenerate_history(Time, DL);
       true ->
            ok
    end,
    G2.

check_for_deaths(Msg, M, G) ->
    %% find "has died" on line
    SearchU1 = "DIED",
    SearchU2 = "DEAD",
    SearchU3 = "BEEN LYNCHED",
    case {find_parts(Msg, SearchU1),
          find_parts(Msg, SearchU2),
          find_parts(Msg, SearchU3)} of
        {{0,_,_}, {0,_,_}, {0,_,_}} -> %% no-one has died
            G;
        {Pos1, Pos2, Pos3} ->
            {Pos, HStr, TStr} =
                if element(1, Pos1) /= 0 -> Pos1;
                   element(1, Pos2) /= 0 -> Pos2;
                   true -> Pos3
                end,
            {KilledUserB, Comment, _MsgAfterNL} =
                read_death_line(G, HStr, TStr),
            DeathComment =
                if Comment == use_full_line -> get_line_at(Pos, Msg);
                   true -> Comment
                end,
            case KilledUserB of
                no_dead -> %% no match
                    check_for_deaths(TStr, M, G);
                _ ->
                    {_, G2} = kill_player(G, M, KilledUserB, DeathComment),
                    check_for_deaths(TStr, M, G2)
            end
    end.

-spec kill_player(#mafia_game{}, #message{}, user(), string())
                 -> {Resp :: term(), #mafia_game{}}.
kill_player(G, M, DeadB, DeathComment) ->
    IsMember = lists:member(DeadB, G#mafia_game.players_rem),
    kill_player(G, M, DeadB, DeathComment, IsMember).

kill_player(G, _M, _DeadB, _DeathComment, false) ->
    {not_remaining_player, G};
kill_player(G, M, DeadB, DeathComment, true) ->
    %% remove player from _rem lists.
    NewRems = G#mafia_game.players_rem -- [DeadB],
    DeadStr = ?b2l(DeadB),
    io:format(
      "~s Player ~s died\n",
      [mafia_print:print_time(M#message.time, short),
       DeadStr]),
    {IsEnd, DeathPhase} = is_end_of_phase(M, G),
    Death = #death{player = DeadB,
                   is_end = IsEnd,
                   phase = DeathPhase,
                   msg_id = M#message.msg_id,
                   time = M#message.time,
                   comment = ?l2b(DeathComment),
                   is_deleted = false
                  },
    NewDeaths = add_death(Death, G),
    update_day_rec(M, G, Death),
    G2 = G#mafia_game{players_rem = NewRems,
                      player_deaths = NewDeaths},
    mnesia:dirty_write(G2),
    {{ok, DeathPhase}, G2}.

%% Returns exact User binary.
-spec read_death_line(#mafia_game{}, string(), string())
                     -> {user(), Comment :: string(), StrAfterNL :: string()}.
read_death_line(G, HStr, TStr) ->
    %% find one remaining player before on the same line.
    RevStr = ?lrev(HStr),
    RevStr2 = case string:tokens(RevStr, ".\n") of
                  [] -> "";
                  [Head | _] -> Head
              end,
    PreLine = ?lrev(RevStr2),
    RemUsersU = G#mafia_game.players_rem,
    Users =
        lists:dropwhile(
          fun(UserU) ->
                  case find_parts(PreLine, ?b2ul(UserU)) of
                      {0, _, _} -> true;
                      {_, HStr2, TStr2} ->
                          not is_last_non_letter(HStr2) orelse
                              not is_first_non_letter(TStr2)
                  end
          end,
          RemUsersU),
    DeadUser = case Users of
                    [First | _] -> First;
                    [] -> no_dead
                end,
    {_, RemLine, AfterNextNL} = find_parts(TStr, "\n"),
    WasComment =
        case {find_parts_match(RemLine, "SHE WAS"),
              find_parts_match(RemLine, "HE WAS")} of
            {{0, _, _, _}, {0, _, _, _}} -> % Neither found use full line
                use_full_line;
            {Find1, Find2} ->
                {_, _Before, Match, After} =
                    if element(1, Find1) /= 0 -> Find1;
                       true -> Find2
                    end,
                Match ++ After
        end,
    {DeadUser, WasComment, AfterNextNL}.

get_line_at(Pos, Msg) ->
    Left = string:left(Msg, Pos - 1),
    Right = lists:nthtail(Pos - 1, Msg),
    NotNlF = fun($\n) -> false; (_) -> true end,
    LeftLine = ?lrev(lists:takewhile(NotNlF, ?lrev(Left))),
    RightLine = lists:takewhile(NotNlF, Right),
    LeftLine ++ RightLine.

-spec add_death(#death{}, #mafia_game{} | #mafia_day{})
               -> NewDeaths :: [#death{}].
add_death(D, G=#mafia_game{})->
    Deaths = G#mafia_game.player_deaths,
    add_deathI(D, Deaths);
add_death(D, Day=#mafia_day{})->
    Deaths = Day#mafia_day.player_deaths,
    add_deathI(D, Deaths).

add_deathI(D, Deaths) ->
    case lists:keyfind(D#death.player, #death.player, Deaths) of
        false -> [D | Deaths];
        D2 ->
            D3 = if D#death.comment /= ?undefined -> D;
                    true ->
                         %% remove delete marking on D2
                         D2#death{is_deleted = false}
                 end,
            lists:keyreplace(D#death.player, #death.player, Deaths, D3)
    end.

-spec is_end_of_phase(M :: #message{}, G :: #mafia_game{})
                     -> {IsEnd :: boolean(), phase()}.
is_end_of_phase(M, G) ->
    TimeMsg = M#message.time,
    PhaseMsg = mafia_time:calculate_phase(G, TimeMsg),
    Time10m = TimeMsg - ?MAX_GM_DL_MINS * ?MinuteSecs,
    Phase10m = mafia_time:calculate_phase(G, Time10m),
    IsEnd = PhaseMsg /= Phase10m,
    {IsEnd, Phase10m}.

%% In case someone votes before GM annouce dead, the day record
%% will have too many remaining players
update_day_rec(M, G, Death) ->
    TimeMsg = M#message.time,
    case mafia_time:calculate_phase(G, TimeMsg) of
        {DayNum, ?day} -> %% New day appears with new day record.
            [D] = ?rday(G, DayNum),
            case Death of
                #death{is_end = true} ->
                    NewRems = D#mafia_day.players_rem -- [Death#death.player],
                    mnesia:dirty_write(
                      D#mafia_day{players_rem = NewRems});
                #death{is_end = false} ->
                    %% For the "Jamie" case  when Vig kills in mid day
                    NewDeaths = add_death(Death, D),
                    NewRems = D#mafia_day.players_rem -- [Death#death.player],
                    mnesia:dirty_write(
                      D#mafia_day{players_rem = NewRems,
                                  player_deaths = NewDeaths})
            end;
        _ -> ok
    end.

is_last_non_letter(HStr) ->
    is_first_non_letter(?lrev(HStr)).

is_first_non_letter([]) -> true;
is_first_non_letter([H|_]) ->
    lists:member(H, " ,.;:!\"#€%7&/()=+?´`<>-_\t\r\n").

check_for_votes(_S, M, G) ->
    verify_user(M),
    Msg = ?b2l(M#message.message),
    MsgUC = string:to_upper(Msg),
    Players = G#mafia_game.players_rem,
    Players2 = add_nolynch_and_aliases(Players),
    VoteStr = "##VOTE",
    UnvoteStr = "##UNVOTE",
    case {mafia_data:rm_to_after_pos(MsgUC, VoteStr),
          string:str(MsgUC, UnvoteStr)} of
        {{0, ""}, 0} -> ignore;
        {{0, ""}, _} ->
            Vote = ?l2b(?Unvote),
            reg_vote(M, G, Vote, Vote, true);
        {{Pos, RestUC}, _} ->
            RawVote =
                ?l2b(string:strip(
                      string:left(
                        mafia_print:html2txt(
                          mafia_data:get_after_pos(
                            Pos, length(VoteStr), Msg)),
                        60))),
            case rank_options(Players2, RestUC) of
                [{NumV, TopP}] when NumV >= 2; NumV >= length(TopP) ->
                    reg_vote(M, G, TopP, RawVote, true);
                [{NumV1, TopP}, {NumV2, _}|_]
                  when NumV1 > NumV2 andalso
                       (NumV1 >= 2 orelse
                        NumV1 >= length(TopP)) ->
                    reg_vote(M, G, TopP, RawVote, true);
                _ ->
                    Vote = ?l2b("-"),
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
            case ?rday(M#message.thread_id, DayNum) of
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
    CheckRes = check_user(User),
    case CheckRes of
        ok -> ok;
        {user_new, UserRec} ->
            io:format(
              "~s Warning: created new user ~p\n",
              [mafia_print:print_time(M#message.time, short), User]),
            mnesia:dirty_write(UserRec);
        {user_game, _UserRec} ->
            auto_correct_case(?b2l(User), M#message.thread_id);
        {user, UserRec} ->
            mnesia:dirty_write(UserRec)
    end.

-spec print_verify_user(string()) -> ok.
print_verify_user(User) ->
    CheckRes = check_user(?l2b(User)),
    io:format("User name \"~s\" ", [User]),
    case CheckRes of
        ok -> io:format("is ok\n", []);
        {user, _} -> io:format("is unverified\n", []);
        {user_game, _} -> io:format("has wrong case\n", []);
        {user_new, _} -> io:format("does not exist\n", [])
    end.

-spec check_user(User :: user())
                -> ok |                   % found ok
                   {user, #user{}} |      % found unverified
                   {user_game, #user{}} | % wrong case
                   {user_new, #user{}}.   % did not find
check_user(User) ->
    UserU = ?b2ub(User),
    case mnesia:dirty_read(user, UserU) of
        [#user{verification_status = ?verified}] ->
            ok;
        [#user{name = User} = U] -> %% found unverified user
            {user, U#user{verification_status = ?verified}};
        [#user{} = U] ->  %% found wrong case
            {user_game,
             U#user{name = User,
                    verification_status = ?verified}};
        [] -> %% user not found
            {user_new, #user{name_upper = ?b2ub(User),
                             name = User,
                             aliases = [],
                             verification_status = ?verified}}
    end.

%% -----------------------------------------------------------------------------

auto_correct_case(CcUser, GId) when is_integer(GId) ->
    case ?rgame(GId) of
        [] -> ok;
        [G] -> auto_correct_case(CcUser, G)
    end;
auto_correct_case(CcUser, G = #mafia_game{}) ->
    io:format("Correcting case for user ~p in game M~p\n",
              [CcUser, G#mafia_game.game_num]),
    CorrectF = correct_case_fun2(),
    PsOrigL = [?b2l(P) || P <- G#mafia_game.players_orig],
    PsOrigL2 = [CorrectF(CcUser, P) || P <- PsOrigL],
    if PsOrigL2 /= PsOrigL ->
            PsOrigB = [?l2b(P) || P <- PsOrigL2],
            PsRemL = [?b2l(P) || P <- G#mafia_game.players_rem],
            PsRemL2 = [CorrectF(CcUser, P) || P <- PsRemL],
            if PsRemL2 /= PsRemL ->
                    PsRemB = [?l2b(P) || P <- PsRemL2],
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

-spec player_type(#message{}, #mafia_game{})
                 -> ?gm | ?player | ?dead_player | ?other.
player_type(M, G) ->
    UserB = M#message.user_name,
    case is_user_in_list(UserB, G#mafia_game.gms) of
        true -> ?gm;
        false ->
            case is_user_in_list(UserB, G#mafia_game.players_rem) of
                true -> ?player;
                false ->
                    case is_user_in_list(UserB, G#mafia_game.players_orig) of
                        true -> ?dead_player;
                        false -> ?other
                    end
            end
    end.

is_user_in_list(UserB, UsersB) ->
    User = ?b2l(UserB),
    UserU = string:to_upper(User),
    UsersU = [?b2ul(U) || U <- UsersB],
    lists:member(UserU, UsersU).

%% -----------------------------------------------------------------------------

add_nolynch_and_aliases(Players) ->
    AddAlias =
        fun(P, Acc) ->
                case mnesia:dirty_read(user, ?b2ub(P)) of
                    [#user{aliases = AliasesB}] when AliasesB /= [] ->
                        Acc ++ [{?b2l(P), ?b2l(A)} || A <- AliasesB];
                    _ -> Acc
                end
        end,
    Aliases = lists:foldl(AddAlias, [], Players),
    %% io:format("aliases ~p\n", [Aliases]),
    [?b2l(P) || P <- Players] ++ ?Extra ++ Aliases.

%% -----------------------------------------------------------------------------

rank_options(Players, RestUC) ->
    RestUCW = waste_spaces(RestUC),
    F = fun(P) ->
                PorA = select_alias(P),
                PlayerUCW = string:to_upper(waste_spaces(PorA)),
                r_count(PlayerUCW, RestUCW, 0)
        end,
    ?lrev(lists:sort([{F(P), ?l2b(select_name(P))} || P <- Players])).

%% Return {MatchPos, PreMatchStr, PostMatchStr}
find_parts(Str, Search) ->
    find_parts_I(Str, Search, no_match).

%% Return {MatchPos, PreMatchStr, MatchStr, PostMatchStr}
find_parts_match(Str, Search) ->
    find_parts_I(Str, Search, w_match).

find_parts_I(Str, Search, Mode) ->
    StrU = ?l2u(Str),
    SearchU = ?l2u(Search),
    find_parts_reply(Mode,
                     string:str(StrU, SearchU),
                     Str,
                     Search).

find_parts_reply(no_match, 0, Str, _Search) ->
    {0, Str, ""};
find_parts_reply(w_match, 0, Str, _Search) ->
    {0, Str, "", ""};
find_parts_reply(no_match, P, Str, Search) ->
    {P,
     string:left(Str, P - 1),
     mafia_data:get_after_pos(P, length(Search), Str)};
find_parts_reply(w_match, P, Str, Search) ->
    {P,
     string:left(Str, P - 1),
     string:substr(Str, P, length(Search)),
     mafia_data:get_after_pos(P, length(Search), Str)}.

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
    %% io:format("REGVOTE ~p, ~p\n",[M#message.user_name, Vote]),
    case is_remaining_player(
           M#message.user_name,
           G#mafia_game.players_rem) of
        true ->
            case ?b2l(Vote) of
                ?END -> reg_end_vote(add, M);
                ?UNEND -> reg_end_vote(remove, M);
                _ ->
                    vote2(M, G, Vote, RawVote, IsOkVote)
            end;
        false ->
            io:format("~s Warning ~s tried to vote in game\n",
                      [mafia_print:print_time(M#message.time, short),
                       ?b2l(M#message.user_name)]),
            ignore
    end.


-spec is_remaining_player(User :: player(),
                          Remain :: [player()]) -> boolean().
is_remaining_player(User, Rem) ->
    UserL = ?b2l(User),
    UserU = string:to_upper(UserL),
    RemainsU = [?b2ul(R) || R <- Rem],
    lists:member(UserU, RemainsU).

vote2(M, G, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        {DayNum, ?day} ->
            io:format(
              "~s Register Vote: ~s votes ~p ~s\n",
              [mafia_print:print_time(M#message.time, short),
               ?b2l(M#message.user_name), ?b2l(RawVote),
               if IsOkVote -> "Approved"; true -> "Rejected" end]),
            User = M#message.user_name,
            NewVote = #vote{time = M#message.time,
                            id = M#message.msg_id,
                            page = M#message.page_num,
                            vote = Vote,
                            raw = RawVote,
                            valid = IsOkVote
                           },
            Day = hd(?rday(M#message.thread_id, DayNum)),
            Votes = Day#mafia_day.votes,
            Votes2 = case lists:keyfind(User, 1, Votes) of
                         false ->
                             [{User, [NewVote]} | Votes];
                         {User, UVotes} ->
                             UVotes2 =
                                 lists:keystore(NewVote#vote.id,
                                                #vote.id,
                                                UVotes,
                                                NewVote),
                             lists:keystore(User, 1, Votes, {User, UVotes2})
                     end,
            mnesia:dirty_write(Day#mafia_day{votes = Votes2});
        _ ->
            ignore
    end.
