-module(mafia_op).

-export([kill_player/4,
         set_death_msgid/5,
         replace_player/4
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec kill_player(#mafia_game{}, #message{}, user(), string())
                 -> {Resp :: term(), #mafia_game{}}.
kill_player(G, M, DeadB, DeathComment) ->
    IsMember = lists:member(DeadB, G#mafia_game.players_rem),
    kill_player(G, M, DeadB, DeathComment, IsMember).

%% Remaining player. Kill him/her..
kill_player(G, M, DeadB, DeathComment, true) ->
    %% remove player from _rem lists.
    NewRems = G#mafia_game.players_rem -- [DeadB],
    io:format("~s Player ~s died\n",
              [mafia_print:print_time(M#message.time, short),
               ?b2l(DeadB)]),
    {IsEnd, DeathPhase} = is_end_of_phase(M, G),
    Death = #death{player = DeadB,
                   is_end = IsEnd,
                   phase = DeathPhase,
                   msg_key = M#message.msg_key,
                   time = M#message.time,
                   comment = ?l2b(DeathComment),
                   is_deleted = false
                  },
    NewDeaths = add_modify_deaths(Death, G),
    G2 = G#mafia_game{players_rem = NewRems,
                      player_deaths = NewDeaths},
    ?dwrite_game(game_v1, G2),
    {{ok, DeathPhase}, G2};
%% Not remaining. Do update if already dead
kill_player(G, M, DeadB, DeathComment, false) ->
    %% check in death records (allow edit of text and msgid)
    Deaths = [D || D = #death{player = P} <- G#mafia_game.player_deaths,
                  P == DeadB],
    case Deaths of
        [D] ->
            io:format(
              "~s Update death for player ~s\n",
              [mafia_print:print_time(M#message.time, short), ?b2l(DeadB)]),
            {IsEnd, DeathPhase} = is_end_of_phase(M, G),
            OldPhase = D#death.phase,
            Death = D#death{msg_key = M#message.msg_key,
                            time = M#message.time,
                            phase = DeathPhase,
                            is_end = IsEnd,
                            comment = ?l2b(DeathComment)},
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            ?dwrite_game(game_v2, G2),
            if DeathPhase#phase.num == OldPhase#phase.num ->
                    game:regen_history(M, {G2, DeathPhase});
               ?true -> ok
            end,
            {{ok, DeathPhase}, G2};
        _ ->
            {not_remaining_player, G}
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
set_death_msgid(_G, _M, _, [], _) ->
    {?error, msg_no_exist};
set_death_msgid(G, M, DeadB, [DeathMsg], DeathComment) ->
    Deaths = [D || D = #death{player = P} <- G#mafia_game.player_deaths,
                   P == DeadB],
    case Deaths of
        [D] ->
            io:format(
              "~s Set death msg id for player ~s\n",
              [mafia_print:print_time(M#message.time, short), ?b2l(DeadB)]),
            {IsEnd, DeathPhase} = is_end_of_phase(DeathMsg, G),
            io:format("~p\n", [{IsEnd, DeathPhase}]),
            OldPhase = D#death.phase,
            Death = D#death{msg_key = DeathMsg#message.msg_key,
                            time = DeathMsg#message.time,
                            phase = DeathPhase,
                            is_end = IsEnd,
                            comment = ?l2b(DeathComment)
                           },
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            if DeathPhase#phase.num == OldPhase#phase.num ->
                    game:regen_history(DeathMsg, {G2, DeathPhase});
               ?true -> ok
            end,
            ok;
        _ ->
            {?error, player_not_dead}
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-type replace_result() :: ok | old_no_exists | not_remain | ?game_ended.
-spec replace_player(#mafia_game{},
                     #message{},
                     string(),
                     string())
                    -> {replace_result(), #mafia_game{}}.
replace_player(G, M, NewPlayer, OldPlayer) ->
    Site = G#mafia_game.site,
    replace1(G, M,
             ?ruserUB(NewPlayer, Site),
             ?ruserUB(OldPlayer, Site)).

replace1(G, _M, _NewPlayer, []) -> {old_no_exists, G};
replace1(G, _M, [], _OldPlayer) -> {new_no_exists, G};
replace1(G, M, [New], [Old]) ->
    replace2(G, M, New, Old,
             lists:member(?e1(Old#user.name), G#mafia_game.players_rem)).

replace2(G, _M, _New, _Old, false) ->
    {not_remain, G};
replace2(G, M, New, Old, true) ->
    replace3(G, M, New, Old).

-spec replace3(#mafia_game{}, #message{}, #user{}, #user{}) ->
                      {ok, #mafia_game{}} | {?game_ended, #mafia_game{}}.
replace3(G, M, New, Old) ->
    Phase = mafia_time:calculate_phase(G, M#message.time),
    if Phase#phase.ptype == ?game_ended ->
            {?game_ended, G};
       true ->
            NewP = ?e1(New#user.name),
            OldP = ?e1(Old#user.name),
            ?dbg(M#message.time, {?b2l(NewP), replaces, ?b2l(OldP)}),
            Replacement = #replacement{
              new_player = NewP,
              replaced_player = OldP,
              phase = Phase,
              msg_key = M#message.msg_key,
              time = M#message.time
             },
            DeathsG2 = [Replacement | G#mafia_game.player_deaths],
            G2 = G#mafia_game{player_deaths = DeathsG2},
            replace4(G2, OldP, NewP)
    end.

-spec replace4(#mafia_game{}, term(), term()) -> {ok, #mafia_game{}}.
replace4(G, OldUB, NewUB) ->
    NewRem = repl_user(OldUB, NewUB, G#mafia_game.players_rem),
    G2 = G#mafia_game{players_rem = NewRem},
    ?dwrite_game(game_v4, G2),
    {ok, G2}.

repl_user(OldUB, NewUB, Users) ->
    R = fun(U) when U == OldUB -> NewUB;
           (U) -> U
        end,
    [R(U) || U <- Users].

%% -----------------------------------------------------------------------------
%% Internal help funs
%% -----------------------------------------------------------------------------
-spec is_end_of_phase(M :: #message{}, G :: #mafia_game{})
                     -> {IsEnd :: boolean(), #phase{}}.
is_end_of_phase(M, G) ->
    TimeMsg = M#message.time,
    PhaseMsg = mafia_time:calculate_phase(G, TimeMsg),
    Time20m = TimeMsg - ?MAX_GM_DL_SECS,
    Phase20m = mafia_time:calculate_phase(G, Time20m),
    IsEnd = PhaseMsg /= Phase20m,
    {IsEnd, Phase20m}.

%% -----------------------------------------------------------------------------

-spec add_modify_deaths(#death{},
                        #mafia_game{} | #mafia_day{}
                       )
                       -> NewDeaths :: [#death{} | #replacement{}].
add_modify_deaths(D, G = #mafia_game{}) ->
    Deaths = G#mafia_game.player_deaths,
    add_deathI(D, Deaths).

add_deathI(D, Deaths) ->
    Match = fun(#death{player = P}) -> P == D#death.player;
               (_) -> false
            end,
    case find(Match, Deaths) of
        false -> [D | Deaths];
        D2 ->
            D3 = if D#death.comment /= ?undefined -> D;
                    true ->
                         %% remove delete marking on D2
                         D2#death{is_deleted = false}
                 end,
            mafia_lib:replace_p(Match, Deaths, D3)
    end.

%% -----------------------------------------------------------------------------
%% @doc Find matching element or false
-spec find(MatchF :: function(), list()) -> false | any().
find(MatchF, List) ->
    case lists:dropwhile(fun(E) -> not MatchF(E) end, List) of
        [] -> false;
        [MatchElement | _] -> MatchElement
    end.
