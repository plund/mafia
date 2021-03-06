-module(mafia_op).

-export([kill_player/4,
         resurrect_player/3,
         remove_player/2,
         add_gm/3,
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
    case death_status(G, M, DeadB) of
        alive_and_kicking ->
            %% Remaining player. Kill him/her..
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
                           comment = unicode:characters_to_binary(DeathComment),
                           is_deleted = false
                          },
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{players_rem = NewRems,
                              player_deaths = NewDeaths},
            ?dwrite_game(game_v1, G2),
            MsgPhase = mafia_time:calculate_phase(G, M#message.time),
            if DeathPhase /= MsgPhase ->
                    ?regen_history(update_death, M, {G2, DeathPhase});
               ?true -> ok
            end,
            {{ok, DeathPhase}, G2};

        {Res, D} when Res == dead_already; Res == not_dead_yet ->
            %% Not remaining. Do update if already dead
            io:format("~s Update death for player ~s\n",
                      [mafia_print:print_time(M#message.time, short),
                       ?b2l(DeadB)]),
            D2 = D#death{comment = unicode:characters_to_binary(DeathComment)},
            Death =
                if Res == not_dead_yet ->
                        %% moving death to earlier message
                        {IsEnd, DeathPhase} = is_end_of_phase(M, G),
                        D2#death{msg_key = M#message.msg_key,
                                 time = M#message.time,
                                 phase = DeathPhase,
                                 is_end = IsEnd};
                   true -> D2
                end,
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            ?dwrite_game(game_v2, G2),
            {{ok, Death#death.phase}, G2};
        not_player_in_game ->
            {not_remaining_player, G}
    end.

death_status(G, #message{msg_key = MsgKey}, PlayerB) ->
    death_status(G, MsgKey, PlayerB);
death_status(G, MsgKey, PlayerB) ->
    IsRemaining = lists:member(PlayerB, G#mafia_game.players_rem),
    Deaths = [D || D = #death{player = P} <- G#mafia_game.player_deaths,
                   P == PlayerB],
    case Deaths of
        _ when IsRemaining -> alive_and_kicking;
        [D = #death{msg_key = DeathMsgKey}] when MsgKey < DeathMsgKey ->
            {not_dead_yet, D};
        [D = #death{}] ->
            {dead_already, D};
        [] -> not_player_in_game
    end.

%% -----------------------------------------------------------------------------
%% @doc ##RESURRECT PLAYER
%% @end
%% -----------------------------------------------------------------------------
resurrect_player(G, M, PlayerU) ->
    resurrect_player2(G, M, PlayerU, ?ruserUB(PlayerU, G#mafia_game.site)).

resurrect_player2(_, _, PlayerU, []) ->
    ?dbg({?error, {PlayerU, does_not_exist}});
resurrect_player2(G, M, _, [#user{name = {PlayerB, _}}]) ->
    case lists:partition(fun(#death{player = P}) -> P == PlayerB;
                            (_) -> ?false
                         end,
                         G#mafia_game.player_deaths) of
        {[], _} ->
            ?dbg({?error, {PlayerB, not_dead}});
        {[#death{msg_key = DMK, time = DTime}], NewDeaths} ->
            ?dbg({resurrect, PlayerB, M#message.msg_key}),
            NewRems =
                mafia_lib:to_bin_sort([PlayerB | G#mafia_game.players_rem]),
            %% reinsert PLAYER into #mafia_game.player_rem
            %% remove #death in #mafia_game.player_deaths
            G2 = G#mafia_game{players_rem = NewRems,
                              player_deaths = NewDeaths},
            ?dwrite_game(game_v5, G2),
            MTime = M#message.time,
            Site = G#mafia_game.site,
            ThId = G#mafia_game.thread_id,
            %% Remove pages with too old messages
            PageFilter =
                fun(PNum) ->
                        case ?rpage(ThId, PNum, Site) of
                            [#page_rec{message_ids = MsgIds}] ->
                                case ?rmess({lists:last(MsgIds), Site}) of
                                    [#message{time = PMsgTime}] ->
                                        PMsgTime > DTime;
                                    _ -> false
                                end
                        end
                end,
            %% Replay all messages after DTime
            Gtmp = G#mafia_game{page_to_read = 1,
                                last_msg_id = element(1, DMK),
                                last_msg_time = DTime},
            {MsgIdFun0, Acc} = mafia_data:checkvote_fun(Gtmp, false),
            MsgIdFun =
                fun(MsgId, Acc2) ->
                        case ?rmess({MsgId, Site}) of
                            [#message{msg_key = MK, time = T} = M2]
                              when MK /= M#message.msg_key,
                                   MK /= DMK,
                                   DTime =< T ->
                                case mafia_vote:player_type(M2, G2) of
                                    ?player -> MsgIdFun0(MsgId, Acc2);
                                    _ -> Acc2
                                end;
                            _ ->
                                Acc2
                        end
                end,
            mafia_lib:iter_msgids({ThId, Site}, MsgIdFun, Acc, PageFilter),

            %% for phases prior to the current phase we should regen the history
            %% from DTime to MTime
            RegenPhases = mafia_time:game_phases(G2, DTime, MTime),
            [begin
                 ?dbg({replay_user, Phase}),
                 game_gen:regenerate_history_phase(G2#mafia_game.game_num,
                                                   Phase)
             end
             || Phase <- RegenPhases],
            {ok, G2}
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
remove_player(G, PlayerB) ->
    #mafia_game{players_rem = PsRem, players_orig = PsOrig} = G,
    G2 = G#mafia_game{players_rem = PsRem -- [PlayerB],
                      players_orig = PsOrig -- [PlayerB]},
    if G2 /= G ->
            ?dwrite_game(G2),
            ok;
       true -> {?error, game_not_updated}
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
add_gm(G, M, UserB) ->
    IsDeathStatusOk =
        case death_status(G, M, UserB) of
            {dead_already, _} -> true;
            not_player_in_game -> true;
            _ -> false
        end,
    case {lists:member(UserB, G#mafia_game.gms), IsDeathStatusOk} of
        {false, true} ->
            G2 = G#mafia_game{gms = G#mafia_game.gms ++ [UserB]},
            ?dwrite_game(G2),
            ok;
        {true, _} -> {?error, user_is_gm};
        {_, false} -> {?error, user_is_player_still}
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
            Death = D#death{msg_key = DeathMsg#message.msg_key,
                            time = DeathMsg#message.time,
                            phase = DeathPhase,
                            is_end = IsEnd,
                            comment = unicode:characters_to_binary(DeathComment)
                           },
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            ?dwrite_game(G2),
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
    Rem2 = G#mafia_game.players_rem -- [OldUB],
    NewRems = mafia_lib:to_bin_sort([NewUB | Rem2]),
    %% NewRems = repl_user(OldUB, NewUB, G#mafia_game.players_rem),
    G2 = G#mafia_game{players_rem = NewRems},
    ?dwrite_game(game_v4, G2),
    {ok, G2}.

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
                       ) -> NewDeaths :: [#death{} | #replacement{}].
add_modify_deaths(Death, G = #mafia_game{}) ->
    Deaths = G#mafia_game.player_deaths,
    Deaths1 = [D || D = #death{} <- Deaths],
    Deaths2 =
        case lists:keyfind(Death#death.player, #death.player, Deaths1) of
            false -> [Death | Deaths];
            _ ->
                Match = fun(#death{player = Player}) ->
                                Player == Death#death.player;
                           (_) -> false
                        end,
                mafia_lib:replace_p(Match, Deaths, Death)
        end,
    lists:sort(fun(A, B) -> msg_key(B) =< msg_key(A) end, Deaths2).

msg_key(D = #death{}) -> D#death.msg_key;
msg_key(R = #replacement{}) -> R#replacement.msg_key.
