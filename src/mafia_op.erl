-module(mafia_op).

-export([kill_player/4,
         set_death_msgid/5,
         replace_player/4,
         resurrect_player/3
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
            MsgPhase = mafia_time:calculate_phase(G, M#message.time),
            if DeathPhase#phase.num == OldPhase#phase.num,
               DeathPhase /= MsgPhase ->
                    ?regen_history(update_death, M, {G2, DeathPhase});
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
resurrect_player(G, M, PlayerB) ->
    %% ##RESURRECT PLAYER
    case lists:partition(fun(#death{player = P}) -> P == PlayerB;
                            (_) -> ?false
                         end,
                         G#mafia_game.player_deaths) of
        {[], _} ->
            not_found;
        {[#death{msg_key = DMK, time = DTime}], NewDeaths} ->
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
                    ?regen_history(set_death_msgid, DeathMsg, {G2, DeathPhase});
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
