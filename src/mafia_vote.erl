-module(mafia_vote).

-export([get_regexs/0,
         check_cmds_votes/3,

         kill_player/4,
         set_death_msgid/5,
         replace_player/4,

         find_parts/2
        ]).

-include("mafia.hrl").

-record(regex,
        {pos,
         msg_text_u,
         msg_text,
         match,
         pre_match,
         pre_match_u,
         play_repl,
         game_end,
         game_unend
        }).

%% -----------------------------------------------------------------------------
%% Get compiled regexs
%% -----------------------------------------------------------------------------
get_regexs() ->
    #regex{play_repl = regex_player_replacement(),
           game_end = regex_game_end(),
           game_unend = regex_game_unend()
          }.

%% -----------------------------------------------------------------------------
%% Returns ignore when #message or #mafia_game cannot be found
%% -----------------------------------------------------------------------------
-spec check_cmds_votes(#mafia_game{},
                       #regex{}, #message{})
                      -> MsgTime :: seconds1970().
check_cmds_votes(G = #mafia_game{}, Re, M = #message{}) ->
    mafia_data:update_stat(G, M),
    Msg = mafia_print:html2txt(?b2l(M#message.message)),
    MsgU = ?l2u(Msg),
    Re2 = Re#regex{msg_text_u = MsgU, msg_text = Msg},
    check_cmds_votes2(G, Re2, M).

check_cmds_votes2(G, Re, M) ->
    IsEnded = case G#mafia_game.game_end of
                  ?undefined -> false;
                  {EndTime, _MsgId} ->
                      M#message.time >= EndTime
              end,
    PhaseM = mafia_time:calculate_phase(G, M#message.time),
    IsStarted = PhaseM#phase.don /= ?game_start,
    DoGenerate =
        case mafia_lib:prev_msg(M) of
            ?none -> false;
            PrevM ->
                PhasePrevM = mafia_time:calculate_phase(G, PrevM#message.time),
                PhasePrevM /= PhaseM
                    andalso PhasePrevM#phase.don /= ?game_start
        end,
    if DoGenerate ->
            mafia_web:regen_history(M, G);
       true -> ok
    end,
    if not IsStarted ->
            case player_type(M, G) of
                ?gm -> check_for_gm_cmds(Re, M, G, true);
                UserType -> log_unallowed_msg(UserType, M)
            end;
       not IsEnded ->
            case player_type(M, G) of
                ?gm -> check_for_gm_cmds(Re, M, G, DoGenerate);
                ?player -> check_for_votes(Re, M, G);
                ?dead_player -> log_unallowed_msg(?dead_player, M);
                ?other -> log_unallowed_msg(?other, M)
            end;
       IsEnded ->
            case player_type(M, G) of
                ?gm ->
                    G2 = check_for_deaths(Re, M, G),
                    check_for_game_unend(Re, M, G2);
                _ -> ignore
            end
    end,
    M#message.time.

log_unallowed_msg(Type, M) ->
    MTime = M#message.time,
    User = ?b2l(M#message.user_name),
    MsgId = M#message.msg_id,
    ?dbg(MTime, {Type, sent_message, MsgId, User}).

%% Removes player from Game if dead
check_for_gm_cmds(Re, M, G, DoGenerate) ->
    G3 = check_for_early_end(Re, M#message.time, G),
    G4 = check_for_deadline_move(Re, M, G3),
    G5 = check_for_player_replacement(Re, M, G4),
    G6 = check_for_game_end(Re, M, G5),
    G7 = check_for_deaths(Re, M, G6),

    %% if time is 0 - 20 min after a deadline generate a history page
    {RelTimeSecs, _DL} = mafia_time:nearest_deadline(G7, M#message.time),
    if not DoGenerate,
       G7 /= G6, %% someone died
       RelTimeSecs >= 0,
       RelTimeSecs =< ?MAX_GM_DL_MINS * ?MinuteSecs ->
            mafia_web:regen_history(M, G7);
       true ->
            ok
    end,
    G7.

check_for_deaths(Reg = #regex{}, M, G) ->
    %% find "has died" on line
    SearchU1 = "DIED",
    SearchU2 = "DEAD",
    SearchU3 = "BEEN LYNCHED",
    case {regex_find(SearchU1, Reg),
          regex_find(SearchU2, Reg),
          regex_find(SearchU3, Reg)} of
        {{?nomatch, _}, {?nomatch, _}, {?nomatch, _}} -> %% no-one has died
            G;
        Matches ->
            Rs = [ReRes || {MRes, ReRes} <- tuple_to_list(Matches),
                           MRes == ?match],
            Reg2 = lists:min(Rs),
            ?dbg(M#message.time, {msgid_pos, M#message.msg_id, Reg2#regex.pos}),
            {KilledUserB, DeathComment} =
                read_death_line(G, Reg#regex.msg_text, Reg2),
            case KilledUserB of
                ?noone_died -> %% no match
                    check_for_deaths(Reg2, M, G);
                _ ->
                    {_, G2} = kill_player(G, M, KilledUserB, DeathComment),
                    check_for_deaths(Reg2, M, G2)
            end
    end.

%% Returns exact User binary.
-spec read_death_line(#mafia_game{}, string(), #regex{})
                     -> {user() | ?noone_died,
                         Comment :: ?use_full_line | string()}.
read_death_line(G, MsgLong, Reg) ->
    %% find one remaining player before on the same line.
    %% Extend the search list with already dead players to make an update
    %% of the death comment possible.
    Deaths = [D#death.player || D = #death{} <- G#mafia_game.player_deaths],
    LookForUsers = G#mafia_game.players_rem ++ Deaths,
    PreLineU = pre_to_nl(Reg#regex.pre_match_u),
    Users =
        lists:dropwhile(
          fun(User) ->
                  case find_parts(PreLineU, ?b2ul(User)) of
                      {0, _, _} -> true;
                      {_, HStr2, TStr2} ->
                          not is_last_non_letter(HStr2) orelse
                              not is_first_non_letter(TStr2)
                  end
          end,
          LookForUsers
         ),
    DeadPlayer =
        case Users of
            [First | _] ->
                First;
            [] ->
                ?noone_died
        end,
    {_, Reg2} = regex_find("\n", Reg), %% Get text to/before next new-line
    WasComment =
        case {regex_pre_find("SHE WAS", Reg2),
              regex_pre_find("HE WAS", Reg2)} of
            {{?nomatch, _}, {?nomatch, _}} -> % Neither found use full line
                get_line_at(Reg#regex.pos, MsgLong);
            {Find1, Find2} ->
                {?match, #regex{match = Match, msg_text = After}} =
                    if element(1, Find1) == ?match -> Find1;
                       true -> Find2
                    end,
                Match ++ After
        end,
    {DeadPlayer, WasComment}.

-spec kill_player(#mafia_game{}, #message{}, user(), string())
                 -> {Resp :: term(), #mafia_game{}}.
kill_player(G, M, DeadB, DeathComment) ->
    IsMember = lists:member(DeadB, G#mafia_game.players_rem),
    kill_player(G, M, DeadB, DeathComment, IsMember).

%% Remaining player. Kill him/her..
kill_player(G, M, DeadB, DeathComment, true) ->
    %% remove player from _rem lists.
    NewRems = G#mafia_game.players_rem -- [DeadB],
    io:format(
      "~s Player ~s died\n",
      [mafia_print:print_time(M#message.time, short), ?b2l(DeadB)]),
    {IsEnd, DeathPhase} = is_end_of_phase(M, G),
    Death = #death{player = DeadB,
                   is_end = IsEnd,
                   phase = DeathPhase,
                   msg_id = M#message.msg_id,
                   time = M#message.time,
                   comment = ?l2b(DeathComment),
                   is_deleted = false
                  },
    NewDeaths = add_modify_deaths(Death, G),
    update_day_rec(G, M, Death),
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
            Death = D#death{msg_id = M#message.msg_id,
                            time = M#message.time,
                            phase = DeathPhase,
                            is_end = IsEnd,
                            comment = ?l2b(DeathComment)},
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            ?dwrite_game(game_v2, G2),
            if DeathPhase#phase.num /= OldPhase#phase.num ->
                    %% TODO / FIXME
                    ?dbg(M#message.time,
                         "WARNING: Death moved between day records "
                         "Not fixed! Do refresh_votes()");
               true ->
                    update_day_rec(G2, M, Death),
                    mafia_web:regen_history(M, {G2, DeathPhase})
            end,
            {{ok, DeathPhase}, G2};
        _ ->
            {not_remaining_player, G}
    end.

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
            Death = D#death{msg_id = DeathMsg#message.msg_id,
                            time = DeathMsg#message.time,
                            phase = DeathPhase,
                            is_end = IsEnd,
                            comment = ?l2b(DeathComment)
                           },
            NewDeaths = add_modify_deaths(Death, G),
            G2 = G#mafia_game{player_deaths = NewDeaths},
            ?dwrite_game(game_v3, G2),
            if DeathPhase#phase.num /= OldPhase#phase.num ->
                    %% TODO / FIXME
                    ?dbg(M#message.time,
                         "WARNING: Death moved between day records "
                         "Not fixed! Do refresh_votes()");
               true ->
                    update_day_rec(G2, M, Death),
                    mafia_web:regen_history(DeathMsg, {G2, DeathPhase})
            end,
            ok;
        _ ->
            {?error, player_not_dead}
    end.

%% return previous part of line stopping early at ".:;!"
pre_to_nl(HStrU) ->
    RevStr = ?lrev(HStrU),
    RevStr2 = lists:takewhile(fun(C) -> not lists:member(C, ".;:!\n") end,
                              RevStr),
    ?lrev(RevStr2).

get_line_at(Pos, Msg) ->
    Left = string:left(Msg, Pos - 1),
    Right = lists:nthtail(Pos - 1, Msg),
    NotNlF = fun($\n) -> false; (_) -> true end,
    LeftLine = ?lrev(lists:takewhile(NotNlF, ?lrev(Left))),
    RightLine = lists:takewhile(NotNlF, Right),
    LeftLine ++ RightLine.

-spec add_modify_deaths(#death{},
                        #mafia_game{} | #mafia_day{}
                       )
                       -> NewDeaths :: [#death{} | #replacement{}].
add_modify_deaths(D, G = #mafia_game{}) ->
    Deaths = G#mafia_game.player_deaths,
    add_deathI(D, Deaths);
add_modify_deaths(D, Day = #mafia_day{})->
    Deaths = Day#mafia_day.player_deaths,
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
            replace(Match, Deaths, D3)
    end.

%% mafia_lib?
%% @doc Find matching element or false
-spec find(MatchF :: function(), list()) -> false | any().
find(MatchF, List) ->
    case lists:dropwhile(fun(E) -> not MatchF(E) end, List) of
        [] -> false;
        [MatchElement | _] -> MatchElement
    end.

%% @doc Replace element in list if MatchF returns true
replace(MatchF, List, NewR) ->
    [case MatchF(E) of true -> NewR; false -> E end || E <- List].

-spec is_end_of_phase(M :: #message{}, G :: #mafia_game{})
                     -> {IsEnd :: boolean(), #phase{}}.
is_end_of_phase(M, G) ->
    TimeMsg = M#message.time,
    PhaseMsg = mafia_time:calculate_phase(G, TimeMsg),
    Time20m = TimeMsg - ?MAX_GM_DL_MINS * ?MinuteSecs,
    Phase20m = mafia_time:calculate_phase(G, Time20m),
    IsEnd = PhaseMsg /= Phase20m,
    {IsEnd, Phase20m}.

%% In case someone votes before GM annouce dead, the day record
%% will have too many remaining players
update_day_rec(G, M, Death) ->
    MsgPhase = mafia_time:calculate_phase(G, M#message.time),
    DeathPhase = Death#death.phase,
    IsSamePhase = MsgPhase == DeathPhase,
    Player = Death#death.player,
    RemPlayer =
        fun(D) ->
                NewRems = D#mafia_day.players_rem -- [Player],
                D#mafia_day{players_rem = NewRems}
        end,
    AddDeath =
        fun(D) ->
                NewDeaths = add_modify_deaths(Death, D),
                D#mafia_day{player_deaths = NewDeaths}
        end,
    MDay = ?rday(G, MsgPhase),
    if IsSamePhase ->
            MDay2 = RemPlayer(MDay),
            ?dwrite_day(AddDeath(MDay2));
       not IsSamePhase ->
            ?dwrite_day(RemPlayer(MDay)),
            DDay = ?rday(G, DeathPhase),
            DDay2 = RemPlayer(DDay),
            ?dwrite_day(AddDeath(DDay2))
    end.

is_last_non_letter(HStr) ->
    is_first_non_letter(?lrev(HStr)).

is_first_non_letter([]) -> true;
is_first_non_letter([H|_]) ->
    lists:member(H, " ,.;:!\"#€%&/()=+?^´`'*<>-_\t\r\n").

%% -----------------------------------------------------------------------------

check_for_early_end(#regex{msg_text_u = MsgText}, Time, G) ->
    case find_early_end(MsgText) of
        {?error, _} -> G;
        {ok, DoN} ->
            case mafia_time:calculate_phase(G, Time) of
                #phase{don = ?game_ended} ->
                    ?dbg(Time, "GM early end command for game that has ended"),
                    G;
                #phase{don = DoN} = Phase ->
                    mafia_time:end_phase(G, Phase, Time);
                _ ->
                    ?dbg(Time,
                         {"GM early end command with wrong phase type", DoN}),
                    G
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc ENDED EARLY GM commands must be on an own line to avoid GM misorders
%% Nothing After "ENDED EARLY" or before "DAY"/"NIGHT"
%% @end
%% -----------------------------------------------------------------------------
-spec find_early_end(string()) -> {ok, ?day | ?night} | {?error, atom()}.
find_early_end(MsgText) ->
    SearchU1 = "ENDED EARLY",
    case find_parts(MsgText, SearchU1) of
        {0, _, _} -> {?error, no_early_end};
        {_, HStr1, TStr1} ->
            %%_TStr1 no letters/numbers before end
            NoTxtBeforeNL =
                fun(Str) ->
                        Res =
                            lists:foldl(fun(C, no_nl_yet) when C > $\s -> false;
                                           (C, no_nl_yet) when C == $\n -> true;
                                           (_, St) -> St
                                        end,
                                        no_nl_yet,
                                        Str),
                        if Res == false -> false;
                           true -> true
                        end
                end,
            case NoTxtBeforeNL(TStr1) of
                true ->
                    SearchU2 = "DAY",
                    SearchU3 = "NIGHT",
                    case {find_parts(HStr1, SearchU2),
                          find_parts(HStr1, SearchU3)} of
                        {{0, _, _}, {0, _, _}} ->
                            {?error, no_phase_type};  %% no "DAY" or "NIGHT"
                        {Pos1, Pos2} ->
                            if element(1, Pos1) /= 0 ->
                                    HStr2 = element(2, Pos1),
                                    case NoTxtBeforeNL(?lrev(HStr2)) of
                                        true -> {ok, ?day};
                                        false -> {?error, no_early_end}
                                    end;
                               true ->
                                    HStr2 = element(2, Pos2),
                                    case NoTxtBeforeNL(?lrev(HStr2)) of
                                        true ->
                                            {ok, ?night};
                                        false -> {?error, no_early_end}
                                    end
                            end
                    end;
                false ->
                    {?error, no_early_end}
            end
    end.

%% -----------------------------------------------------------------------------

check_for_deadline_move(#regex{msg_text_u = MsgText}, M, G) ->
    G3 = case find_deadline_move(MsgText) of
             {found, DeltaSecs} ->
                 {_, G2} = mafia_time:move_next_deadline(G, M, DeltaSecs),
                 G2;
             not_found -> G;
             {?error, _} -> G
         end,
    G3.

%% DEADLINE ... MOVED 24H LATER
%% DEADLINE ... MOVED 24H EARLIER
-spec find_deadline_move(string())
                        -> {found, Secs :: integer()} |
                           not_found |
                           {?error, term()}.
find_deadline_move(MsgTextU) ->
    Reg = "DEADLINE(.*)MOVED(.*)(LATER|EARLIER)",
    case re:run(MsgTextU, Reg) of
        nomatch -> not_found;
        {match, Ms} ->
            [_, _, TimeStr, Dir] = mafia_lib:re_matches(MsgTextU, Ms),
            Sign = case Dir of "LATER" -> +1; "EARLIER" -> -1 end,
            case find_time(TimeStr) of
                {?error, _} = E -> E;
                {ok, D, H, M} ->
                    Int =
                        fun(I) when is_integer(I) -> I;
                           (_) -> 0
                        end,
                    DeltaSecs =
                        Sign * ((Int(D) * 24 + Int(H)) * 60 + Int(M)) * 60,
                    {found, DeltaSecs}
            end
    end.

find_time(Text) ->
    TextU = ?l2u(Text),
    RegDays = "^\\W+([0-9]+) *(DAYS?|D)",
    {NumD, RestD} = find_expr(TextU, RegDays),
    RegHours = "^\\W*([0-9]+) *(HOURS?|H)",
    {NumH, RestH} = find_expr(RestD, RegHours),
    RegMins = "^\\W*([0-9]+) *(MINUTES?|M)",
    {NumM, RestM} = find_expr(RestH, RegMins),
    RegEnd = "^\\W+$",
    case re:run(RestM, RegEnd) of
        nomatch -> {?error, bad_time};
        _ -> {ok, NumD, NumH, NumM}
    end.

find_expr(Text, Reg) ->
    case re:run(Text, Reg) of
        {match, Matches} ->
            {S, L} = hd(Matches),
            Rest = string:substr(Text, S + L + 1),
            [_, NumStr, _] = mafia_lib:re_matches(Text, Matches),
            {?l2i(NumStr), Rest};
        nomatch ->
            {not_found, Text}
    end.

-spec check_for_player_replacement(
        #regex{}, #message{}, #mafia_game{}
       ) -> #mafia_game{}.
check_for_player_replacement(Re, M, G) ->
    case string:str(Re#regex.msg_text_u, "REPLAC") of
        0 -> %% no-one has been replaced
            G;
        _ ->
            do_check_for_player_replacement(Re, M, G)
    end.

do_check_for_player_replacement(Re, M, G) ->
    case find_player_replacement(Re) of
        no_replace ->
            G;
        {replace, OldPlayer, NewPlayer, Re2} ->
            ?dbg(M#message.time, {replace_match, OldPlayer, NewPlayer}),
            case replace_player(G, M, NewPlayer, OldPlayer) of
                {ok, G2} -> G2;
                {Err, G2} ->
                    ?dbg(M#message.time, {replace, Err}),
                    G2
            end,
            do_check_for_player_replacement(Re2, M, G2)
    end.

regex_player_replacement() ->
    Reg = "(^|\\s)([^\\s].*[^\\s]) +(HAS +REPLACED|IS +REPLACING)"
        " +([^\\s].*[^\\s])(\\s|$)",
    %% fprof did not see any performance improvment with comiled regexs.
    element(2, re:compile(Reg)).

find_player_replacement(Reg = #regex{play_repl = RE}) ->
    case re:run(Reg#regex.msg_text_u, RE, [{capture, [2, 4, 5]}]) of
        nomatch ->
            no_replace;
        {match, [M1, M2, {End,_}]} ->
            Matches = [M1, M2],
            [NewPlayer, OldPlayer] =
                mafia_lib:re_matches(Reg#regex.msg_text, Matches),
            MsgU2 = lists:nthtail(End, Reg#regex.msg_text_u),
            Msg2 = lists:nthtail(End, Reg#regex.msg_text),
            Reg2 = Reg#regex{msg_text_u = MsgU2,
                             msg_text = Msg2},
            {replace, OldPlayer, NewPlayer, Reg2}
    end.

-type replace_result() :: ok | old_no_exists | not_remain | ?game_ended.
-spec replace_player(#mafia_game{},
                     #message{},
                     string(),
                     string())
                    -> {replace_result(), #mafia_game{}}.
replace_player(G, M, NewPlayer, OldPlayer) ->
    replace1(G, M, ?ruserUB(NewPlayer), ?ruserUB(OldPlayer)).

replace1(G, _M, _NewPlayer, []) -> {old_no_exists, G};
replace1(G, _M, [], _OldPlayer) -> {new_no_exists, G};
replace1(G, M, [New], [Old]) ->
    replace2(G, M, New, Old,
             lists:member(Old#user.name, G#mafia_game.players_rem)).

replace2(G, _M, _New, _Old, false) ->
    {not_remain, G};
replace2(G, M, New, Old, true) ->
    replace3(G, M, New, Old).

-spec replace3(#mafia_game{}, #message{}, #user{}, #user{}) ->
                      {ok, #mafia_game{}} | {?game_ended, #mafia_game{}}.
replace3(G, M, New, Old) ->
    %% replace ALSO in #mafia_day.players_rem
    Phase = mafia_time:calculate_phase(G, M#message.time),
    if Phase#phase.don == ?game_ended ->
            {?game_ended, G};
       true ->
            DayNum = if Phase#phase.don == ?game_start -> 1;
                        true -> Phase#phase.num
                     end,
            Day = ?rday(G, DayNum),
            NewP = New#user.name,
            OldP = Old#user.name,
            ?dbg(M#message.time, {?b2l(NewP), replaces, ?b2l(OldP)}),
            Rems2 = repl_user(OldP, NewP, Day#mafia_day.players_rem),
            Replacement = #replacement{
              new_player = NewP,
              replaced_player = OldP,
              phase = Phase,
              msg_id = M#message.msg_id,
              time = M#message.time
             },
            DeathsD2 = [Replacement | Day#mafia_day.player_deaths],
            ?dwrite_day(Day#mafia_day{players_rem = Rems2,
                                      player_deaths = DeathsD2}),
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

check_for_game_end(S, M, G) ->
    case string:str(S#regex.msg_text_u, "GAME") of
        0 ->
            G;
        _ ->
            case find_game_end(S) of
                nomatch ->
                    G;
                {match, _Ms} ->
                    {_Reply, G2} = mafia_time:end_game(M, G),
                    G2
            end
    end.

regex_game_end() ->
    Reg = "(^|\\W)GAME +((HAS +)?ENDED|IS +OVER)(\\W|$)",
    %% Reg = "^((.|\\s)*\\s)?GAME +((HAS +)?ENDED|IS +OVER)(\\s(.|\\s)*)?$",
    element(2, re:compile(Reg)).

find_game_end(#regex{msg_text_u = MsgTextU, game_end = RE}) ->
    re:run(MsgTextU, RE).

%% -----------------------------------------------------------------------------

check_for_game_unend(S, M, G) ->
    case find_game_unend(S) of
        nomatch ->
            G;
        {match, _Ms} ->
            {_Reply, G2} = mafia_time:end_game(M, G),
            G2
    end.

regex_game_unend() ->
    Reg = "^((.|\\s)*\\s)?(GAME +(HAS +)?UNENDED|UNEND +GAME)(\\s(.|\\s)*)?$",
    element(2, re:compile(Reg)).

find_game_unend(#regex{msg_text_u = MsgTextU, game_unend = RE}) ->
    re:run(MsgTextU, RE).

%% -----------------------------------------------------------------------------

check_for_votes(#regex{msg_text_u = MsgU}, M, G) ->
    check_VOTE(MsgU, M, G),
    EndStr = "##END",
    UnendStr = "##UNEND",
    case {string:str(MsgU, EndStr),
          string:str(MsgU, UnendStr)} of
        {0, 0} -> ok;
        {_, 0} -> %% add end
            reg_end_vote(G, add, M);
        {0, _} -> %% remove end
            reg_end_vote(G, remove, M);
        _ -> ok
    end.

check_VOTE(MsgUC, M, G) ->
    Players = G#mafia_game.players_rem,
    Players2 = add_nolynch_and_aliases(Players),
    VoteStr = "##VOTE",
    UnvoteStr = "##UNVOTE",
    case {mafia_data:rm_to_after_pos(MsgUC, VoteStr),
          mafia_data:rm_to_after_pos(MsgUC, UnvoteStr)} of
        {{0, ""}, {0, ""}} ->
            ignore;
        {{0, ""}, {_, RestUC}} ->
            Vote = ?l2b(?Unvote),
            reg_vote(M, G, Vote, Vote, true),
            check_VOTE(RestUC, M, G);
        {{Pos, RestUC}, _} ->
            Msg = mafia_print:html2txt(?b2l(M#message.message)),
            RawVote =
                ?l2b(string:strip(
                       string:left(
                         mafia_data:get_after_pos(
                           Pos, length(VoteStr), Msg),
                         60))),
            case rank_options(Players2, RestUC) of
                [{NumV, TopP}] when NumV >= 2; NumV >= size(TopP) ->
                    reg_vote(M, G, TopP, RawVote, true);
                [{NumV1, TopP}, {NumV2, _}|_]
                  when NumV1 > NumV2 andalso
                       (NumV1 >= 2 orelse
                        NumV1 >= size(TopP)) ->
                    reg_vote(M, G, TopP, RawVote, true);
                _ ->
                    Vote = ?l2b("-"),
                    reg_vote(M, G, Vote, RawVote, false)
            end,
            check_VOTE(RestUC, M, G)
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
    lists:member(UserB, UsersB).
    %% User = ?b2l(UserB),
    %% UserU = ?l2u(User),
    %% UsersU = [?b2ul(U) || U <- UsersB],
    %% lists:member(UserU, UsersU).

%% -----------------------------------------------------------------------------

add_nolynch_and_aliases(Players) ->
    AddAlias =
        fun(P, Acc) ->
                case ?ruser(P) of
                    [#user{aliases = AliasesB}] when AliasesB /= [] ->
                        Acc ++ [{?b2l(P), ?b2l(A)} || A <- AliasesB];
                    _ -> Acc
                end
        end,
    Aliases = lists:foldl(AddAlias, [], Players),
    [?b2l(P) || P <- Players] ++ ?Extra ++ Aliases.

%% -----------------------------------------------------------------------------

rank_options(Players, RestUC) ->
    RestUCW = waste_spaces(RestUC),
    F = fun(P) ->
                PorA = select_alias(P),
                PlayerUCW = ?l2u(waste_spaces(PorA)),
                r_count(PlayerUCW, RestUCW, 0)
        end,
    ?lrev(lists:sort([{F(P), ?l2b(select_name(P))} || P <- Players])).

%% @doc Search for upper case SearchU in #regex.pre_match_u
-spec regex_pre_find(SearchU :: string(), #regex{}) ->
                            {?nomatch, #regex{}} |
                            {?match, #regex{}}.
regex_pre_find(SearchU, Reg = #regex{}) ->
    MsgU = Reg#regex.pre_match_u,
    case string:str(MsgU, SearchU) of
        0 ->
            {?nomatch,
             Reg#regex{pos = 0,
                       msg_text = "",
                       msg_text_u = ""
                      }};
        P ->
            Msg = Reg#regex.pre_match,
            PreMatch = string:left(Msg, P - 1),
            PreMatchU = string:left(MsgU, P - 1),
            Match = string:substr(Msg, P, length(SearchU)),
            PostMatch = mafia_data:get_after_pos(P, SearchU, Msg),
            PostMatchU = mafia_data:get_after_pos(P, SearchU, MsgU),
            Reg2 = Reg#regex{msg_text = PostMatch,
                             msg_text_u = PostMatchU,
                             match = Match,
                             pos = P,
                             pre_match = PreMatch,
                             pre_match_u = PreMatchU
                            },
            {?match, Reg2}
    end.

%% @doc Search for upper case SearchU in #regex.msg_txt_u
-spec regex_find(SearchU :: string(), #regex{}) ->
                        {?nomatch, #regex{}} |
                        {?match, #regex{}}.
regex_find(SearchU, Reg = #regex{}) ->
    MsgU = Reg#regex.msg_text_u,
    case string:str(MsgU, SearchU) of
        0 ->
            {?nomatch,
             Reg#regex{pos = 0,
                       pre_match = Reg#regex.msg_text,
                       pre_match_u = Reg#regex.msg_text_u
                      }};
        P ->
            Msg = Reg#regex.msg_text,
            PreMatch = string:left(Msg, P - 1),
            PreMatchU = string:left(MsgU, P - 1),
            Match = string:substr(Msg, P, length(SearchU)),
            PostMatch = mafia_data:get_after_pos(P, SearchU, Msg),
            PostMatchU = mafia_data:get_after_pos(P, SearchU, MsgU),
            Reg2 = Reg#regex{msg_text = PostMatch,
                             msg_text_u = PostMatchU,
                             match = Match,
                             pos = P,
                             pre_match = PreMatch,
                             pre_match_u = PreMatchU
                            },
            {?match, Reg2}
    end.

%% Return {MatchPos, PreMatchStr, PostMatchStr}
find_parts(Str, Search) ->
    find_parts_I(Str, Search, no_match).

find_parts_I(Str, Search, Mode) ->
    StrU = ?l2u(Str),
    SearchU = ?l2u(Search),
    find_parts_reply(Mode,
                     string:str(StrU, SearchU),
                     Str,
                     Search).

find_parts_reply(no_match, 0, Str, _Search) ->
    {0, Str, ""};
find_parts_reply(no_match, P, Str, Search) ->
    {P,
     string:left(Str, P - 1),
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
    case ?b2ul(Vote) of
        ?END -> reg_end_vote(G, add, M);
        ?UNEND -> reg_end_vote(G, remove, M);
        _ ->
            vote2(M, G, Vote, RawVote, IsOkVote)
    end.

vote2(M, G, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        Phase = #phase{don =?day} ->
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
            Day = ?rday(G, Phase),
            Votes = Day#mafia_day.votes,
            Votes2 =
                case lists:keyfind(User, 1, Votes) of
                    false -> [{User, [NewVote]} | Votes];
                    {User, UVotes} ->
                        ExistVote = lists:keyfind(NewVote#vote.id,
                                                  #vote.id,
                                                  UVotes),
                        UVotes2 =
                            if IsOkVote; ExistVote == false ->
                                    %% Overwrite vote for this msg_id is ok
                                    lists:keystore(NewVote#vote.id,
                                                   #vote.id,
                                                   UVotes,
                                                   NewVote);
                               true ->
                                    UVotes
                            end,
                        lists:keystore(User, 1, Votes, {User, UVotes2})
                end,
            ?dwrite_day(Day#mafia_day{votes = Votes2});
        _ ->
            ignore
    end.

reg_end_vote(G, Op, M) ->
    case mafia_time:calculate_phase(G, M#message.time) of
        Phase = #phase{don = ?day} ->
            Day = ?rday(G, Phase),
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
            ?dwrite_day(Day#mafia_day{end_votes = NewEndVotes});
        _ ->
            ok
    end.

%% -------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

get_pl_re(Text) ->
    Txt = mafia_print:html2txt(Text),
    #regex{msg_text_u = ?l2u(Txt),
           msg_text = Txt,
           play_repl = regex_player_replacement()}.

find_player_replacement_test_() ->
    [
     ?_assertMatch(
        {replace, "Bbb", "Aaa", #regex{}},
        find_player_replacement(get_pl_re("Aaa is replacing Bbb"))),
     ?_assertMatch(
        {replace, "Bbb", "Aaa", #regex{}},
        find_player_replacement(
          get_pl_re(
            "\nsadf\raf\nAaa is replacing Bbb\nfsda\nfdsa"))),
     ?_assertMatch(
        {replace, "Bbb", "Aaa", #regex{}},
        find_player_replacement(
          get_pl_re(
            "Aaa    has replaced   Bbb\r\n   \n  jj"))),
     ?_assertMatch(
        {replace, "Bbb", "Aaa", #regex{}},
        find_player_replacement(
          get_pl_re(
            "Aaa    has  replaced   Bbb")))
    ].

tu(Str) -> ?l2u(mafia_print:html2txt(Str)).

find_deadline_move_test_() ->
    [
     ?_assertMatch(
        not_found,
        find_deadline_move(tu("deadline move 24 H earlier"))),
     ?_assertMatch(
        {found, -86400},
        find_deadline_move(tu("deadline moved 24 H earlier"))),
     ?_assertMatch(
        {found, -86400},
        find_deadline_move(tu("deadline moved 24H earlier"))),
     ?_assertMatch(
        {found, 86400},
        find_deadline_move(tu("s \n deadline moved 24H  later\n \nsf"))),
     ?_assertMatch(
        {found, -86400},
        find_deadline_move(tu(" \n deadline moved 24H earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        {found, 86400},
        find_deadline_move(
          tu(" \n deadlineasfsadf moved 24H later  sadf\n  \nsf"))),
     ?_assertMatch(
        {found, -86400},
        find_deadline_move(
          tu("  \n deadline asfsadf moved 24H earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        not_found,
        find_deadline_move(
          tu("  \n deaddline day 1 moved 24H earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        {found, -86400},
        find_deadline_move(
          tu("  \n deadline d2 moved 24H earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        {found, -86700},
        find_deadline_move(
          tu("  \n deadline n3 moved 24H 5m earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        {error, bad_time},
        find_deadline_move(
          tu("  \n deadline  moved 24H 5m3s earlier  sadf\n  \nsf"))),
     ?_assertMatch(
        {found, -86700},
        find_deadline_move(
          tu("  \n deadline night 3 moved 24H 5m earlier  sadf\n  \nsf")))
    ].

get_game_end(Text) ->
    #regex{msg_text_u = ?l2u(mafia_print:html2txt(Text)),
           game_end = regex_game_end()}.

%% GAME ((HAS )?ENDED|IS OVER)
find_game_end_test_() ->
    [
     ?_assertMatch(
        {match, _},
        find_game_end(get_game_end("game has ended"))),
     ?_assertMatch(
        {match, _},
        find_game_end(get_game_end("game ended"))),
     ?_assertMatch(
        {match, _},
        find_game_end(get_game_end("game is over"))),
     ?_assertMatch(
        {match, _},
        find_game_end(
          get_game_end(
            " a \nasfs s \n a d f\n the game  has  ended night 5 \r \n \n")))
    ].

get_game_unend(Text) ->
    #regex{msg_text_u = ?l2u(mafia_print:html2txt(Text)),
           game_unend = regex_game_unend()}.

find_game_unend_test_() ->
    [
     ?_assertMatch(
        {match, _},
        find_game_unend(get_game_unend("game has unended"))),
     ?_assertMatch(
        {match, _},
        find_game_unend(get_game_unend("game unended"))),
     ?_assertMatch(
        {match, _},
        find_game_unend(get_game_unend("unend game"))),
     ?_assertMatch(
        {match, _},
        find_game_unend(
          get_game_unend(" \rg\n g \n  game  has  unended  \n g  \r g "))),
     ?_assertMatch(
        {match, _},
        find_game_unend(
          get_game_unend(" \rg\n g \n the game  has  unended now \n g  \r g ")))
    ].
