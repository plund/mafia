-module(mafia_vote).

-export([get_regexs/0,
         check_cmds_votes/3,
         find_parts/2,
         player_type/2
        ]).

-include("mafia.hrl").

%% first match is lowest since pos is first
-record(regex,
        {pos,
         msg_text_u,
         msg_text,
         match,
         match_u,
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

insert_msg_into_re(Msg, Re) ->
    Msg2 =
        mafia_lib:html2txt(
          mafia_lib:remove_blockquotes(
            unicode:characters_to_list(Msg))),
    MsgU = ?l2u(Msg2),
    Re#regex{msg_text_u = MsgU, msg_text = Msg2}.

%% -----------------------------------------------------------------------------
%% Returns ignore when #message or #mafia_game cannot be found
%% -----------------------------------------------------------------------------
-spec check_cmds_votes(#mafia_game{},
                       #regex{}, #message{})
                      -> MsgTime :: seconds1970().
check_cmds_votes(G = #mafia_game{}, Re, M = #message{}) ->
    mafia_data:update_stat(G, M),
    Re2 = insert_msg_into_re(M#message.message, Re),
    check_cmds_votes2(G, Re2, M).

check_cmds_votes2(G, Re, M) ->
    IsEnded = case G#mafia_game.game_end of
                  ?undefined -> false;
                  {EndTime, _MsgId} ->
                      M#message.time >= EndTime
              end,
    PhaseM = mafia_time:calculate_phase(G, M#message.time),
    IsStarted = PhaseM#phase.ptype /= ?game_start,
    %% Is this a phase change?
    DoGenerate =
        case mafia_lib:prev_msg(M) of
            ?none -> ?false;
            PrevM ->
                PhasePrevM = mafia_time:calculate_phase(G, PrevM#message.time),
                PhasePrevM /= PhaseM
                    andalso PhasePrevM#phase.ptype /= ?game_start
        end,
    if not IsStarted ->
            case player_type(M, G) of
                ?gm -> check_for_gm_cmds(Re, M, G, true);
                UserType -> log_unallowed_msg(UserType, M)
            end;
       not IsEnded ->
            case player_type(M, G) of
                ?gm -> check_for_gm_cmds(Re, M, G, DoGenerate);
                ?player -> check_for_votes(G, M, Re, PhaseM);
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
    if DoGenerate -> ?regen_history(change_phase, M, G);
       ?true -> ok
    end,
    M#message.time.

log_unallowed_msg(Type, M) ->
    MTime = M#message.time,
    User = ?b2l(M#message.user_name),
    MsgId = M#message.msg_key,
    ?dbg(MTime, {Type, sent_message, MsgId, User}).

%% Removes player from Game if dead
check_for_gm_cmds(Re, M, G, DoGenerate) ->
    G3 = check_for_early_end(Re, M#message.time, G),
    G4 = check_for_deadline_move(Re, M, G3),
    G5 = check_for_player_replacement(Re, M, G4),
    G6 = check_for_player_resurrect(Re, M, G5),
    %% Switched order of next 2 to regenerate last day with
    %% possible deaths in mafia_time:end_game
    G7 = check_for_deaths(Re, M, G6),
    IsSomeoneDead = G7 /= G6,
    G8 = check_for_game_end(Re, M, G7),

    %% if time is 0 - 20 min after a deadline generate a history page
    {RelTimeSecs, _PT} = mafia_time:nearest_deadline(G8, M#message.time),
    if not DoGenerate,
       IsSomeoneDead,
       RelTimeSecs >= 0,
       RelTimeSecs =< ?MAX_GM_DL_SECS,
       G8#mafia_game.game_end == ?undefined ->
            ?regen_history(died, M, G7);
       true ->
            ok
    end,
    G8.

check_for_player_resurrect(Reg = #regex{}, M, G) ->
    SearchU1 = "##RESURRECT",
    case regex_find_words(SearchU1, Reg) of
        {?nomatch, _} -> G;
        {?match, Reg2} ->
            PostMatch = Reg2#regex.msg_text,
            [Player | _] = string:tokens(PostMatch, ".\n"),
            PlayerB = ?l2b(string:strip(Player)),
            case mafia_op:resurrect_player(G, M, PlayerB) of
                {ok, G2} ->
                    check_for_player_resurrect(Reg2, M, G2);
                not_found ->
                    G
            end
    end.

check_for_deaths(Reg = #regex{}, M, G) ->
    Deaths = [D#death.player || D = #death{} <- G#mafia_game.player_deaths],
    LookForUsers = G#mafia_game.players_rem ++ Deaths,
    case check_for_deaths2(Reg, M, LookForUsers) of
        ?nomatch -> G;
        {?noone_died, _, Reg2} ->
            check_for_deaths(Reg2, M, G);
        {KilledUserB, DeathComment, Reg2} ->
            G2 = case lists:member(KilledUserB,
                                   G#mafia_game.players_rem) of
                     ?true ->
                         element(2,
                                 mafia_op:kill_player(G, M, KilledUserB,
                                                      DeathComment));
                     ?false ->
                         %% GM cannot kill non-remaining player
                         G
                 end,
            check_for_deaths(Reg2, M, G2)
    end.

check_for_deaths2(Reg = #regex{}, M, LookForUsers) ->
    %% find "has died" on line
    SearchU1 = "DIED",
    SearchU2 = "DEAD",
    SearchU3 = "BEEN LYNCHED",
    SearchU4 = "BEEN EATEN",
    case [regex_find_words(SearchU1, Reg),
          regex_find_words(SearchU2, Reg),
          regex_find_words(SearchU3, Reg),
          regex_find_words(SearchU4, Reg)] of
        [{?nomatch, _}, {?nomatch, _}, {?nomatch, _}, {?nomatch, _}] ->
            %% no-one has died
            ?nomatch;
        Matches ->
            Rs = [ReRes || {MRes, ReRes} <- Matches, MRes == ?match],
            Reg2 = lists:min(Rs),
            if is_record(M, message) ->
                    ?dbg(M#message.time,
                         {msgid_pos, M#message.msg_key, Reg2#regex.pos});
               true -> ok
            end,
            {KilledUserB, DeathComment} =
                read_death_line(Reg#regex.msg_text, Reg2, LookForUsers),
            {KilledUserB, DeathComment, Reg2}
    end.

%% Returns exact User binary.
-spec read_death_line(string(), #regex{}, [user()])
                     -> {user() | ?noone_died,
                         Comment :: ?use_full_line | string()}.
read_death_line(MsgLong, Reg, LookForUsers) ->
    %% find one remaining player before on the same line.
    %% Extend the search list with already dead players to make an update
    %% of the death comment possible.
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

is_last_non_letter(HStr) ->
    is_first_non_letter(?lrev(HStr)).

is_first_non_letter([]) -> true;
is_first_non_letter([H|_]) ->
    lists:member(H, " ,.;:!\"#€%&/()=+?^´`'*<>-_\t\r\n").

%% -----------------------------------------------------------------------------

check_for_early_end(#regex{msg_text_u = MsgText}, Time, G) ->
    case find_early_end(MsgText) of
        {?error, _} -> G;
        {ok, Ptype} ->
            case mafia_time:calculate_phase(G, Time) of
                #phase{ptype = ?game_ended} ->
                    ?dbg(Time, "GM early end command for game that has ended"),
                    G;
                #phase{ptype = Ptype} = Phase ->
                    mafia_time:end_phase(G, Phase, Time);
                _ ->
                    ?dbg(Time,
                         {"GM early end command with wrong phase type", Ptype}),
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
                    %% Get beginning of "ENDED EARLY" line
                    HEarlyLine = ?lrev(hd(string:tokens(?lrev(HStr1), "\n"))),
                    case {find_parts(HEarlyLine, SearchU2),
                          find_parts(HEarlyLine, SearchU3)} of
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
    case re:run(MsgTextU, Reg, [unicode]) of
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
    case re:run(RestM, RegEnd, [unicode]) of
        nomatch -> {?error, bad_time};
        _ -> {ok, NumD, NumH, NumM}
    end.

find_expr(Text, Reg) ->
    case re:run(Text, Reg, [unicode]) of
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
            case mafia_op:replace_player(G, M, NewPlayer, OldPlayer) of
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
    element(2, re:compile(Reg, [unicode])).

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
    element(2, re:compile(Reg, [unicode])).

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
    element(2, re:compile(Reg, [unicode])).

find_game_unend(#regex{msg_text_u = MsgTextU, game_unend = RE}) ->
    re:run(MsgTextU, RE).

%% -----------------------------------------------------------------------------
-define(V_VOTE, "##VOTE").
-define(V_UNVOTE, "##UNVOTE").
-define(V_END, "##END").
-define(V_UNEND, "##UNEND").

%% @doc Count only last valid ##VOTE/##UNVOTE in a message
%% Count only last ##END/##UNEND in a message
%% Count ##END only if it is last and the user is voting for someone
%% Start checking current Vote status, then process message
check_for_votes(G, M, Reg = #regex{}, #phase{ptype = ?day, num = DayNum}) ->
    %% Check current vote and end vote status
    User = M#message.user_name,
    #mafia_day{votes = Votes,
               end_votes = EndVotes} =
        ?rday(G#mafia_game.game_num, DayNum),
    CurVote = case lists:keyfind(User, 1, Votes) of
                  false -> ?undefined;
                  {_, UVotes} ->
                      case [ValUVote || #vote{vote = ValUVote,
                                              valid = true} <- UVotes] of
                          [] -> ?undefined;
                          ValUVotes ->
                              lists:last(ValUVotes)
                      end
              end,
    IsEndVote = lists:keymember(User, 1, EndVotes),

    %% Analyze message text and figure out what that needs to change
    Actions = check_for_votes1(G, Reg, CurVote, IsEndVote),
    _ = [case Action of
             remove_end = Op ->
                 reg_end_vote(G, M, Op);
             {add_end, _} = Op ->
                 reg_end_vote(G, M, Op);
             {vote, NewVote, RawVote, IsValid} ->
                 reg_vote(G, M, NewVote, RawVote, IsValid);
             {unvote} ->
                 UnVote = ?l2b(?Unvote),
                 reg_vote(G, M, UnVote, UnVote, true)
         end
         || Action <- Actions],
    ok;
check_for_votes(_, _, _, _) ->
    ok.

%% Returns actions to perform
check_for_votes1(G, Reg, CurVote, IsEndVote) ->
    %% ?dbg(CurVote),
    {Vote, EndVote} = check_for_votes2(G, Reg, []),
    {IsStillEndVote, Actions1, NewVote2} =
        case Vote of
            {vote, NewVote, _, _} when NewVote /= CurVote ->
                {false, [remove_end, Vote], NewVote};
            {vote, NewVote, _, _}  when NewVote == CurVote ->
                {IsEndVote, [Vote], NewVote};
            {unvote} = V when CurVote /= ?undefined ->
                {false, [remove_end, V], ?undefined};
            ?undefined ->
                {IsEndVote, [], CurVote};
            _ ->
                {false, [], CurVote}
        end,
    %% Find out if there is a vote after this message
    IsVote = case {CurVote, Vote} of
                 {?undefined, ?undefined} -> false;
                 {_, ?undefined} -> true;
                 _ when ?e1(Vote) == vote -> true;
                 _ when ?e1(Vote) == unvote -> false
             end,
    Actions2 =
        case EndVote of
            end_vote when not IsStillEndVote, IsVote ->
                [{add_end, NewVote2}];
            unend_vote when IsEndVote ->
                [remove_end];
            ?undefined -> [];
            _  -> []
        end,
    Actions1 ++ Actions2.

%% Returns relevant votes to consider
check_for_votes2(G, Reg, Acc) ->
    case [regex_find(?V_VOTE, Reg),
          regex_find(?V_UNVOTE, Reg),
          regex_find(?V_END, Reg),
          regex_find(?V_UNEND, Reg)] of
        [{?nomatch, _}, {?nomatch, _}, {?nomatch, _}, {?nomatch, _}] ->
            remove_double_votes(Acc, ?undefined);
        Matches ->
            Rests = [ReRest || {MRes, ReRest} <- Matches, MRes == ?match],
            Reg2 = lists:min(Rests),
            case Reg2#regex.match_u of
                ?V_VOTE ->
                    Msg = Reg2#regex.msg_text,
                    RawVote = unicode:characters_to_binary(
                                string:strip(
                                  string:left(Msg, 60))),
                    RestUC = Reg2#regex.msg_text_u,
                    {Vote, IsValid} =
                        case rank_options(G, RestUC) of
                            [{NumV, TopP}] when NumV >= 2; NumV >= size(TopP) ->
                                {TopP, true};
                            [{NumV1, TopP}, {NumV2, _}|_]
                              when NumV1 > NumV2 andalso
                                   (NumV1 >= 2 orelse
                                    NumV1 >= size(TopP)) ->
                                {TopP, true};
                            [{NumV1, TopP} | _] = Rankings when NumV1 >= 2 ->
                                IsOneTop =
                                    lists:all(fun({N, Pl}) ->
                                                      Pl == TopP orelse
                                                          N < NumV1
                                              end,
                                              Rankings),
                                case IsOneTop of
                                    true -> {TopP, true};
                                    false -> {?l2b("-"), false}
                                end;
                            _ ->
                                {?l2b("-"), false}
                        end,
                    New =
                        case ?b2l(Vote) of
                            ?END -> end_vote;
                            ?UNEND -> unend_vote;
                            _ -> {vote, Vote, RawVote, IsValid}
                        end,
                    check_for_votes2(G, Reg2, [New | Acc]);
                ?V_UNVOTE ->
                    check_for_votes2(G, Reg2, [{unvote} | Acc]);
                ?V_END ->
                    check_for_votes2(G, Reg2, [end_vote | Acc]);
                ?V_UNEND ->
                    check_for_votes2(G, Reg2, [unend_vote | Acc])
            end
    end.

%% Votes comes in reverser 'time' order
%% Find first vote or unvote in list
remove_double_votes([V | _], EndVote)
  when ?e1(V) == vote; ?e1(V) == unvote ->
    {V, EndVote};
%% Find first end_vote and unend_vote in list before vote/unvote
remove_double_votes([V | T], EndVote)
  when EndVote == ?undefined andalso
       (V == end_vote orelse V == unend_vote) ->
    remove_double_votes(T, V);
remove_double_votes([_ | T], EndVote) ->
    remove_double_votes(T, EndVote);
remove_double_votes([], EndVote) ->
    {?undefined, EndVote}.

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

%% -----------------------------------------------------------------------------

add_nolynch_and_aliases(G) ->
    Players = G#mafia_game.players_rem,
    AddAlias =
        fun(P, Acc) ->
                Site = G#mafia_game.site,
                case ?ruser(P, Site) of
                    [#user{aliases = AliasesB}] when AliasesB /= [] ->
                        Acc ++ [{?b2l(P), ?b2l(A)} || A <- AliasesB];
                    _ -> Acc
                end
        end,
    Aliases = lists:foldl(AddAlias, [], Players),
    [?b2l(P) || P <- Players] ++ ?Extra ++ Aliases.

%% -----------------------------------------------------------------------------
-spec rank_options(#mafia_game{}, string()
                  ) -> [{integer(), user()}].
rank_options(G, RestUC) ->
    Players = add_nolynch_and_aliases(G),
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
            MatchU = string:substr(MsgU, P, length(SearchU)),
            PostMatch = mafia_data:get_after_pos(P, SearchU, Msg),
            PostMatchU = mafia_data:get_after_pos(P, SearchU, MsgU),
            Reg2 = Reg#regex{msg_text = PostMatch,
                             msg_text_u = PostMatchU,
                             match = Match,
                             match_u = MatchU,
                             pos = P,
                             pre_match = PreMatch,
                             pre_match_u = PreMatchU
                            },
            {?match, Reg2}
    end.

%% Same as regex_find but surrounding chars must not be alphanumerical
regex_find_words(SearchU, Reg = #regex{}) ->
    case regex_find(SearchU, Reg) of
        M = {?nomatch, _} -> M;
        M = {?match, Reg2} ->
            case is_either_side_alphanum(Reg2) of
                ?false -> M;
                ?true -> regex_find_words(SearchU, Reg2)
            end
    end.

is_either_side_alphanum(#regex{pre_match_u = Pre,
                               msg_text_u = Post}) ->
    is_alpha(?lrev(Pre)) orelse is_alpha(Post).

is_alpha([]) -> ?false;
is_alpha([Char | _]) -> mafia_lib:is_alpha_num(Char).

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

find_parts_reply(no_match, 0, Str, _) ->
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

%% +1 for matching char
r_count([Hp|Tp], [Hr|Tr], N) when Hp == Hr ->
    r_count(Tp, Tr, N + 1);
%% stop when no match
r_count([_Hp|_Tp], [], N) ->
    N;
%% +1 for complete
r_count([], [_Hr|_Tr], N) ->
    N + 1;
%% stop when diff
r_count([Hp|_Tp], [Hr|_Tr], N) when Hp /= Hr ->
    N;
%% +1 for exact match
r_count([], [], N) ->
    N + 1.

%% -----------------------------------------------------------------------------

reg_vote(G, M, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        Phase = #phase{ptype =?day} ->
            io:format(
              "~s Register Vote: ~s votes ~p ~s\n",
              [mafia_print:print_time(M#message.time, short),
               ?b2l(M#message.user_name), ?b2l(RawVote),
               if IsOkVote -> "Approved"; true -> "Rejected" end]),
            User = M#message.user_name,
            NewVote = #vote{time = M#message.time,
                            msg_key = M#message.msg_key,
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
                        ExistVote = lists:keyfind(NewVote#vote.msg_key,
                                                  #vote.msg_key,
                                                  UVotes),
                        UVotes2 =
                            if IsOkVote; ExistVote == false ->
                                    %% Overwrite vote for this msg_key is ok
                                    lists:keystore(NewVote#vote.msg_key,
                                                   #vote.msg_key,
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

reg_end_vote(G, M, Op) ->
    %% end_votes is [{User, Voted}] so that max end votes for one Voted
    %% can be counted.
    case mafia_time:calculate_phase(G, M#message.time) of
        Phase = #phase{ptype = ?day} ->
            Day = ?rday(G, Phase),
            User = M#message.user_name,
            OldEndVotes = Day#mafia_day.end_votes,
            NewEndVotes =
                case Op of
                    {add_end, Vote} ->
                        lists:keystore(User, 1, OldEndVotes, {User, Vote});
                    remove_end ->
                        lists:keydelete(User, 1, OldEndVotes)
                end,
            Day2 = Day#mafia_day{end_votes = NewEndVotes},

            %% Calculate higest end vote target
            NewTop =
                lists:sort(
                  fun(A, B) -> A >= B end, % reverse sort
                  lists:foldl(
                    fun({_, V}, Cnt) ->
                            case lists:keyfind(V, 2, Cnt) of
                                false ->
                                    [{1, V} | Cnt];
                                {C, _} ->
                                    lists:keystore(V, 2, Cnt, {C + 1, V})
                            end
                    end,
                    [],
                    NewEndVotes)),
            {TopNum, TopVote} =
                case NewTop of
                    [] -> {0, ?undefined};
                    [H | _] -> H
                end,
            Day3 =
                if TopNum > Day#mafia_day.endvote_high_num ->
                        Day2#mafia_day{
                          endvote_high_num = TopNum,
                          endvote_high = {M#message.time, TopVote}};
                   true -> Day2
                end,
            ?dwrite_day(Day3);
        _ ->
            ok
    end.

%% -------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

get_pl_re(Text) ->
    Txt = mafia_lib:html2txt(Text),
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

tu(Str) -> ?l2u(mafia_lib:html2txt(Str)).

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
    #regex{msg_text_u = ?l2u(mafia_lib:html2txt(Text)),
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
    #regex{msg_text_u = ?l2u(mafia_lib:html2txt(Text)),
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

ut_regex(Text) ->
    #regex{msg_text = Text,
           msg_text_u = ?l2u(Text)}.

ut_game(Ps) ->
    #mafia_game{players_rem = [?l2b(P) || P <- Ps]}.

check_for_votes1_test() ->
    Mods = [mafia_lib],
    meck:new(Mods, [passthrough]),
    meck:expect(mafia_lib, ruser, fun(_, _) -> [] end),
    ?assertMatch(
       [],
       check_for_votes1(ut_game([]),
                        ut_regex("Hello"),
                        <<"peterlund">>, %% CurVote,
                        true %% IsEndVote
                       )
      ),
    ?assertMatch(
       [{vote, <<"peterlund">>, <<"peter">>, true}],
       check_for_votes1(ut_game(["peterlund"]),
                        ut_regex("##vote peter"),
                        <<"peterlund">>, %% CurVote,
                        false %% IsEndVote
                       )
      ),
    ?assertMatch(
       [{vote, <<"peterlund">>, <<"peter">>, true}],
       check_for_votes1(ut_game(["peterlund"]),
                        ut_regex("##vote peter"),
                        <<"peterlund">>, %% CurVote,
                        true %% IsEndVote
                       )
      ),
    ?assertMatch(
       [remove_end, {vote, <<"brainbomb">>, <<"brain">>, true}],
       check_for_votes1(ut_game(["peterlund", "brainbomb"]),
                        ut_regex("##vote brain"),
                        <<"peterlund">>, %% CurVote,
                        true %% IsEndVote
                       )
      ),
    ?assertMatch(
       %% but end_vote should not be registered
       [remove_end, {unvote}],
       check_for_votes1(ut_game([]),
                        ut_regex("##unvote"),
                        <<"brainbomb">>, %% CurVote,
                        true %% IsEndVote
                       )
      ),
    meck:unload(Mods).

check_for_votes2_test() ->
    Mods = [mafia_lib],
    meck:new(Mods, [passthrough]),
    meck:expect(mafia_lib, ruser, fun(_, _) -> [] end),
    ?assertMatch(
       {{vote, <<"abc">>, <<"abc, ##end">>, true},
        end_vote},
       check_for_votes2(ut_game(["abc", "def", "ghi"]),
                        ut_regex("##vote abc, ##end"),
                        [])
      ),
    ?assertMatch(
       {{vote, <<"ghi">>, <<"ghill, dd">>, true},
        ?undefined},
       check_for_votes2(ut_game(["abc", "def", "ghi"]),
                        ut_regex("##vote hej, ##end, ##vote abc, "
                                 "##end ##vote ghill, dd"),
                        [])
      ),
    ?assertMatch(
       %% but end_vote should not be registered
       {{unvote}, end_vote},
       check_for_votes2(ut_game([]),
                        ut_regex("##unvote ##end"),
                        [])
      ),
    %% ##unvote
    ?assertMatch(
       {{unvote}, ?undefined},
       check_for_votes2(ut_game(["abc", "def", "ghi"]),
                        ut_regex("##unvote"),
                        [])
      ),
    ?assertMatch(
       {{unvote}, unend_vote},
       check_for_votes2(ut_game(["abc", "def", "ghi"]),
                        ut_regex("##unvote ##unend"),
                        [])
      ),
    %% ##VOTE END / UNEND
    ?assertMatch(
       {?undefined, end_vote},
       check_for_votes2(ut_game(["ezio"]),
                        ut_regex("##vote end"),
                        [])
      ),
    ?assertMatch(
       {?undefined, end_vote},
       check_for_votes2(ut_game(["enda"]),
                        ut_regex("##voteend"),
                        [])
      ),
    ?assertMatch(
       {?undefined, unend_vote},
       check_for_votes2(ut_game(["una"]),
                        ut_regex("##voteunend"),
                        [])
      ),
    %% Vote similar users "abc" "abcde"
    ?assertMatch(
       {{vote, <<"abc">>, <<"abc ef ##end">>, true}, end_vote},
       check_for_votes2(ut_game(["abc", "abcde"]),
                        ut_regex("##vote abc ef ##end "),
                        [])
      ),
    ?assertMatch(
       {{vote, <<"abcde">>, <<"abcd ef ##unend">>, true}, unend_vote},
       check_for_votes2(ut_game(["abc", "abcde"]),
                        ut_regex("##vote abcd ef ##unend "),
                        [])
      ),
    ?assertMatch(
       {{vote, <<"-">>, <<"ab ef ##unend">>, false}, unend_vote},
       check_for_votes2(ut_game(["abc", "abcde"]),
                        ut_regex("##vote ab ef ##unend "),
                        [])
      ),
    %% make sure one char match is not enough
    ?assertMatch(
       {{vote, <<"-">>, <<"ghug">>, false}, undefined},
       check_for_votes2(ut_game(["guak"]),
                        ut_regex("##vote ghug"),
                        [])
      ),
    meck:unload(Mods).

-define(DeathTxt1, "Golfinger heard footsteps approaching. Multiple people – and at a rapid pace no less. He quickly ducked behind a couch at the edge of the room. Suddenly the power went out. There was blackness. He remained still, trying to control his breathing. He couldn’t make it to the doorway in the darkness so he sat. The footsteps were in the room, and he heard labored breath. It was a scuffle.<br>
The lights flickered on momentarily! There were many forms in the room. It was pure mayhem. And just as quickly as the light came, they were taken back to darkness.<br>
<br>
Suddenly muzzle flashes alit the room, and Golfinger’s ears were enveloped. with the ringing from the shots. He covered his ears and continued to hide. The footsteps left the room. He sat for what seemed like an eternity.<br>
The lights came back on, a generator humming somewhere from the back of the estate. He peeked his head over the couch expecting the worst but instead saw nothing. Suddenly he heard something behind him. His world went black.<br>
<br>
<br>
<br>
Balki Bartokomous has died. He was Auric Goldfinger a VANILLA TOWN.<br>
<br>
<br>
<br>
NIGHT 4 HAS BEGUN. YOU MAY NOW POST").

check_for_deaths2_test() ->
    ?assertMatch(
       {<<"Balki Bartokomous">>,
        "He was Auric Goldfinger a VANILLA TOWN.", _},
       check_for_deaths2(death_re(?DeathTxt1),
                         ?undefined,
                         [<<"peterlund">>, <<"Balki Bartokomous">>])
      ).

death_re(Msg) ->
    MsgUni = unicode:characters_to_binary(Msg),
    insert_msg_into_re(MsgUni, get_regexs()).
