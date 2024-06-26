-module(mafia).

-include("mafia.hrl").
%% Change title "M35 Game Status Day 1"  "M35 History"
%% remove global values timezone_game, dst_game...
%% Check out free CA LetsEncrypt
%% Coordinate poll_timer and dl_timer. "No poll at dl"
%% Make use of dl_poll_info.txt when generating history pages at:
%%    1) deadline 2) player death at End of Day/Night
%%    - use quickcheck license
%% Useful? io:format("~*s~*s~*s~n", [-15, "aaa", 5, "bbb", -5, "cc"]).
%% try MacOs autostart again
%% vhosts in inets - no support - try patch inets :)
%% LOW - Add Last&more link also on game_end page
%% Instead add ServerKeeper/GM commands:
%% ##bot endgame <msgid> | unendgame
%% ##bot endphase|unendphase <msgid>
%% ##bot replaceplayer <msgid> <old> <new>
%% ##bot deadline <msgid> earlier|later <time>
%% ##bot assistant add|remove <msgid> <player>
%% - Display msgs since last login with a browser (cookie)
%%    - check if abbrev code can loop forever
%%    - add unit tests for abbreviations

%% interface
-export([
         start/0,
         stop/0,

         end_phase/2,
         unend_phase/2,
         move_next_deadline/4,
         end_game/2,
         unend_game/2,

         set_signup_thid/2,
         set_role_pm/2,

         initiate_game/2,
         initiate_game/3,
         write_settings_file/1,
         remove_not_running_game/1,
         delete_game_and_all_data/1,

         replace_player/4,
         kill_player/4,
         resurrect_player/3,
         ignore_message/2,
         add_gm/3,

         set_death_msgid/5,

         print_votes/0,
         print_votes/1,
         print_votes/2,

         man_downl/0,
         show_settings/0,
         refresh_messages/0,
         refresh_messages/1,
         refresh_votes/0,
         refresh_votes/1,
         refresh_votes/2,

         show_game_users/1,
         show_game_users/2,
         show_all_users/0,
         show_all_users/1,
         show_all_users/2,
         show_all_aliases/0,
         show_aliases/1,
         add_user/2,
         remove_user/2,
         add_alias/3,
         remove_alias/3,
         export_user_data/0,
         import_user_data/0,

         setup_mnesia/0,
         remove_mnesia/0,
         upgrade/0,
         upgrade/1
        ]).

%% libary
-export([game_for_thid/1
        ]).

%% utilities
-export([show_game_pages/1,
         show_game_data/2,
         l/0,
         print_all_cnts/0,
         print_all_cnts/1,
         save_cnts_to_file/0,

         fix_user_bug/0, fix_user_bug/1,
         get_user_names_for_site/1,
         get_user_names_for_site_binary/1
        ]).

-export([cmp_vote_raw/0
        ]).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================
start() -> application:start(mafia).
stop() -> application:stop(mafia).

print_votes() -> mafia_print:print_votes().
print_votes(DayNum) -> mafia_print:print_votes(DayNum).
print_votes(DayNum, Ptype) -> mafia_print:print_votes(DayNum, Ptype).
man_downl() -> mafia_data:man_downl().
setup_mnesia() -> mafia_db:setup_mnesia().
remove_mnesia() -> mafia_db:remove_mnesia().
upgrade() -> mafia_upgrade:upgrade().
upgrade(Tab) -> mafia_upgrade:upgrade(Tab).
refresh_messages() -> mafia_data:refresh_messages().
refresh_messages(GNum) -> mafia_data:refresh_messages(GNum).

refresh_votes() ->
    %% fprof:trace(start),
    mafia_data:refresh_votes(),
    %% fprof:trace(stop),
    ok.
refresh_votes(GNum) -> mafia_data:refresh_votes(GNum).
refresh_votes(GNum, Opts) -> mafia_data:refresh_votes(GNum, Opts).

%% 1. run refresh_votes()
%% 2. fprof:profile().
%% 3. fprof:analyse([{dest, "fprof.analysis.refresh_votes.5"}, {cols, 120}]).
%% 4. rp(lists:reverse(lists:sort([{L,Fun}||{_, {Fun,_,_,L}, _}
%%        <- element(2,file:consult("fprof.analysis.refresh_votes.5"))]))).
%% 5. rm fprof.trace

show_game_pages(GNum) when is_integer(GNum) ->
    case ?rgame(GNum) of
        [] ->
            io:format("No game record exist for ~p\n", [GNum]);
        [#mafia_game{thread_id = ThId,
                     site = Site}] when is_integer(ThId) ->
            io:format("Game page keys and messages\n"),
            show_game_pagesI(ThId, Site);
        [_] ->
            io:format("Game has no thread id\n")
    end.

show_game_pagesI(ThId, Site) ->
    [{Page, length((hd(?rpage(Key)))#page_rec.message_ids)}
     || Key = {_, Page, _} <- mafia_lib:all_page_keys({ThId, Site})].

-spec show_game_data(integer(), ?text | ?html ) -> ok | list().
show_game_data(GNum, Mode) ->
    Html =
        case ?rgame(GNum) of
            [] ->
                if Mode == ?text ->
                        io:format("No game record exist\n");
                   Mode == ?html -> ["No game record exist\r\n"]
                end;
            [#mafia_game{thread_id = ThId, site = Site}] ->
                PageKeys = [K || K = {Th, _, _} <- all_keys(page_rec),
                                 Th == ThId],
                NumPageKeys = length(PageKeys),
                MsgIds = [MsgId || {_, MsgId}
                                       <- mafia_lib:all_msgids(ThId, Site),
                                   [] /= ?rmess({MsgId, Site})
                         ],
                NumMsgs = length(MsgIds),
                if Mode == ?text ->
                        io:format("Game record exist\n"),
                        io:format("Thread id ~p\n", [ThId]),
                        io:format("Num Page records ~p\n", [NumPageKeys]),
                        io:format(
                          "There are ~p messages in mnesia for this game\n",
                          [NumMsgs]);
                   Mode == ?html ->
                        ["Game record exist\r\n",
                         "Thread id ",
                         if is_integer(ThId) -> ?i2l(ThId);
                            true -> "none" end,
                         "\r\n",
                         "Num Page records ", ?i2l(NumPageKeys), "\r\n",
                         "There are ", ?i2l(NumMsgs),
                         " messages in mnesia for this game\r\n"]
                end
        end,
    DayKeys = mafia_lib:all_day_keys(GNum),
    NumDayKeys = length(DayKeys),
    IsStatKey = fun(Id, {_, Id}) -> true;
                   (Id, {_, Id, _}) -> true;
                   (_, _) -> false
                end,
    StatKeys = [K || K <- mnesia:dirty_all_keys(stat), IsStatKey(GNum, K)],
    NumStatRecs = length(StatKeys),
    if Mode == ?text ->
            io:format("Num Day records ~p\n", [NumDayKeys]),
            io:format("Day keys ~p\n", [DayKeys]),
            io:format("Num Stat records ~p\n", [NumStatRecs]);
       Mode == ?html ->
            [Html,
             ["Num Day records ", ?i2l(NumDayKeys), "\r\n",
              "Day keys ", io_lib:format("~p", [DayKeys]), "\r\n",
              "Num Stat records ", ?i2l(NumStatRecs), "\r\n"
             ]]
    end.

%% -----------------------------------------------------------------------------
%% @doc Initiate game that has not started yet
%% @end
%% -----------------------------------------------------------------------------
initiate_game(GNum, Site) when ?IS_SITE_OK(Site) ->
    initiate_game(GNum, [], Site).

initiate_game(GNum, GMs, Site) when is_integer(GNum) ->
    DoFun = fun(G) ->
                    GMsB = get_user_list(GMs, Site),
                    ?dwrite_game(game_m1,
                                 G#mafia_game{game_num = GNum,
                                              gms = GMsB,
                                              site = Site}),
                    game:start_new_game(GNum),
                    {wrote, GNum, GMsB}
            end,
    do_if_not_running(GNum, DoFun).

-spec write_settings_file(game_num() | #mafia_game{})
                         -> ok | {error, atom()}.
write_settings_file(GNum) ->
    web_game_settings:write_settings_file(GNum).

%% -----------------------------------------------------------------------------
%% @doc Remove an initiated game that has not started yet
%% @end
%% -----------------------------------------------------------------------------
remove_not_running_game(GN) ->
    DoFun = fun(#mafia_game{game_num = ?undefined}) ->
                    {non_exist, GN};
               (#mafia_game{game_num = GNum}) ->
                    mnesia:dirty_delete(mafia_game, GNum),
                    {deleted, GN}
            end,
    do_if_not_running(GN, DoFun).

do_if_not_running(GN, DoFun) ->
    do_if_not_running2(?rgame(GN), DoFun).

do_if_not_running2([], DoFun) ->
    DoFun(#mafia_game{});
do_if_not_running2([G = #mafia_game{thread_id = ?undefined}], DoFun) ->
    DoFun(G);
do_if_not_running2(_, _) ->
    {error, running}.

get_user_list([], _) -> [];
get_user_list([User | T], Site) ->
    case mafia_lib:ruserUB(User, Site) of
        [#user{name = {NameB, _}}] ->
            [NameB | get_user_list(T, Site)];
        _ -> get_user_list(T, Site)
    end.

%% -----------------------------------------------------------------------------
%% @doc Delete a game and all game data in other tables
%% @end
%% -----------------------------------------------------------------------------
delete_game_and_all_data(GNum) when is_integer(GNum) ->
    case ?rgame(GNum) of
        [G] ->
            GName = if is_binary(G#mafia_game.name) ->
                            unicode:characters_to_list(G#mafia_game.name);
                       true -> "(no name)"
                    end,
            io:format("Deleting Game ~p - ~s\n",
                      [GNum, GName]),
            show_game_data(GNum, ?text),
            Answer = io:get_line(?l2a("Are you really sure you want to "
                                      "delete this game (NO/yes)> ")),
            case string:to_upper(Answer) of
                "YES" ++ _ ->
                    mafia_data:delete_game_data_in_other_tabs(GNum),
                    mnesia:dirty_delete(mafia_game, GNum),
                    {game_deleted, GNum};
                _ ->
                    game_not_deleted
            end;
        _ ->
            {error, no_game}
    end.


%% -----------------------------------------------------------------------------
-spec set_signup_thid(GNum :: game_num(),
                      thread_id()) -> ok | {error, atom()}.
set_signup_thid(GNum, SuThId) when is_integer(SuThId) ->
    case ?rgame(GNum) of
        [G] when G#mafia_game.thread_id == ?undefined ->
            ?dwrite_game(game_m3,
                         G#mafia_game{signup_thid = SuThId,
                                      page_to_read = 1}),
            ok;
        [G] when is_integer(G#mafia_game.thread_id) ->
            ?dwrite_game(game_m4, G#mafia_game{signup_thid = SuThId}),
            ok;
        _ ->
            {error, no_game}
    end.

set_role_pm(GNum, Url) when is_integer(GNum) ->
    UrlVal =
        case Url of
            ?undefined -> ?undefined;
            Url when is_list(Url) -> ?l2b(Url)
        end,
    set_role_pmI(?rgame(GNum), UrlVal).

set_role_pmI([G], UrlVal) ->
    ?dwrite_game(game_m5, G#mafia_game{role_pm = UrlVal}),
    ok;
set_role_pmI(_, _) -> {error, no_game_found}.


%% -----------------------------------------------------------------------------
%% @doc End current phase with GM message and set next phase at
%% the game's local date and time given by the GM in the same message
%% @end
%% -----------------------------------------------------------------------------
end_phase(GNum, MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            case mafia_time:calculate_phase(G, Time) of
                #phase{ptype = ?game_ended} -> {?error, ?game_ended};
                Phase = #phase{} ->
                    Cmd = #cmd{time = Time,
                               msg_id = MsgId,
                               mfa = {mafia, end_phase, [GNum, MsgId]}},
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    mafia_time:end_phase(G, Phase, Time),
                    done
            end;
        {?error, _} = E -> E
    end.

%% - add unend_phase putting previous phase correct number hours (48/24) later
%%   than now
unend_phase(GNum, MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_phase, [GNum, MsgId]}},
            ?man(Time, {'UNDO', Cmd}),
            mafia_file:manual_cmd_from_file(G, Cmd),
            mafia_time:unend_phase(G, M);
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Move next and all following deadlines forward or backward in time
%% keeping the day/night lengths
%% @end
%% -----------------------------------------------------------------------------
-spec move_next_deadline(GNum :: game_num(),
                         msg_id(),
                         later | earlier,
                         hour() | {hour(), minute()})
                        -> term().
move_next_deadline(GNum, MsgId, Direction, TimeDiff) when is_integer(MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            {Reply, _} =
                mafia_time:move_next_deadline(G, M, Direction, TimeDiff),
            if Reply == ok ->
                    Cmd =
                        #cmd{time = M#message.time,
                             msg_id = MsgId,
                             mfa = {mafia, move_next_deadline,
                                    [GNum, MsgId, Direction, TimeDiff]}},
                    ?man(M#message.time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd);
               true ->
                    ok
            end,
            Reply;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc End the whole game by giving GM msg_id where game end was proclaimed.
%% @end
%% -----------------------------------------------------------------------------
%% Example: mafia:end_game(1427800).
-spec end_game(GNum :: game_num(),
               MsgId :: msg_id()) -> term().
end_game(GNum, MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [GNum, MsgId]}},
            ?man(Time, Cmd),
            mafia_file:manual_cmd_to_file(G, Cmd),
            {Reply, _G2} = mafia_time:end_game(M, G),
            Reply;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Remove the changes of the end_game command.
%% @end
%% -----------------------------------------------------------------------------
%% Example: mafia:unend_game(1427800).
-spec unend_game(GNum :: game_num(),
                 MsgId :: msg_id()) -> term().
unend_game(GNum, MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [GNum, MsgId]}},
            ?man(Time, {'UNDO', Cmd}),
            mafia_file:manual_cmd_from_file(G, Cmd),
            {Reply, _G2} = mafia_time:unend_game(G),
            Reply;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Read the GM message and add good comment about who the dead player was.
%% @end
%% -----------------------------------------------------------------------------
-spec replace_player(GNum :: game_num(),
                     MsgId :: msg_id(),
                     OldPlayer :: string(),
                     NewPlayer :: string()) -> term().
replace_player(GNum, MsgId, OldPlayer, NewPlayer) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            case mafia_op:replace_player(G, M,
                                         NewPlayer,
                                         OldPlayer) of
                {ok, _} ->
                    Cmd =
                        #cmd{time = M#message.time,
                             msg_id = MsgId,
                             mfa = {mafia, replace_player,
                                    [GNum, MsgId, OldPlayer, NewPlayer]}},
                    ?man(M#message.time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    ok;
                _ ->
                    replace_failed
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Set the #death.msgid, time, to point to DeathMsgId, MsgId need to be
%% set to the last death announcement or later.
%% @end
%% -----------------------------------------------------------------------------
%% mafia:set_death_msgid(1468321, "Maniac", 1468307, "he was Big Ham, the "
%% "Mafia Goon.").
set_death_msgid(GNum, MsgId, Player, DeathMsgId, DeathComment) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Site = G#mafia_game.site,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, set_death_msgid,
                              [GNum, MsgId, Player, DeathMsgId, DeathComment]}},
            PlayerB = unicode:characters_to_binary(Player),
            case mafia_op:set_death_msgid(G, M,
                                          PlayerB,
                                          ?rmess({DeathMsgId, Site}),
                                          DeathComment) of
                ok ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    {set_death_msgid, Player, DeathMsgId};
                {?error, _} = E -> E
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Read the GM message and add good comment about who the dead player was.
%% @end
%% -----------------------------------------------------------------------------
-spec kill_player(GNum :: game_num(),
                  MsgId :: msg_id(),
                  Player :: string(),
                  Comment :: string())
                 -> {player_killed, #phase{}} |
                    {?error, msg_not_found | game_not_found} |
                    {player_other_case | not_remaining_player, string()}.
kill_player(GNum, MsgId, Player, Comment) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, kill_player,
                              [GNum, MsgId, Player, Comment]}},
            PlayerB = ?unicode_noesc_binary(Player),
            case mafia_op:kill_player(G, M, PlayerB, Comment) of
                {{ok, DeathPhase}, _G2} ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    ?regen_history(mankill, M, G),
                    {player_killed, DeathPhase};
                {not_remaining_player, _G2} ->
                    case ?ruser(PlayerB, G#mafia_game.site) of
                        [] ->
                            {player_no_exist, Player};
                        [#user{name = {NameB, _}}] ->
                            if NameB /= PlayerB ->
                                    {player_other_case,
                                     ?unicode_noesc_list(NameB)};
                               true ->
                                    {not_remaining_player,
                                     ?unicode_noesc_list(PlayerB)}
                            end
                    end
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc Read the GM message and add good comment about who the dead player was.
%% @end
%% -----------------------------------------------------------------------------
-spec resurrect_player(GNum :: game_num(),
                       MsgId :: msg_id(),
                       Player :: string())
                 -> ok |
                    {?error, msg_not_found | game_not_found} |
                    {?error, player_not_dead}.
resurrect_player(GNum, MsgId, Player) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, resurrect_player,
                              [GNum, MsgId, Player]}},
            PlayerB = unicode:characters_to_binary(Player),
            case mafia_op:resurrect_player(G, M, PlayerB) of
                {ok, _G2} ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    ok;
                {?error, player_not_dead} ->
                    {?error, player_not_dead}
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------

-spec ignore_message(GNum :: game_num(),
                     MsgId :: msg_id())
                    -> ok | {?error, msg_not_found | game_not_found}.
ignore_message(GNum, MsgId) when is_integer(GNum), is_integer(MsgId)  ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, ignore_message,
                              [GNum, MsgId]}},
            case mafia_file:manual_cmd_to_file(G, Cmd) of
                added ->
                    timer:sleep(200),
                    mafia_data:refresh_votes(GNum),
                    ok;
                not_added ->
                    ok
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------

-spec add_gm(GNum :: game_num(),
             MsgId :: msg_id(),
             User :: string())
            -> ok |
               {?error, msg_not_found | game_not_found} |
               {?error, term()}.
add_gm(GNum, MsgId, User) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, add_gm,
                              [GNum, MsgId, User]}},
            UserB = unicode:characters_to_binary(User),
            case mafia_op:add_gm(G, M, UserB) of
                ok ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    ok;
                {?error, _} = E -> E
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------

-spec find_mess_game(GNum :: game_num(),
                     MsgId :: msg_id())
                    -> {ok, #mafia_game{}, #message{}} |
                       {?error, msg_not_found | game_not_found}.
find_mess_game(GNum, MsgId) when is_integer(MsgId) ->
    case ?rgame(GNum) of
        [] -> {?error, game_not_found};
        [G = #mafia_game{thread_id = ThId}] ->
            case ?rmess({MsgId, G#mafia_game.site}) of
                [M = #message{thread_id = ThId}] -> {ok, G, M};
                _ -> {?error, msg_not_found}
            end
    end.

game_for_thid(ThId) ->
    Pattern = mnesia:table_info(mafia_game, wild_pattern),
    MatchHead = Pattern#mafia_game{thread_id = '$1'},
    Guard = [{'==', '$1', ThId}],
    Result = '$_',
    MatchExpr2 = [{MatchHead, Guard, [Result]}],
    mnesia:dirty_select(mafia_game, MatchExpr2).

%% load all beams in local dir
l() ->
    {ok, Files} = file:list_dir("patches"),
    Beams = [?l2a(?lrev(ModRev))
             || "maeb." ++ ModRev
                    <- [?lrev(F) || F <- Files]],
    Beams2 = (Beams -- [mafia]) ++ [mafia],
    [begin
         code:purge(M),
         code:load_file(M),
         case lists:keyfind(test, 1, M:module_info(exports)) of
             {test, 0} -> eunit:test(M);
             false -> ok
         end,
         M end
     || M <- Beams2].

show_settings() ->
    PrintSettings =
        fun(K) -> Setting = hd(mnesia:dirty_read(?kv_store, K)),
                  SetKey = element(2, Setting),
                  SetVal = element(3, Setting),
                  io:format("~p: ~p\n", [SetKey, SetVal])
        end,
    [PrintSettings(K) || K <- all_keys(?kv_store)],
    ok.

%% exp
show_all_users() ->
    show_usersI(?standard_io, all_keys(user), ?all).

%% exp
show_all_users(Site) when is_atom(Site) ->
    UKeys = get_user_keys_for_site(Site),
    show_usersI(?standard_io, UKeys, ?all);
show_all_users(Search) when is_list(Search) ->
    UserKeys = match_user_keys(Search),
    show_usersI(?standard_io, UserKeys, ?all).

get_user_names_for_site(Site) ->
    [unicode:characters_to_list(UserB)
     || UserB <- get_user_names_for_site_binary(Site)].

get_user_names_for_site_binary(Site) ->
    [begin
         [#user{name = {User, _}}] = mafia_lib:ruserUB(UserB, Site),
         User
     end
     || {UserB, _} <- get_user_keys_for_site(Site)].

get_user_keys_for_site(Site) ->
    if Site == ?all -> all_keys(user);
       ?true -> [UK || UK = {_, USite} <- all_keys(user),
                       USite == Site]
    end.

%% exp
show_all_users(Site, ?return_text) ->
    UKeys = get_user_keys_for_site(Site),
    show_usersI(?return_text, UKeys, ?all).

show_users(UserKeys, M) ->
    show_usersI(?standard_io, UserKeys, M).

show_usersI(Mode, UserKeys, M) when M == alias; M == ?all ->
    Header = print_header_w_fmt(Mode),
    Text = [begin
                U = hd(?ruserUB(UserKey)),
                if M == all; U#user.aliases /= [] ->
                        pr_user_line(Mode, U);
                   true -> ""
                end
            end
            || UserKey <- UserKeys],
    case Mode of
        ?return_text -> [Header, Text];
        ?standard_io -> ok
    end.

show_game_users(GNum) ->
    Mode = ?standard_io,
    show_game_users(Mode, GNum).

show_game_users(Mode, GNum) ->
    Header = print_header_w_fmt(Mode),
    Text =
        case ?rgame(GNum) of
            [G] ->
                [begin [U] = ?ruser(UName, G#mafia_game.site),
                       pr_user_line(Mode, U)
                 end
                 || UName <- mafia_lib:all_players_in_game(G)];
            _ ->
                "error: game_not_found"
        end,
    case Mode of
        ?return_text -> [Header, Text];
        ?standard_io -> ok
    end.

pr_user_line(Mode, U) ->
    Aliases = string:join(
                ["\"" ++ ?b2l(AliasB) ++ "\""
                 || AliasB <- U#user.aliases], ", "),
    print_w_fmt(Mode,
                [?b2l(?e1(U#user.name)),
                 case U#user.pw_hash of
                     ?undefined -> " ";
                     _ -> "*"
                 end,
                 ?a2l(U#user.site),
                 Aliases
                ]).

print_header_w_fmt(Mode) ->
    [print_w_fmt(Mode, ["User", " ", "Site", "Aliases"]),
     print_w_fmt(Mode, ["----", " ", "----", "-------"])].

print_w_fmt(Mode, Args) ->
    Fmt = "~30s~-1s~-6s ~s~n",
    do_print(Mode, Fmt, Args).

all_keys(Tab) -> mafia_lib:all_keys(Tab).

match_user_keys(Search) ->
    [UserKey || UserKey = {UserUB, _} <- all_keys(user),
                0 /= string:str(?b2l(UserUB), ?l2u(Search))].

show_all_aliases() ->
    show_aliases(all).

-spec show_aliases(UserSearch :: string()) -> ok | {error, Reason :: term()}.
show_aliases(all) ->
    Keys = all_keys(user),
    show_users(Keys, alias);
show_aliases(Search) ->
    io:format("Search: ~s\n", [Search]),
    UserKeys = match_user_keys(Search),
    [show_aliasesI(UKey) || UKey <- UserKeys],
    ok.

do_print(?standard_io, Fmt, Args) -> io:format(Fmt, Args);
do_print(?return_text, Fmt, Args) -> io_lib:format(Fmt, Args).

show_aliasesI(UserSite) ->
    case ?ruserUB(UserSite) of
        [] -> {error, user_not_found};
        [#user{name = {Name, _}, aliases = Aliases}] ->
            io:format("Found: ~s\nAliases: ~p\n",
                      [?b2l(Name),
                       [?b2l(AliasB) || AliasB <- Aliases]])
    end.

-spec add_user(Name :: (string() | binary()),
               site())
              -> ok | {error, eexists}.
add_user(NameB, Site)
  when is_binary(NameB), ?IS_SITE_OK(Site) ->
    add_userI(NameB, Site);
add_user(Name, Site)
  when is_list(Name), ?IS_SITE_OK(Site) ->
    add_userI(Name, Site).

add_userI(Name, Site) ->
    NameB = ?unicode_noesc_binary(Name),
    NameUB = ?unicode_noesc_upper_binary(Name),
    case ?ruser(NameB, Site) of
        [] ->
            User = #user{name_upper = {NameUB, Site},
                         name = {NameB, Site},
                         site = Site,
                         verification_status = verified},
            ?dwrite_user(User);
        [#user{}] ->
            {error, eexists}
    end.

remove_user(Name, Site) ->
    case ?ruser(Name, Site) of
        [] ->
            {error, enoexists};
        [#user{} = U] ->
            io:format("User = ~p\n", [U]),
            Answer = io:get_line(?l2a("Are you really sure you want to "
                                      "delete this user (NO/yes)> ")),
            case string:to_upper(Answer) of
                "YES" ++ _ ->
                    mnesia:dirty_delete(?user, U#user.name_upper),
                    {user_deleted, Name};
                _ ->
                    user_not_deleted
            end
    end.

-spec add_alias(User :: string(),
                site(),
                Alias :: string())
               -> ok | {error, Reason :: term()}.
add_alias(User, Site, Alias)
  when is_list(User),
       is_list(Alias),
       ?IS_SITE_OK(Site) ->
    UserB = ?l2b(User),
    AliasB = ?l2b(Alias),
    AliasUB = ?l2ub(Alias),
    case ?ruser(User, Site) of
        [] -> {error, user_not_found};
        [#user{} = U] when ?e1(U#user.name) /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            AliasesUB = [?b2ub(AlB) || AlB <- U#user.aliases],
            case lists:member(AliasUB, AliasesUB) of
                true ->
                    {error, alias_exist_already};
                false ->
                    ?dwrite_user(U#user{aliases = [AliasB | AliasesB]}),
                    ok
            end
    end.

-spec remove_alias(User :: string(),
                   site(),
                   Alias :: string())
                  -> ok | {error, Reason :: term()}.
remove_alias(User, Site, Alias) ->
    UserB = ?l2b(User),
    AliasB = ?l2b(Alias),
    case ?ruser(User, Site) of
        [] -> {error, user_not_found};
        [#user{} = U] when ?e1(U#user.name) /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            case lists:member(AliasB, AliasesB) of
                true ->
                    NewAliasesB = AliasesB -- [AliasB],
                    ?dwrite_user(U#user{aliases = NewAliasesB}),
                    ok;
                false ->
                    {error, alias_does_not_exist}
            end
    end.

%% Export user table to file
export_user_data() ->
    %% to file
    Keys = all_keys(user),
    Recs = [hd(?ruserUB(UserSite)) || UserSite <- Keys],
    file:write_file("user_data.txt",
                    io_lib:format("~p.\n", [Recs]),
                    [write]).

%% Import user data from file
import_user_data() ->
    %% import from file and MERGE
    import_user_data(file:consult("user_data.txt")).

import_user_data({error, _} = Error) -> Error;
import_user_data({ok, [Users]}) ->
    RespF =
        fun() ->
                Resp =
                    io:get_line(
                      ?l2a("Action I = store Imported User, "
                           "D = keep DB unchanged, X = eXit (D): ")),
                %% io:format("Resp ~p\n", [Resp]),
                case string:to_upper(Resp) of
                    "I"++_ -> ?take_import;
                    "X"++_ -> ?stop;
                    _ -> ?continue
                end
        end,
    Cmp = fun(_Uimp, stop) -> stop;
             (Uimp, _) ->
                  KeyImp = element(2, Uimp),
                  Udb = case ?ruserUB(KeyImp) of
                            [] -> no_exist;
                            [U] -> U
                        end,
                  if Uimp /= Udb ->
                          io:format("Imp ~999p\n", [Uimp]),
                          io:format("DB  ~999p\n", [Udb]),
                          Action = RespF(),
                          io:format("~p\n", [Action]),
                          if Action == ?take_import -> ?dwrite_user(Uimp);
                             true -> ignore
                          end,
                          Action;
                     true ->
                          io:format("Imp=DB ~999p\n", [Uimp]),
                          %% io:format("---\n", []),
                          ?continue
                  end
          end,
    case lists:foldl(Cmp, ?continue, Users) of
        ?continue ->
            done_all;
        R -> R
    end.

print_all_cnts() -> mafia_lib:print_all_cnts().

print_all_cnts(N) -> mafia_lib:print_all_cnts(N).

save_cnts_to_file() -> mafia_lib:save_cnts_to_file().

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% No-one seems to use this one.
cmp_vote_raw() ->
    ThId = ?getv(?thread_id),
    DayNum = 1,
    #mafia_day{votes = GVotes} = ?rday(ThId, DayNum),
    [begin
         VoteSum =
             [{?b2l(V#vote.vote), ?b2l(V#vote.raw), V#vote.valid}
              || V <- Votes],
         {?b2l(User), VoteSum}
     end || {User, Votes} <- GVotes].

%% =============================================================================
%% Bugfixes
%% =============================================================================

%% Fix faulty User names in user and message tables 2018-07-08
fix_user_bug() ->
    fix_user_bug(report_only).

-spec fix_user_bug(report_only | fix_errors) -> ok.
fix_user_bug(Mode) ->
    Faults =
        [{"bo_sox48  (4271", "bo_sox48"},
         {"brainbomb  (456", "brainbomb"},
         {"Chaqa  (3134", "Chaqa"},
         {"ghug  (3734", "ghug"},
         {"Jamiet99uk  (100", "Jamiet99uk"},
         {"ND  (647", "ND"},
         {"teacon7  (306", "teacon7"}],

    %% check/fix user table
    lists:foreach(fun(Fault) ->
                          fix_user(Mode, Fault)
                  end,
                  Faults),

    %% check/fix messages
    FaultsB = [{?l2b(Er), ?l2b(Co)} || {Er, Co} <- Faults],
    AMKs = mnesia:dirty_all_keys(message),
    lists:foreach(fun(MsgKey) ->
                          fix_message(Mode, MsgKey, FaultsB)
                  end,
                  AMKs).

fix_user(Mode, {Er, Co}) ->
    case {?ruser(Er, webDip), ?ruser(Co, webDip)} of
        {[], _} -> ok;
        {[ErUser], CoUsers} ->
            io:format("Error: ~p\n", [ErUser]),
            case {Mode, CoUsers} of
                {_, []} ->
                    io:format("No remove ~p. There is no correct user ~p.\n",
                              [Er, Co]);
                {fix_errors, [CoUser]} ->
                    io:format("Correct: ~p\n", [CoUser]),
                    %% remove error user
                    mnesia:dirty_delete(user, ErUser#user.name_upper);
                _ -> ok
            end
    end.

fix_message(Mode, MsgKey, FaultsB) ->
    [M] = ?rmess(MsgKey),
    U = M#message.user_name,
    U2 = fix_maybe_update_name(U, FaultsB),
    if U2 /= U ->
            io:format("~p\n~p -> ~p\n---\n",
                      [M, U, U2]),
            if Mode == fix_errors ->
                    M2 = M#message{user_name = U2},
                    ?dwrite_msg(M2);
               true -> ok
            end;
       true -> ok
    end.

%% return updated UserB
fix_maybe_update_name(UserB, FaultsB) ->
    lists:foldl(fun({ErB, CoB}, Acc) ->
                        case UserB of
                            ErB -> CoB;
                            _ -> Acc
                        end
                end,
                UserB,
                FaultsB).
