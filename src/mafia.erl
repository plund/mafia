-module(mafia).

-include("mafia.hrl").
%% Add page for serverkeeper to initiate_new_game
%%  - will a game with signup only, poll the thread?
%%    - dont think so, BUT it should
%% Poll signup until game start
%% Poll also game thread before game start
%% Why is GNum needed (default game_key) to show a message using "msg"?
%%  Answer: we display phase and a link to page and pages in a game
%%  Conclusion: Change to always require g=<gamenum> (remove use of game_key)
%%    OR do not display phase and links when GNum is missing.
%% Vote switch should remove end vote
%% - disregard votes when more than one vote is present in the same message
%% - make sure vote order applies also to ##end
%% Coordinate poll_timer and dl_timer. "No poll at dl"
%% Make use of dl_poll_info.txt when generating history pages at:
%%    1) deadline 2) player death at End of Day/Night
%%    - use quickcheck license
%% Bug?: Why does not manual poll() trigger vote counting Site?
%% Useful? io:format("~*s~*s~*s~n", [-15, "aaa", 5, "bbb", -5, "cc"]).
%% Multiple active games and pollings
%% - do LATE read of signup thread
%% try autostart again
%% vhosts in inets - no support - try patch inets :)
%% LOW - Add Last&more link also on game_end page
%% - balki vote on Jamie g29? d1 46:13 did not get the correct part in console
%%   due to unicode
%% http://mafia_test.peterlund.se/e/web/msgs?part=p3-5#msg_id=1480166
%% Instead add ServerKeeper/GM commands:
%% ##bot endgame <msgid> | unendgame
%% ##bot endphase|unendphase <msgid>
%% ##bot replaceplayer <msgid> <old> <new>
%% ##bot deadline <msgid> earlier|later <time>
%% ##bot assistant add|remove <msgid> <player>
%%   - define how and when to use a smarter vote reader!! ??
%% - Display msgs since last login with a browser (cookie)
%% - fix a better player name recognition in votes and deaths?
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

         initiate_game/1,
         initiate_game/2,
         initiate_game/3,
         write_settings_file/1,
         remove_not_running_game/1,
         delete_game_and_all_data/1,

         replace_player/4,
         kill_player/4,
         ignore_message/2,

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

         show_game_users/1,
         show_game_users/2,
         show_all_users/0,
         show_all_users/1,
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
         show_game_data/1,

         l/0,

         print_all_cnts/0,
         print_all_cnts/1,
         save_cnts_to_file/0
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

show_game_data(GNum) ->
    case ?rgame(GNum) of
        [] ->
            io:format("No game record exist\n");
        [#mafia_game{thread_id = ThId, site = Site}] ->
            io:format("Game record exist\n"),
            io:format("Thread id ~p\n", [ThId]),
            PageKeys = [K || K = {Th, _, _} <- all_keys(page_rec),
                             Th == ThId],
            io:format("Num Page records ~p\n", [length(PageKeys)]),
            MsgIds = [MsgId || {_, MsgId} <- mafia_lib:all_msgids(ThId, Site),
                               [] /= ?rmess({MsgId, Site})
                     ],
            io:format("There are ~p messages in mnesia for this game\n",
                      [length(MsgIds)])
    end,
    DayKeys = mafia_lib:all_day_keys(GNum),
    io:format("Num Day records ~p\n", [length(DayKeys)]),
    io:format("Day keys ~p\n", [DayKeys]),

    IsStatKey = fun(Id, {_, Id}) -> true;
                   (Id, {_, Id, _}) -> true;
                   (_, _) -> false
                end,
    StatKeys = [K || K <- mnesia:dirty_all_keys(stat), IsStatKey(GNum, K)],
    io:format("Num Stat records ~p\n", [length(StatKeys)]).


%% -----------------------------------------------------------------------------
%% @doc Initiate game that has not started yet
%% @end
%% -----------------------------------------------------------------------------
initiate_game(GNum) ->
    initiate_game(GNum, [], ?webDip).

initiate_game(GNum, Site) when Site == ?webDip;
                               Site == ?vDip ->
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
                            ?b2l(G#mafia_game.name);
                       true -> "(no name)"
                    end,
            io:format("Deleting Game ~p - ~s\n",
                      [GNum, GName]),
            show_game_data(GNum),
            CurrentGameNum = ?getv(game_key),
            if GNum == CurrentGameNum ->
                    io:format("WARNING: This is the current game.\n");
               true -> ok
            end,
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
            case mafia_vote:replace_player(G, M,
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
            PlayerB = ?l2b(Player),
            case mafia_vote:set_death_msgid(G, M,
                                            PlayerB,
                                            ?rmess({DeathMsgId, Site}),
                                            DeathComment) of
                ok ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    %%io:format("Do mafia:refresh_votes().\n"),
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
            PlayerB = ?l2b(Player),
            case mafia_vote:kill_player(G, M, PlayerB, Comment) of
                {{ok, DeathPhase}, _G2} ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    game:regen_history(M, G),
                    {player_killed, DeathPhase};
                {not_remaining_player, _G2} ->
                    case ?ruser(Player, G#mafia_game.site) of
                        [] ->
                            {player_no_exist, Player};
                        [#user{name = {NameB, _}}] ->
                            if NameB /= PlayerB ->
                                    {player_other_case, ?b2l(NameB)};
                               true ->
                                    {not_remaining_player, ?b2l(PlayerB)}
                            end
                    end
            end;
        {?error, _} = E -> E
    end.

%% -----------------------------------------------------------------------------

ignore_message(GNum, MsgId) ->
    case find_mess_game(GNum, MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, ignore_message,
                              [GNum, MsgId]}},
            mafia_file:manual_cmd_to_file(G, Cmd);
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
        [G] ->
            case ?rmess({MsgId, G#mafia_game.site}) of
                [] -> {?error, msg_not_found};
                [M] -> {ok, G, M}
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

show_all_users() ->
    show_usersI(?standard_io, all_keys(user), all).

show_all_users(?return_text) ->
    show_usersI(?return_text, all_keys(user), all);
show_all_users(Search) ->
    UserKeys = match_user_keys(Search),
    show_usersI(?standard_io, UserKeys, all).

show_users(UserKeys, M) ->
    show_usersI(?standard_io, UserKeys, M).

show_usersI(Mode, UserKeys, M) when M == alias; M == all ->
    Header = print_header_w_fmt(Mode),
    Text =
        [begin
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
                 || UName <- G#mafia_game.players_orig];
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
add_user(NameB, Site) when is_binary(NameB) ->
    add_user(?b2l(NameB), Site);
add_user(Name, Site)
  when is_list(Name) andalso
       (Site == ?webDip orelse Site == ?vDip) ->
    case ?ruser(Name, Site) of
        [] ->
            NameU = string:to_upper(Name),
            User = #user{name_upper = {?l2b(NameU), Site},
                         name = {?l2b(Name), Site},
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
  when is_list(User) andalso is_list(Alias) andalso
       (Site == ?webDip orelse Site == ?vDip) ->
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
