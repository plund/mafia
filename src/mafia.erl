-module(mafia).

-include("mafia.hrl").
%% coordinate poll_timer and dl_timer. "No poll at dl"
%% Make use of dl_poll_info.txt when generating history pages at:
%%    1) deadline 2) player death at End of Day/Night
%% do LATE read of signup thread
%% Add signup_thid info to all games.
%% Mnesia overloaded fixes
%% try autostart again
%% vhosts in inets - no support - try patch inets :)
%% LOW - Add Last&more link also on game_end page
%% - balki vote on Jamie g29? d1 46:13 did not get the correct part in console
%%   due to unicode
%% http://mafia_test.peterlund.se/e/web/msgs?part=p3-5#msg_id=1480166
%% Force split long lines in thread
%% - split mafia_print. stats and tracker into separate modules?
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
%%    - use quickcheck license

%% interface
-export([
         start/0,
         stop/0,

         game_start/2, %% old
         end_phase/1,
         unend_phase/1,
         end_phase/2, %% deprecated
         move_next_deadline/3,
         end_game/1,
         unend_game/1,
         switch_to_game/1,
         set_signup_thid/2,
         set_role_pm/2,
         switch_thread_id/2,
         game_thread/1,
         initiate_game/1,
         initiate_game/2,
         write_settings_file/1,
         remove_not_running_game/1,
         delete_game_and_all_data/1,
         pregame_create/1, % old
         pregame_update/0, % old

         replace_player/3,
         kill_player/3,
         set_death_msgid/4,
         set_death_comment/3, %% deprecated

         print_votes/0,
         print_votes/1,
         print_votes/2,
         print_messages/1,

         man_downl/0,
         show_settings/0,
         refresh_messages/0,
         refresh_messages/1,
         refresh_votes/0,
         refresh_votes/1,

         show_all_users/0,
         show_all_users/1,
         show_all_aliases/0,
         show_aliases/1,
         add_user/1,
         remove_user/1,
         add_alias/2,
         remove_alias/2,
         export_user_data/0,
         import_user_data/0,

         setup_mnesia/0,
         remove_mnesia/0,
         upgrade/0,
         upgrade/1
        ]).

%% libary
-export([game_for_thid/1,
         pages_for_thread/1,
         last_msg_in_thread/1
        ]).

%% utilities
-export([show_game_pages/1,
         show_game_data/1,

         l/0,

         print_all_cnts/0,
         print_all_cnts/1,
         save_cnts_to_file/0,
         add_sim_gm_message/4,
         rm_sim_gm_message/2
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
print_votes(DayNum, DoN) -> mafia_print:print_votes(DayNum, DoN).
print_messages(User) -> mafia_print:print_messages(User).
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
        [#mafia_game{thread_id = ThId}] when is_integer(ThId) ->
            io:format("Game page keys and messages\n"),
            show_game_pagesI(ThId);
        [_] ->
            io:format("Game has no thread id\n")
    end.

show_game_pagesI(ThId) ->
    [{P, length((hd(?rpage(T,P)))#page_rec.message_ids)}
     || {T,P} <- mafia_lib:all_page_keys(ThId)].

show_game_data(GNum) ->
    case ?rgame(GNum) of
        [] ->
            io:format("No game record exist\n");
        [G] ->
            io:format("Game record exist\n"),
            ThId = G#mafia_game.thread_id,
            io:format("Thread id ~p\n", [ThId]),
            PageKeys = [K || K = {Th, _} <- all_keys(page_rec),
                             Th == ThId],
            io:format("Num Page records ~p\n", [length(PageKeys)]),
            MsgIds = [MsgId || {_, MsgId} <- mafia_lib:all_msgids(ThId),
                               [] /= ?rmess(MsgId)
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
initiate_game(GNum) when is_integer(GNum) ->
    initiate_game(GNum, []).

initiate_game(GNum, GMs) when is_integer(GNum) ->
    DoFun = fun(G) ->
                    GMsB = get_user_list(GMs),
                    ?dwrite_game(G#mafia_game{game_num = GNum,
                                              gms = GMsB}),
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

get_user_list([]) -> [];
get_user_list([User | T]) ->
    case mafia_lib:ruserUB(User) of
        [#user{name = NameB}] ->
            [NameB | get_user_list(T)];
        _ -> get_user_list(T)
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
                    io:format("WARNING: This is the current game. "
                              "Consider switch_to_game first.\n");
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
%% @doc Create and Switch to game that has not started yet
%% @end
%% -----------------------------------------------------------------------------
pregame_create(GNum) when is_integer(GNum) ->
    Create = fun() -> mafia_db:write_game(GNum) end,
    ?set(game_key, GNum),
    pregame(Create).

pregame_update() ->
    GNum = ?getv(game_key),
    Create = fun() -> mafia_db:rewrite_game(GNum) end,
    pregame(Create).

pregame(Create) ->
    Create(),
    mafia:stop(),     %% Set gen_server #state.game_num
    mafia:start(),
    mafia_web:poll(). %% creates text file

%% -----------------------------------------------------------------------------
%% @doc Switch to other game
%% @end
%% -----------------------------------------------------------------------------
-spec switch_to_game(GNum :: game_num()) -> term().
switch_to_game(GNum) ->
    ?set(game_key, GNum), %% Must have it set as default game when omitted
    case ?rgame(GNum) of
        [#mafia_game{deadlines = DLs}] when DLs /= [] ->
            mafia_web:change_current_game(GNum);
        [G = #mafia_game{deadlines = DLs}] when DLs == [] ->
            G2 = mafia_time:initial_deadlines(G),
            ?dwrite_game(G2),
            mafia_web:change_current_game(GNum);
        [] ->
            {error, game_noexist}
    end.

-spec set_signup_thid(GNum :: game_num(),
                      thread_id()) -> ok | {error, atom()}.
set_signup_thid(GNum, SuThId) when is_integer(SuThId) ->
    case ?rgame(GNum) of
        [G] when G#mafia_game.thread_id == ?undefined ->
            ?dwrite_game(G#mafia_game{signup_thid = SuThId,
                                      page_to_read = 1}),
            ok;
        [G] when is_integer(G#mafia_game.thread_id) ->
            ?dwrite_game(G#mafia_game{signup_thid = SuThId}),
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
    ?dwrite_game(G#mafia_game{role_pm = UrlVal}),
    ok;
set_role_pmI(_, _) -> {error, no_game_found}.

%% -----------------------------------------------------------------------------
%% @doc Switch to other thread_id() and reread all info
%% @end
%% -----------------------------------------------------------------------------
-spec switch_thread_id(GNum :: game_num(),
                       thread_id()) -> term().
switch_thread_id(GNum, NewThId) when is_integer(NewThId) ->
    case ?rgame(GNum) of
        [G] ->
            ?dwrite_game(G#mafia_game{thread_id = NewThId}),
            mafia_data:refresh_messages(GNum),
            mafia_data:refresh_votes(GNum),
            ok;
        _ ->
            {error, no_game}
    end.

game_thread(GNum) ->
    case ?rgame(GNum) of
        [G] -> G#mafia_game.thread_id;
        [] -> ?getv(?thread_id)
    end.

%% -----------------------------------------------------------------------------
%% @doc Create a game and translation in table and on file
%% @end
%% -----------------------------------------------------------------------------
-spec game_start(GNum :: integer(), ThId :: thread_id()) -> ok | term().
game_start(GNum, ThId) when is_integer(GNum), is_integer(ThId) ->
    ?set(game_key, GNum),
    ?set(thread_id, ThId),
    mafia_db:write_game(GNum, ThId).

%% -----------------------------------------------------------------------------
%% @doc End current phase with GM message and set next phase at
%% the game's local date and time given by the GM in the same message
%% @end
%% -----------------------------------------------------------------------------
end_phase(MsgId) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            case mafia_time:calculate_phase(G, Time) of
                #phase{don = ?game_ended} -> {?error, ?game_ended};
                Phase = #phase{} ->
                    Cmd = #cmd{time = Time,
                               msg_id = MsgId,
                               mfa = {mafia, end_phase, [MsgId]}},
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    mafia_time:end_phase(G, Phase, Time),
                    done
            end;
        {?error, _} = E -> E
    end.

%% - add unend_phase putting previous phase correct number hours (48/24) later
%%   than now
unend_phase(MsgId) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_phase, [MsgId]}},
            ?man(Time, {'UNDO', Cmd}),
            mafia_file:manual_cmd_from_file(G, Cmd),
            mafia_time:unend_phase(G, M);
        {?error, _} = E -> E
    end.


%% OLD DEPRECATED - should call end_phase/1 and then
%% Example: mafia:end_phase(1427098, {{2016, 12, 13}, {18,0,0}}).
-spec end_phase(MsgId :: msg_id(),
                TimeNextDL :: datetime()) -> ok.
end_phase(MsgId, _TimeNextDL) ->
    case ?rmess(MsgId) of
        [] -> msg_not_found;
        [_] ->
            end_phase(MsgId),
            move_next_deadline(MsgId, earlier, 24)
    end.


%% -----------------------------------------------------------------------------
%% @doc Move next and all following deadlines forward or backward in time
%% keeping the day/night lengths
%% @end
%% -----------------------------------------------------------------------------
-spec move_next_deadline(msg_id(), later|earlier, hour() | {hour(), minute()})
                        -> term().
move_next_deadline(MsgId, Direction, TimeDiff) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            {Reply, _} =
                mafia_time:move_next_deadline(G, M, Direction, TimeDiff),
            if Reply == ok ->
                    Cmd =
                        #cmd{time = M#message.time,
                             msg_id = MsgId,
                             mfa = {mafia, move_next_deadline,
                                    [MsgId, Direction, TimeDiff]}},
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
-spec end_game(MsgId :: msg_id()) -> term().
end_game(MsgId) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [MsgId]}},
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
-spec unend_game(MsgId :: msg_id()) -> term().
unend_game(MsgId) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [MsgId]}},
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
-spec replace_player(MsgId :: msg_id(),
                     OldPlayer :: string(),
                     NewPlayer :: string()) -> term().
replace_player(MsgId, OldPlayer, NewPlayer) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            case mafia_vote:replace_player(G, M,
                                           NewPlayer,
                                           OldPlayer) of
                {ok, _} ->
                    Cmd =
                        #cmd{time = M#message.time,
                             msg_id = MsgId,
                             mfa = {mafia, replace_player,
                                    [MsgId, OldPlayer, NewPlayer]}},
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
set_death_msgid(MsgId, Player, DeathMsgId, DeathComment) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, set_death_msgid,
                              [MsgId, Player, DeathMsgId, DeathComment]}},
            PlayerB = ?l2b(Player),
            case mafia_vote:set_death_msgid(G, M,
                                            PlayerB,
                                            ?rmess(DeathMsgId),
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
-spec kill_player(MsgId :: msg_id(),
                  Player :: string(),
                  Comment :: string())
                 -> {player_killed, #phase{}} |
                    {?error, msg_not_found | game_not_found} |
                    {player_other_case | not_remaining_player, string()}.
kill_player(MsgId, Player, Comment) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, kill_player,
                              [MsgId, Player, Comment]}},
            PlayerB = ?l2b(Player),
            case mafia_vote:kill_player(G, M, PlayerB, Comment) of
                {{ok, DeathPhase}, _G2} ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(G, Cmd),
                    mafia_web:regen_history(M, G),
                    {player_killed, DeathPhase};
                {not_remaining_player, _G2} ->
                    case ?ruser(Player) of
                        [] ->
                            {player_no_exist, Player};
                        [#user{name = NameB}] ->
                            if NameB /= PlayerB ->
                                    {player_other_case, ?b2l(NameB)};
                               true ->
                                    {not_remaining_player, ?b2l(PlayerB)}
                            end
                    end
            end;
        {?error, _} = E -> E
    end.

%% deprecated - kept only for command_files
set_death_comment(MsgId, Player, Comment) ->
    kill_player(MsgId, Player, Comment).

%% -----------------------------------------------------------------------------

-spec find_mess_game(MsgId :: msg_id())
                    -> {ok, #mafia_game{}, #message{}} |
                       {?error, msg_not_found | game_not_found}.
find_mess_game(MsgId) ->
    case ?rmess(MsgId) of
        [] -> {?error, msg_not_found};
        [M] ->
            ThId = M#message.thread_id,
            case game_for_thid(ThId) of
                [] -> {?error, game_not_found};
                [G] -> {ok, G, M}
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

pages_for_thread(ThId) ->
    mafia_lib:pages_for_thread(ThId).

-spec last_msg_in_thread(ThId :: integer()) -> none | #message{}.
last_msg_in_thread(ThId) when is_integer(ThId) ->
    case pages_for_thread(ThId) of
        [] -> none;
        Ps ->
            LastPage = lists:last(Ps),
            PageRec = hd(?rpage(ThId, LastPage)),
            MsgId = lists:last(PageRec#page_rec.message_ids),
            hd(?rmess(MsgId))
    end.

show_all_users() ->
    io:format("All Users\n"),
    show_usersI(?standard_io, all_keys(user), all).

show_all_users(?return_text) ->
    show_usersI(?return_text, all_keys(user), all);
show_all_users(Search) ->
    UserKeys = match_user_keys(Search),
    show_usersI(?standard_io, UserKeys, all).

show_users(UserKeys, M) ->
    show_usersI(?standard_io, UserKeys, M).

show_usersI(Mode, UserKeys, M) when M == alias; M == all ->
    H1 = print(Mode, ["User", "Aliases"]),
    H2 = print(Mode, ["----", "-------"]),
    Text =
        [begin
             U = hd(?ruserUB(UserUB)),
             if M == all; U#user.aliases /= [] ->
                     case U#user.aliases of
                         [] -> print(Mode, [?b2l(U#user.name)]);
                         _ -> print(Mode,
                                    [?b2l(U#user.name),
                                     string:join(
                                       ["\"" ++ ?b2l(AlB) ++ "\""
                                        || AlB <- U#user.aliases], ", ")])
                     end;
                true -> ""
             end
         end
         || UserUB <- UserKeys],
    case Mode of
        ?return_text -> [H1, H2, Text];
        ?standard_io -> ok
    end.

all_keys(Tab) -> mafia_lib:all_keys(Tab).

match_user_keys(Search) ->
    [UserUB || UserUB <- all_keys(user),
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

print(Mode, Args) ->
    Fmt =
        case {Mode, length(Args)} of
            {?return_text, 1} -> "~s\n";
            {?return_text, 2} -> "~s <=> ~s\n";
            {?standard_io, 1} -> "~-20s\n";
            {?standard_io, 2} -> "~-20s ~s\n"
          end,
    do_print(Mode, Fmt, Args).

do_print(?standard_io, Fmt, Args) -> io:format(Fmt, Args);
do_print(?return_text, Fmt, Args) -> io_lib:format(Fmt, Args).

show_aliasesI(User) ->
    case ?ruserUB(User) of
        [] -> {error, user_not_found};
        [#user{} = U] ->
            io:format("Found: ~s\nAliases: ~p\n",
                      [?b2l(U#user.name),
                       [?b2l(AlB) || AlB <- U#user.aliases]])
    end.

-spec add_user(Name :: (string() | binary()))
              -> ok | {error, eexists}.
add_user(NameB) when is_binary(NameB) ->
    add_user(?b2l(NameB));
add_user(Name) when is_list(Name) ->
    case ?ruser(Name) of
        [] ->
            NameU = string:to_upper(Name),
            User = #user{name_upper = ?l2b(NameU),
                         name = ?l2b(Name),
                         verification_status = verified},
            ?dwrite_user(User);
        [#user{}] ->
            {error, eexists}
    end.

remove_user(Name) ->
    case ?ruser(Name) of
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

-spec add_alias(User :: string(), Alias :: string())
               -> ok | {error, Reason :: term()}.
add_alias(User, Alias) ->
    UserB = ?l2b(User),
    AliasB = ?l2b(Alias),
    AliasUB = ?l2ub(Alias),
    case ?ruser(User) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
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

-spec remove_alias(User :: string(), Alias :: string())
                  -> ok | {error, Reason :: term()}.
remove_alias(User, Alias) ->
    UserB = ?l2b(User),
    AliasB = ?l2b(Alias),
    case ?ruser(User) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
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
    Recs = [hd(?ruserUB(UserUB)) || UserUB <- Keys],
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
                Resp =io:get_line(
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

%% Use one message as template to create a new
%% put it first on any page
%% refresh({upto, })
add_sim_gm_message(Page, SimMsgId, TplMsgId, Text) ->
    ThId = ?getv(?game_key),
    case {?rgame(ThId),
          ?rpage(ThId, Page),
          ?rmess(TplMsgId),
          ?rmess(SimMsgId)} of
        {[], _, _, _} -> no_game;
        {_, [], _, _} -> no_page;
        {_, _, [], _} -> no_template_message;
        {_, _, _, [_]} -> message_exist_already;
        {[G], [P], [M], []} ->
            GM1 = hd(G#mafia_game.gms),
            SimMsg = M#message{user_name = GM1,
                               msg_id = SimMsgId,
                               page_num = Page,
                               message = ?l2b(Text)},
            P2 = P#page_rec{message_ids = [SimMsgId | P#page_rec.message_ids]},
            ?dwrite_msg(SimMsg),
            ?dwrite_page(P2),
            ok
    end.


rm_sim_gm_message(Page, SimMsgId) ->
    ThId = ?getv(?game_key),
    case ?rpage(ThId, Page) of
        [P] ->
            case P#page_rec.message_ids of
                [SimMsgId|T]  ->
                    P2 = P#page_rec{message_ids = T},
                    mnesia:dirty_delete(messsage, SimMsgId),
                    ?dwrite_page(P2),
                    sim_msg_deleted;
                _ ->
                    {page_has_not_msg_id, Page, SimMsgId}
            end;
        [] ->
            {page_do_not_exist, ThId, Page}
    end.

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
