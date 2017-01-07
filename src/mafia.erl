-module(mafia).

-include("mafia.hrl").
%% - Handle new player that is NOT yet in the DB.
%% - add/remove fake GM message to test commands
%% ? - align new deadlines to next full minute. - can it come off full minutes?
%% ? - GM command expand alias list (Manual exist already).
%%        - Game startup is too hard.

%% - Call the Game Status generation from the gen_server also for html variants
%%   when they are ready to be stored on file
%% - web:deliver game_status in parts out to browser?
%% - split mafia_print. stats and tracker into separate modules?
%% - implement the GM_commands. How to test them?
%%   - define now and when to use a smarter vote reader!!
%% - Display msgs since last login with a browser (cookie)
%% - be DAY/NIGHT sensitive when reading "Day/night ... has ended early"
%% ToDo:
%% - Use new DL calc and remove old calculation NEW: "get_some_extra_dls"
%% ***** - define deadline() :: {phase(), secs1970()} and change at all places.
%% - Fix proper server on lundata - start at MacOS reboot
%% - fix a better player name recognition in votes and deaths?
%% - check if abbrev code can loop forever
%% ?- downl and pps should look similar, Died/Vote messages and deadline markers

%% M25 GOD QT https://www.quicktopic.com/52/H/gBqFhw3Bidb
%% M25 spectator QT https://www.quicktopic.com/52/H/ZPja4vQgBFQ7

%% interface
-export([
         help/0,

         start/0,
         stop/0,
         stop_polling/0,
         start_polling/0,
         state/0,

         setup_mnesia/0,
         remove_mnesia/0,

         game_start/2,
         end_phase/1,
         unend_phase/1,
         end_phase/2, %% deprecated
         move_next_deadline/3,
         end_game/1,
         unend_game/1,
         switch_to_game/1,
         switch_to_game/2,

         replace_player/3,
         kill_player/3,
         set_death_comment/3, %% deprecated

         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2,
         print_messages/1,

         verify_new_user_list/1,
         downl/0,
         add_thread/2,
         rm_thread/1,
         show_settings/0,
         set_thread_id/1,
         refresh_votes/0,
         refresh_votes/1,

         show_all_users/0,
         show_all_users/1,
         show_all_aliases/0,
         show_aliases/1,
         add_alias/2,
         remove_alias/2
        ]).

%% libary
-export([pages_for_thread/1,
         last_msg_in_thread/1,
         rmess/1,
         rpage/2,
         rday/2,
         rgame/1
        ]).

%% utilities
-export([verify_users/1,
         grep/1, grep/2,
         l/0,
         add_sim_gm_message/4
        ]).

-export([cmp_vote_raw/0
        ]).

-define(HELP,
"MAFIA HELP
==========
General
-------
In the thread_pages directory you find raw downloaded source thread pages,"
" which can be reread in case
the message table is cleared.
In the command_files directory you find manual commands issued that are rerun"
" when running refresh_votes.

Erlang shell commands
---------------------
mafia:l()              - load all beams found in src dir. Run "
"./make in a unix shell first.
mafia:start()          - start the gen_server and the http server
mafia:stop()           - stop the gen_server and the http server
mafia:stop_polling()   - Stop regular polling of source
mafia:start_polling()  - Start regular polling of source
mafia:state()          - Get gen_server state.

mafia:game_start(GName, ThId) - Creates game and defines ThId for game
mafia:switch_to_game(GN) - GN = m25 | thread_id()
mafia:refresh_votes()  - Clear mafia_day and mafia_game and reread all"
" messages.
mafia:refresh_votes({upto, PageNum}) - clear data and reread messages upto and
         including the page given.
mafia:refresh_votes(hard) - reinitialize also the mafia_game record.

mafia:pps()          - Display last message page in current game
mafia:pps(Page)      - Display message page in current game
mafia:pps(Game,Page) - Display message page in game
mafia:pm(MsgId)      - Display one complete message
mafia:print_votes()  - Current status

mafia_time:show_time_offset()   - Display offset
mafia_time:set_time_offset(Off) - Change the time offset
         do a refresh_votes() after changing offset
         Offset = Secs | {msg_id, MsgId} | {days_hours, Days, Hours})

mafia:show_all_users()          - List primary keys in User DB
mafia:show_all_users(Search)    - List primary keys matching Search
mafia:show_all_aliases()        - Display all defined
mafia:show_aliases(Search)      - User search string.
mafia:add_alias(User, Alias)    - Add one alias
mafia:remove_alias(User, Alias) - Remove one alias

Manual Commands
---------------
mafia:end_phase(MsgId)   - Ends current phase.
mafia:unend_phase(MsgId) - Remove early end of this last phase
mafia:move_next_deadline(MsgId, Dir, Time) - Moves next deadline
         earlier or later. A deadline can not be moved into the past.
         Dir = later | earlier
         Time = H | {H, M}
mafia:end_game(MsgId)   - Ends the game with the given msg_id
mafia:unend_game(MsgId) - Unend game
mafia:kill_player(MsgId, Player, Comment) - Kill a player
mafia:replace_player(MsgId, OldPlayer, NewPlayer) - NEEDS IMPL! New is
         replacing old in game. Exact names! Both must exist in user DB.
").

help() ->
    io:format("~s", [?HELP]).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================
start() -> mafia_web:start().
stop() -> mafia_web:stop().
stop_polling() -> mafia_web:stop_polling().
start_polling() -> mafia_web:start_polling().
state() -> mafia_web:get_state().

pm(MsgId) -> mafia_print:pm(MsgId).
pp() -> mafia_print:pp().
pp(Page) -> mafia_print:pp(Page).
pp(ThId, Page) -> mafia_print:pp(ThId, Page).
pps() -> mafia_print:pps().
pps(Page) -> mafia_print:pps(Page).
pps(ThId, Page) -> mafia_print:pps(ThId, Page).
print_votes() -> mafia_print:print_votes().
print_votes(DayNum) -> mafia_print:print_votes(DayNum).
print_votes(DayNum, DoN) -> mafia_print:print_votes(DayNum, DoN).
print_messages(User) -> mafia_print:print_messages(User).

downl() -> mafia_data:downl().

add_thread(ThName, ThId) -> mafia_db:add_thread(ThName, ThId).
rm_thread(ThNameOrId) -> mafia_db:rm_thread(ThNameOrId).

setup_mnesia() -> mafia_db:setup_mnesia().
remove_mnesia() -> mafia_db:remove_mnesia().

refresh_votes() -> mafia_data:refresh_votes().
refresh_votes(P) -> mafia_data:refresh_votes(P).

grep(Str) -> mafia_data:grep(Str).
grep(Str, Mode) -> mafia_data:grep(Str, Mode).

rmess(MsgId) -> ?rmess(MsgId).
rpage(ThId, Page) -> ?rpage(ThId, Page).

rday(ThId, DayNum) -> ?rday(ThId, DayNum).

rgame(?game_key = K) -> ?rgame(?getv(K));
rgame(?thread_id = K) -> ?rgame(?getv(K));
rgame(Id) -> ?rgame(?thid(Id)).

verify_users(m26) ->
    [mafia_vote:print_verify_user(U)
     || U <- ?M26_GMs ++ ?M26_players ++ ?M26_Subs],
    ok.

%% -----------------------------------------------------------------------------
%% @doc Switch to other game and reread all info
%% @end
%% -----------------------------------------------------------------------------
-spec switch_to_game(GameId :: atom() | thread_id()) -> term().
switch_to_game(Id) ->
    switch_to_gameI(?thid(Id), normal).

switch_to_game(Id, Method) ->
    switch_to_gameI(?thid(Id), Method).

%% -----------------------------------------------------------------------------

switch_to_gameI({?error, _} = E, _) -> E;
switch_to_gameI(ThId, normal) ->
    ?set(game_key, ThId),
    ?set(thread_id, ThId),
    ?set(page_to_read, 1),
    mafia:stop(),
    mafia:start();
switch_to_gameI(ThId, hard) -> %% Should always work
    mafia_db:reset_game(ThId),
    ?set(game_key, ThId),
    ?set(thread_id, ThId),
    ?set(page_to_read, 1),
    %% These two to set the #state.game_key
    mafia:stop(),
    timer:sleep(2000),
    mafia:start(),
    timer:sleep(5000),
    mafia:refresh_votes(hard).

%% -----------------------------------------------------------------------------
%% @doc Starts up a game giving it e.g. m26, 1234567
%% @end
%% -----------------------------------------------------------------------------
-spec game_start(GName :: atom(), ThId :: thread_id()) -> ok | term().
game_start(GName, ThId) ->
    case mafia_db:add_thread(GName, ThId) of
        {reg_add, _} ->
            file:write_file("game_info.txt",
                            io_lib:format("{~p, ~p}.\n", [GName, ThId]),
                            [append]),
            mafia_db:write_game({GName, ThId});
        E -> E
    end.

%% -----------------------------------------------------------------------------
%% @doc End current phase with GM message and set next phase at
%% the game's local date and time given by the GM in the same message
%% @end
%% -----------------------------------------------------------------------------
end_phase(MsgId) ->
    case find_mess_game(MsgId) of
        {ok, G, M} ->
            Time = M#message.time,
            ThId = M#message.thread_id,
            case mafia_time:calculate_phase(G, Time) of
                ?game_ended -> {?error, ?game_ended};
                Phase ->
                    Cmd = #cmd{time = Time,
                               msg_id = MsgId,
                               mfa = {mafia, end_phase, [MsgId]}},
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(ThId, Cmd),
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
            ThId = M#message.thread_id,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_phase, [MsgId]}},
            ?man(Time, {'UNDO', Cmd}),
            mafia_file:manual_cmd_from_file(ThId, Cmd),
            mafia_time:unend_phase(G, M);
        {?error, _} = E -> E
    end.


find_mess_game(MsgId) ->
    case ?rmess(MsgId) of
        [] -> {?error, msg_not_found};
        [M] ->
            ThId = M#message.thread_id,
            case ?rgame(ThId) of
                [] -> {?error, game_not_found};
                [G] -> {ok, G, M}
            end
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
                    mafia_file:manual_cmd_to_file(M#message.thread_id, Cmd);
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
            #message{thread_id = ThId, time = Time} = M,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [MsgId]}},
            ?man(Time, Cmd),
            Reply = mafia_file:manual_cmd_to_file(ThId, Cmd),
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
            #message{thread_id = ThId, time = Time} = M,
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, end_game, [MsgId]}},
            ?man(Time, {'UNDO', Cmd}),
            mafia_file:manual_cmd_from_file(ThId, Cmd),
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
                    mafia_file:manual_cmd_to_file(M#message.thread_id, Cmd),
                    ok;
                _ ->
                    replace_failed
            end;
        {?error, _} = E -> E
    end.

 %% M26 = 1432756.
 %% D1 = hd(mafia:rday(M26, 1)).
 %% Rem2 = [case U of <<"ND">> -> <<"zorclex">>; _ -> U end || U <- D1#mafia_day.players_rem].
 %% mnesia:dirty_write(D1#mafia_day{players_rem = Rem2}).

%% -----------------------------------------------------------------------------
%% @doc Read the GM message and add good comment about who the dead player was.
%% @end
%% -----------------------------------------------------------------------------
-spec kill_player(MsgId :: msg_id(),
                        Player :: string(),
                        Comment :: string())
                       -> ok | {error, not_found}.
kill_player(MsgId, Player, Comment) ->
    case ?rmess(MsgId) of
        [] -> no_message_found;
        [#message{thread_id = ThId,
                  time = Time} = M] ->
            Cmd = #cmd{time = Time,
                       msg_id = MsgId,
                       mfa = {mafia, kill_player,
                              [MsgId, Player, Comment]}},
            case kill_playerI(?rgame(ThId), M, Player, Comment) of
                {ok, DeathPhase} ->
                    ?man(Time, Cmd),
                    mafia_file:manual_cmd_to_file(ThId, Cmd),
                    mafia_web:regenerate_history(M#message.time, DeathPhase),
                    {player_killed, DeathPhase};
                Other -> Other
            end
    end.

kill_playerI([], _M, _Player, _Comment) -> no_game;
kill_playerI([G], M, Player, Comment) ->
    %% Time = M#message.time,
    PlayerB = ?l2b(Player),
    %% CommentB = ?l2b(Comment),
    %% Deaths = G#mafia_game.player_deaths,
    {Resp, _} = mafia_vote:kill_player(G, M, PlayerB, Comment),
    Resp.

%% deprecated - kept only for command_files
set_death_comment(MsgId, Player, Comment) ->
    kill_player(MsgId, Player, Comment).

%% -----------------------------------------------------------------------------

%% load all beams in local dir
l() ->
    {ok, Files} = file:list_dir("."),
    Beams = [?l2a(?lrev(ModRev))
             || "maeb." ++ ModRev
                    <- [?lrev(F) || F <- Files]],
    Beams2 = (Beams -- [mafia]) ++ [mafia],
    [begin code:purge(M), code:load_file(M), M end
     || M <- Beams2].

%% Pre-check user list given by GM in initial game PM
verify_new_user_list(26) ->
    io:format("New--Old \"~s\"\n"
              "Old--New \"~s\"\n",
              [string:join(?M26_players -- ?M26_players_old, "\", \""),
               string:join(?M26_players_old -- ?M26_players, "\", \"")]),
    Users = ?M26_GMs ++ ?M26_players,
    verify_new_user_list2(Users);
verify_new_user_list(25) ->
    Users = ?M25_GMs ++ ?M25_players,
    verify_new_user_list2(Users);
verify_new_user_list(24) ->
    Users = ?M24_GMs ++ ?M24_players,
    verify_new_user_list2(Users).

verify_new_user_list2(Users) ->
    [begin
         UserB = ?l2b(U),
         UserUB = ?l2ub(U),
         case mnesia:dirty_read(user, UserUB) of
             [] ->
                 io:format("User ~p does not exist\n",[U]);
             [#user{name = UserB,
                    verification_status = Ver}] ->
                 io:format("User ~p exists with correct case "
                           "and is ~p\n", [U, Ver]);
             [#user{name = UserB2,
                    verification_status = Ver}] ->
                 io:format("User ~p exists but has incorrect case. "
                           "Correct case is ~p and is ~p\n",
                           [U, ?b2l(UserB2), Ver])
         end
     end
     || U <- Users],
    done.

%% Seems to be unused
-spec set_thread_id(ThId :: integer())  -> ok.
set_thread_id(ThId) when is_integer(ThId) ->
    ?set(?thread_id, ThId),
    PageToRead =
        case pages_for_thread(ThId) of
            [] -> 1;
            Pages ->
                lists:max(Pages)
        end,
    ?set(?page_to_read, PageToRead),
    ok.

show_settings() ->
    PrintSettings =
        fun(K) -> Setting = hd(mnesia:dirty_read(?kv_store, K)),
                  SetKey = element(2, Setting),
                  SetVal = element(3, Setting),
                  io:format("~p: ~p\n", [SetKey, SetVal])
        end,
    [PrintSettings(K) || K <- lists:sort(mnesia:dirty_all_keys(?kv_store))],
    ok.

pages_for_thread(ThId) ->
    MatchHead = #page_rec{key = {'$1', '$2'}, _='_'},
    Guard = {'==', '$1', ThId},
    Result = '$2',
    Pages = mnesia:dirty_select(page_rec,
                                [{MatchHead, [Guard], [Result]}]),
    lists:sort(Pages).

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
    Users = all_users(),
    io:format("All Users: ~p\n", [Users]).

show_all_users(Search) ->
    Users = all_users(Search),
    io:format("All Users: ~999p\n", [Users]).

all_users() ->
    [?b2l(UserUB) || UserUB <- mnesia:dirty_all_keys(user)].

all_users(Search) ->
    [?b2l(UserUB) || UserUB <- mnesia:dirty_all_keys(user),
                   0 /= string:str(?b2l(UserUB), ?l2u(Search))].


show_all_aliases() ->
    show_aliases(all).

-spec show_aliases(UserSearch :: string()) -> ok | {error, Reason :: term()}.
show_aliases(all) ->
    io:format("~-15s ~s\n", ["User", "Aliases"]),
    io:format("~-15s ~s\n", ["----", "-------"]),
    [begin
         U = hd(mnesia:dirty_read(user, UserUB)),
         if U#user.aliases /= [] ->
                 io:format(
                   "~-15s ~s\n",
                   [?b2l(U#user.name),
                    string:join(["\"" ++ ?b2l(AlB) ++ "\""
                                 || AlB <- U#user.aliases], ", ")]);
            true -> ok
         end
     end
     || UserUB <- mnesia:dirty_all_keys(user)],
    ok;
show_aliases(Search) ->
    io:format("Search: ~s\n", [Search]),
    Users = all_users(Search),
    [show_aliasesI(User) || User <- Users],
    ok.


show_aliasesI(User) ->
    UserUB = ?l2ub(User),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] ->
            io:format("Found: ~s\nAliases: ~p\n",
                      [?b2l(U#user.name),
                       [?b2l(AlB) || AlB <- U#user.aliases]])
    end.

-spec add_alias(User :: string(), Alias :: string())
               -> ok | {error, Reason :: term()}.
add_alias(User, Alias) ->
    UserB = ?l2b(User),
    UserUB = ?l2ub(User),
    AliasB = ?l2b(Alias),
    AliasUB = ?l2ub(Alias),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            AliasesUB = [?b2ub(AlB) || AlB <- U#user.aliases],
            case lists:member(AliasUB, AliasesUB) of
                true ->
                    {error, alias_exist_already};
                false ->
                    mnesia:dirty_write(U#user{aliases = [AliasB|AliasesB]}),
                    ok
            end
    end.

-spec remove_alias(User :: string(), Alias :: string())
                  -> ok | {error, Reason :: term()}.
remove_alias(User, Alias) ->
    UserB = ?l2b(User),
    UserUB = ?l2ub(User),
    AliasB = ?l2b(Alias),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            case lists:member(AliasB, AliasesB) of
                true ->
                    NewAliasesB = AliasesB -- [AliasB],
                    mnesia:dirty_write(U#user{aliases = NewAliasesB}),
                    ok;
                false ->
                    {error, alias_does_not_exist}
            end
    end.

add_sim_gm_message(Page, TplMsgId, MsgId, Text) ->
    ThId = ?getv(?game_key),
    case {?rpage(ThId, Page), ?rmess(TplMsgId), ?rmess(MsgId)} of
        {[], _, _} -> no_page;
        {_, [], _} -> no_template_message;
        {_, _, [_]} -> message_exit_already;
        {[P], [M], []} ->
            SimM = M#message{msg_id = MsgId, message = ?l2b(Text)},
            P2 = P#page_rec{message_ids = [SimM | P#page_rec.message_ids]},
            mnesia:dirty_write(SimM),
            mnesia:dirty_write(P2)
    end.

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% No-one seems to use this one.
cmp_vote_raw() ->
    ThId = ?getv(?thread_id),
    DayNum = 1,
    case rday(ThId, DayNum) of
        [] ->
            ignore;
        [#mafia_day{votes = GVotes}] ->
            [begin
                 VoteSum =
                     [{?b2l(V#vote.vote), ?b2l(V#vote.raw), V#vote.valid}
                      || V <- Votes],
                 {?b2l(User), VoteSum}
             end || {User, Votes} <- GVotes]
    end.
