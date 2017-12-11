-module(user_default).

-include("mafia.hrl").

-export([help/0, help/1, mhelp/0, ehelp/0, chelp/0,
         l/0,

         rmess/1,
         rpage/3,
         rday/2,
         rgame/1,
         ruser/2,

         set/2,
         getv/1,
         show/0,
         show_cnts/0,
         show_cnts/1,
         clr_cnts/0,

         stop_poll/1,
         start_poll/1,
         poll/1,
         state/0,
         start/0,
         stop/0,

         pm/1,
         pp/0, pp/3,
         pps/0, pps/1, pps/3
        ]).

rmess(MsgId) -> ?rmess(MsgId).
rpage(ThId, Page, Site) -> ?rpage(ThId, Page, Site).
rday(GNum, DayNum) -> ?rday(GNum, DayNum).
rgame(GNum) when is_integer(GNum) -> ?rgame(GNum).
ruser(Name, Site) -> ?ruser(Name, Site).

show() -> mafia:show_settings().
show_cnts() -> mafia_lib:print_all_cnts().
show_cnts(Cnt) -> mafia_lib:print_all_cnts(Cnt).
clr_cnts() -> mnesia:clear_table(cnt).
set(K, V) -> mafia_db:set(K, V).
getv(K) -> mafia_db:getv(K).

pm(MsgId) -> mafia_print:pm(MsgId).
pp() -> mafia_print:pp().
pp(ThId, Page, Site) -> mafia_print:pp(ThId, Page, Site).
pps() -> mafia_print:pps().
pps(Page) -> mafia_print:pps(Page).
pps(ThId, Page, Site) -> mafia_print:pps(ThId, Page, Site).

stop_poll(GNum) -> game:stop_polling(GNum).
start_poll(GNum) -> game:start_polling(GNum).
poll(GNum) -> game:poll(GNum).
state() -> mafia_web:get_state().
start() -> mafia:start().
stop() -> mafia:stop().

l() -> mafia:l().

-define(HELP,
"MAFIA HELP
----------
FILES:
thread_pages/    - Raw downloaded source thread pages to be reread
command_files/   - Manual commands issued, rerun when refresh_votes
patches/         - Store updated beam files here and do l() to load them
logs/            - Logs run_erl.log, erlang.log.N
user_data.txt    - User table exported, may be imported

DBG: rmess/1, rpage/2, rday/2, rgame/1

COMMANDS:
show()           - Show server settings
show_cnts()      - Show all counters
show_cnts(M)     - Show counters M is int, atom or list
clr_cnts()       - Clear all counters
set(K, V)        - Set a key value pair
getv(K)          - Get a value for a key

pm(MsgId)        - Display one complete message
pp()             - Display last message page in current game
pp(Page)         - Display message page in current game
pp(Game,Page)    - Display message page in game
pps()            - Display last message page in current game
pps(Page)        - Display message page in current game
pps(Game,Page)   - Display message page in game

stop_poll(GNum)  - Stop regular polling of source
start_poll(GNum) - Start regular polling of source
poll(GNum)       - poll now.
state()          - Get gen_server state.
start()          - Start the gen_server and the http server
stop()           - Stop the gen_server and the http server

l()              - Load all beams found in src dir.

mhelp()          - Mafia help
chelp()          - Command help
ehelp()          - Erlang shell help
help(type())
  where type() :: e | erlang | m | mafia | c | cmd | command.
").

-define(MAFIA_HELP,
"Game Life Cycle
---------------

mafia:initiate_game(GNum)
mafia:initiate_game(GNum, Site)
mafia:initiate_game(GNum, GMs, Site)

mafia:refresh_votes().      - Recount votes in current game
mafia:refresh_votes(GNums).
mafia:refresh_votes(all).

Manual Commands
---------------
mafia:end_phase(GNum, MsgId)   - Ends current phase.
mafia:unend_phase(GNum, MsgId) - Remove early end of this last phase
mafia:move_next_deadline(GNum, MsgId, Dir, Time) - Moves next deadline
         earlier or later. A deadline can not be moved into the past.
         Dir = later | earlier
         Time = H | {H, M}
mafia:end_game(GNum, MsgId)   - Ends the game with the given msg_id
mafia:unend_game(GNum, MsgId) - Unend game

mafia:kill_player(GNum, MsgId, Player, Comment) - Kill a player
mafia:set_death_msgid(GNum, MsgId, Player, DeathMsgId, Comment).
    Reference a previous msgid as the death message, give comment.
mafia:replace_player(GNum, MsgId, OldPlayer, NewPlayer)
    New player is replacing old player in game. Exact names!
    Old player must exist in user DB
    New player is created if missing in DB

mafia:refresh_messages() - Reread all messages from disk, use 'game_key'
mafia:refresh_votes()           - refresh current game
mafia:refresh_votes(game_num()) - refresh one selected game
mafia:refresh_votes(all)        - refresh all games
mafia:print_votes()  - Current status
").

-define(CMD_HELP,
"Erlang shell commands
---------------------
mafia:export_user_data() - export to file 'user_data.txt'
mafia:import_user_data() - import from file 'user_data.txt'

mafia_time:show_time_offset()   - Display offset
mafia_time:set_time_offset(Off) - Change the time offset
    do a refresh_votes() after changing offset
    Off = Secs
        | {move, Secs}
        | {msg_key, {MsgId, Site}}
        | {days_hours, Days, Hours})

mafia:show_all_users()          - List primary keys in User DB
mafia:show_all_users(Search)    - List primary keys matching Search
mafia:show_all_aliases()        - Display all defined
mafia:show_aliases(Search)      - User search string.
mafia:add_alias(User, Site, Alias)    - Add one alias
mafia:remove_alias(User, Site, Alias) - Remove one alias
").

help() ->
    io:format("~s", [?HELP]).
ehelp() ->
    shell_default:help().
mhelp() ->
    io:format("~s", [?MAFIA_HELP]).
chelp() ->
    io:format("~s", [?CMD_HELP]).

help(T) when T==e; T==erlang -> ehelp();
help(T) when T==m; T==mafia -> mhelp();
help(T) when T==c; T==cmd; T==command -> chelp();
help(_T) -> help().
