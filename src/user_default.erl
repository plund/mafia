-module(user_default).

-include("mafia.hrl").

-export([help/0, help/1, mhelp/0, ehelp/0, chelp/0,
         l/0,

         grep/1, grep/2,
         rmess/1,
         rpage/2,
         rday/2,
         rgame/1,
         ruser/1,

         set/2,
         unset/1,
         getv/1,
         show/0,

         stop_poll/0,
         start_poll/0,
         poll/0,
         state/0,
         start/0,
         stop/0,

         pm/1,
         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2
        ]).

grep(Str) -> mafia_data:grep(Str).
grep(Str, Mode) -> mafia_data:grep(Str, Mode).
rmess(MsgId) -> ?rmess(MsgId).
rpage(ThId, Page) -> ?rpage(?thid(ThId), Page).
rday(ThId, DayNum) -> ?rday(?thid(ThId), DayNum).
rgame(GNum) when is_integer(GNum) -> ?rgame(GNum).
ruser(Name) -> ?ruser(Name).

show() -> mafia:show_settings().
set(K, V) -> mafia_db:set(K, V).
unset(K) -> mafia_db:unset(K).
getv(K) -> mafia_db:getv(K).

pm(MsgId) -> mafia_print:pm(MsgId).
pp() -> mafia_print:pp().
pp(Page) -> mafia_print:pp(Page).
pp(ThId, Page) -> mafia_print:pp(ThId, Page).
pps() -> mafia_print:pps().
pps(Page) -> mafia_print:pps(Page).
pps(ThId, Page) -> mafia_print:pps(ThId, Page).

stop_poll() -> mafia_web:stop_polling().
start_poll() -> mafia_web:start_polling().
poll() -> mafia_web:poll().
state() -> mafia_web:get_state().
start() -> mafia:start().
stop() -> mafia:stop().

l() -> mafia:l().

-define(HELP,
"MAFIA HELP
----------
FILES:
thread_pages/  - Raw downloaded source thread pages to be reread
command_files/ - Manual commands issued, rerun when refresh_votes
patches/       - Store updated beam files here and do l() to load them
logs/          - Logs run_erl.log, erlang.log.N
user_data.txt  - User table exported, may be imported

DBG: grep/1, grep/2, rmess/1, rpage/2, rday/2, rgame/1

COMMANDS:
show()         - Show server settings
set(K, V)      - Set a key value pair
unset(K)       - Remove a key value pair
getv(K)        - Get a value for a key

pm(MsgId)      - Display one complete message
pp()           - Display last message page in current game
pp(Page)       - Display message page in current game
pp(Game,Page)  - Display message page in game
pps()          - Display last message page in current game
pps(Page)      - Display message page in current game
pps(Game,Page) - Display message page in game

stop_poll()    - Stop regular polling of source
start_poll()   - Start regular polling of source
poll()         - poll now.
state()        - Get gen_server state.
start()        - Start the gen_server and the http server
stop()         - Stop the gen_server and the http server

l()            - Load all beams found in src dir.

mhelp()        - Mafia help
chelp()        - Command help
ehelp()        - Erlang shell help
help(type())
  where type() :: e | erlang | m | mafia | c | cmd | command.
").

-define(MAFIA_HELP,
"Game Life Cycle
---------------
Days before:
mafia:pregame_create(99)  - create pregame
mafia:pregame_update()    - rewrite pregame
When game starts:
THIS NEEDS UPDATES!
mafia:game_start(99, 1460042).  - Assign thread id
mafia:switch_to_game(99).       - Switch and create page_recs.
mafia:refresh_votes().          - Count votes

mafia:game_start(GNum, ThId) - Creates game using GNum ThId
mafia:check_game_data(GNum)
mafia:switch_to_game(GNum)
mafia:switch_to_game(GNum, refresh)

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
mafia:set_death_msgid(MsgId, Player, DeathMsgId, Comment).
    Reference a previous msgid as the death message, give comment.
mafia:replace_player(MsgId, OldPlayer, NewPlayer)
    New player is replacing old player in game. Exact names!
    Old player must exist in user DB
    New player is created if missing in DB

mafia:refresh_messages() - Reread all messages from disk, use 'game_key'
mafia:refresh_votes()  - Clear mafia_day and mafia_game and reread all"
" messages.
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
    Off = Secs | {msg_id, MsgId} | {move, Secs} | {days_hours, Days, Hours})

mafia:show_all_users()          - List primary keys in User DB
mafia:show_all_users(Search)    - List primary keys matching Search
mafia:show_all_aliases()        - Display all defined
mafia:show_aliases(Search)      - User search string.
mafia:add_alias(User, Alias)    - Add one alias
mafia:remove_alias(User, Alias) - Remove one alias

mafia:add_thread(atom(), integer()) - add thread name
mafia:rm_thread(atom() | integer()) - remove thread name
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
