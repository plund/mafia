-module(user_default).

-include("mafia.hrl").

-export([help/0, ehelp/0, chelp/0,
         l/0,

         grep/1, grep/2,
         rmess/1,
         rpage/2,
         rday/2,
         rgame/1,

         set/2,
         getv/1,
         show_settings/0,

         stop_polling/0,
         start_polling/0,
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
rgame(?game_key = K) -> ?rgame(?getv(K));
rgame(?thread_id = K) -> ?rgame(?getv(K));
rgame(Id) -> ?rgame(?thid(Id)).

show_settings() -> mafia:show_settings().
set(K, V) -> mafia_db:set(K, V).
getv(K) -> mafia_db:getv(K).

pm(MsgId) -> mafia_print:pm(MsgId).
pp() -> mafia_print:pp().
pp(Page) -> mafia_print:pp(Page).
pp(ThId, Page) -> mafia_print:pp(ThId, Page).
pps() -> mafia_print:pps().
pps(Page) -> mafia_print:pps(Page).
pps(ThId, Page) -> mafia_print:pps(ThId, Page).

stop_polling() -> mafia_web:stop_polling().
start_polling() -> mafia_web:start_polling().
poll() -> mafia_web:poll().
state() -> mafia_web:get_state().
start() -> mafia:start().
stop() -> mafia:stop().

l() -> mafia:l().

ehelp() ->
    shell_default:help().

-define(HELP,
"MAFIA HELP
----------
FILES:
game_info.txt  - Mapping thread names m26 to thread ids
thread_pages/  - Raw downloaded source thread pages to be reread
command_files/ - Manual commands issued, rerun when refresh_votes
patches/       - Store updated beam files here and do l() to load them
logs/          - Logs run_erl.log, erlang.log.N

DBG: grep/1, grep/2, rmess/1, rpage/2, rday/2, rgame/1

COMMANDS:
show_settings()- Show server settings
set(K, V)      - Set a key value pair
getv(K)        - Get a value for a key

pm(MsgId)      - Display one complete message
pp()           - Display last message page in current game
pp(Page)       - Display message page in current game
pp(Game,Page)  - Display message page in game
pps()          - Display last message page in current game
pps(Page)      - Display message page in current game
pps(Game,Page) - Display message page in game

stop_polling() - Stop regular polling of source
start_polling()- Start regular polling of source
poll()         - poll now.
state()        - Get gen_server state.
start()        - Start the gen_server and the http server
stop()         - Stop the gen_server and the http server

l()            - Load all beams found in src dir.

chelp()        - Command help
ehelp()        - Erlang shell help
").

help() ->
    io:format("~s", [?HELP]).

-define(CMD_HELP,
"Erlang shell commands
---------------------
mafia:game_start(GName, ThId) - Creates game and defines ThId for game
mafia:check_game_data(Id) - Id = m25 | thread_id()
mafia:switch_to_game(Id)  - Id = m25 | thread_id()
mafia:switch_to_game(Id, refresh) - read disk, Id = m25 | thread_id()

mafia_data:refresh_messages() - Reread all messages from disk, use 'game_key'
mafia:refresh_votes()  - Clear mafia_day and mafia_game and reread all"
" messages.

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

mafia:replace_player(MsgId, OldPlayer, NewPlayer)
    New player is replacing old player in game. Exact names!
    Old player must exist in user DB
    New player is created if missing in DB").

chelp() ->
    io:format("~s", [?CMD_HELP]).
