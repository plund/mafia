-module(mafia_db).

-include("mafia.hrl").

-export([
         set_kv/2,
         get_kv/1,
         get_kv/2, 
         setup_mnesia/0, 
         remove_mnesia/0
        ]).

set_kv(Key, Value) ->
    mnesia:dirty_write(#kv_store{key=Key, value = Value}).

get_kv(Key) -> get_kv(Key, undefined).

get_kv(Key, Default) ->
    case mnesia:dirty_read(kv_store, Key) of
        [] -> Default;
        [#kv_store{value = Value}] -> Value
    end.

-spec setup_mnesia() -> ok | schema_existed_already | {error, Reason::term()}.
setup_mnesia() ->
    case mnesia:create_schema([node()]) of
        {error,{_,{already_exists,_}}} ->
            start_mnesia(already_exists),
            schema_existed_already;
        Other ->
            start_mnesia(do_create),
            Other
    end.

remove_mnesia() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

-spec start_mnesia(Op :: already_exists | do_create )
                  -> mnesia_start_ok | {error, Reason::term()}.
start_mnesia(Op) ->
    case mnesia:start() of
        ok ->
            if Op == do_create ->
                    create_tables(),
                    insert_initial_data();
               true ->
                    ok
            end,
            mnesia_start_ok;
        Other ->
            Other
    end.

-define(M24_players, 
        ["Bo_sox48", "CaptainMeme", "Chaqa", "Dargorgyel", "Ezio", "Floodgates",
         "Ghug", "Glen_Alexander", "Goldfinger0303", "Guak", "Hellenic Riot",
         "Ikaneko", "Jamiet99uk", "Krellin", "Maniac", "Peterlund",
         "Rdrivera2005", "Teacon7", "VashtaNeurotic", "Vecna", "Xorxes",
         "Yoyoyozo", "Zorclex"]).

%% List of lists
to_bin(LoL = [[_|_]|_]) ->
    [list_to_binary(L) || L <- LoL].
%% to_bin_upper(LoL = [[_|_]|_]) ->
%%     [list_to_binary(string:to_upper(L)) || L <- LoL].

insert_initial_data() ->
    io:format("Adding Mafia Game M24\n", []),
    MGame = #mafia_game{
      key = ?DefThId,
      name = <<"MAFIA XXIV: Webdiplomacy's Tom Clancy's The Division">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      day1_dl_time = {{2016,10,19},{18,0,0}},
      is_init_dst = true,
      dst_changes = [{{{2016,11,6},{2,0,0}}, false},
                     {{{2017, 4,1},{2,0,0}}, true}],
      gms = to_bin(["DemonRHK", "MoscowFleet"]),
      players_orig = to_bin(?M24_players),
      players_rem = to_bin(?M24_players),
      %% players_rem_upp = to_bin_upper(?M24_players),
      players_dead = [],
      page_to_read = 1,
      complete = false
     },
    MGame2 = mafia_time:add_deadline(MGame, 16),
    mnesia:dirty_write(MGame2),
    io:format("Adding values to kv_store\n", []),
    set_kv(thread_id, ?DefThId),
    set_kv(page_to_read, 1),
    set_kv(timezone_user, 1),
    set_kv(dst_user, false),
    set_kv(print_time, user). % user | game | utc | zulu | gmt

create_tables() ->
    create_table(kv_store),
    create_table(page_rec),
    create_table(message),
    create_table(mafia_day),
    create_table(mafia_game).

create_table(RecName) ->
    Opts = create_table_opts(RecName),
    io:format("mnesia:create_table(~p, ~p).\n", [RecName, Opts]),
    case mnesia:create_table(RecName, Opts) of
        {aborted,{already_exists,_Tab}} ->
            TI = mnesia:table_info(RecName, attributes),
            RI = rec_info(RecName),
            if TI /= RI ->
%%% HERE we should check for upgrade method and stop if upgrade does not exist
                    io:format("Delete table '~p' due to mismatching attribute list\n",[RecName]),
                    mnesia:delete_table(RecName),
                    create_table(RecName);
               true ->
                    io:format("Table '~p' is OK!\n",[RecName])
            end;
        {atomic, ok} ->
            io:format("Created table '~p'\n",[RecName])
    end.

create_table_opts(Table) ->
    [{disc_copies, [node()]},
     {attributes, rec_info(Table)}].

rec_info(kv_store) -> record_info(fields, kv_store);
rec_info(page_rec) -> record_info(fields, page_rec);
rec_info(message) -> record_info(fields, message);
rec_info(mafia_day) -> record_info(fields, mafia_day);
rec_info(mafia_game) -> record_info(fields, mafia_game).
