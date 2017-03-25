-module(mafia_db).

-include("mafia.hrl").
-include("mafia_game.hrl").

-export([
         verify_new_user_list/1,

         set/2,
         unset/1,
         getv/1,
         getv/2,
         add_thread/2,
         rm_thread/1,

         setup_mnesia/0,
         remove_mnesia/0,
         create_tables/0,
         create_table/1,

         reset_game/1,
         write_game/1,
         write_default_user_table/0
        ]).


%% Pre-check user list given by GM in initial game PM
verify_new_user_list(28) ->
    Users = ?M28_GMs ++ ?M28_players,
    verify_new_user_list2(Users);
verify_new_user_list(27) ->
    Users = ?M27_GMs ++ ?M27_players,
    verify_new_user_list2(Users);
verify_new_user_list(26) ->
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
         UserB = ?l2b(User),
         case ?ruserUB(User) of
             [] ->
                 io:format("User ~p does not exist\n",[User]);
             [#user{name = UserB,
                    verification_status = Ver}] ->
                 io:format("User ~p exists with correct case "
                           "and is ~p\n", [User, Ver]);
             [#user{name = UserB2,
                    verification_status = Ver}] ->
                 io:format("User ~p exists but has incorrect case. "
                           "Correct case is ~p and is ~p\n",
                           [User, ?b2l(UserB2), Ver])
         end
     end
     || User <- Users],
    done.

getv(Key) -> getv(Key, ?undefined).

getv(Key, Default) ->
    case mnesia:dirty_read(kv_store, Key) of
        [] -> Default;
        [#kv_store{value = Value}] -> Value
    end.

-spec setup_mnesia() -> ok |
                        already_started |
                        schema_existed_already |
                        {error, Reason::term()}.
setup_mnesia() ->
    case mnesia:system_info(is_running) of
        no ->
            case mnesia:create_schema([node()]) of
                {error,{_,{already_exists,_}}} ->
                    start_mnesia(already_exists),
                    mnesia:add_table_index(user, #user.name),
                    schema_existed_already;
                Other ->
                    start_mnesia(do_create),
                    mnesia:add_table_index(user, #user.name),
                    Other
            end;
        yes ->
            already_started
    end.

remove_mnesia() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

-spec start_mnesia(Op :: already_exists | do_create )
                  -> mnesia_start_ok |
                     {error, Reason::term()}.
start_mnesia(Op) ->
    case mnesia:start() of
        ok ->
            if Op == do_create ->
                    create_tables(),
                    insert_initial_data();
               true ->
                    ok
            end,
            timer:sleep(500),
            mnesia_start_ok;
        Other ->
            Other
    end.

%% List of lists
to_bin(LoL = [[_|_]|_]) -> [?l2b(L) || L <- LoL].

to_bin_sort(LoL = [[_|_]|_]) ->
    [?l2b(L) || L <- mafia_lib:alpha_sort(LoL)].

insert_initial_data() ->
    io:format("Adding values to kv_store\n", []),
    ?set(?page_to_read, 1),
    ?set(?timezone_user, 1),
    ?set(?dst_user, false),
    ?set(?console_tz, user),
    ?set(?mod_msg, ?undefined),
    write_default_user_table(),
    add_threads().

add_threads() ->
    ThInfos = [{CurGame, CurGameId}|_] =
        lists:reverse(
          lists:sort(
            element(2,
                    file:consult("game_info.txt")))),
    _ = [begin
             add_thread(Game, Id),
             write_game(Game)
         end || {Game, Id} <- ThInfos],
    ?set(?thread_id, CurGameId),
    ?set(?game_key, CurGameId),
%%% ?game_key (DB kv_store):
%%% - Must be set right for the single game we are serving at the moment
%%% - gen_server mafia_web sets this ?game_key value in its init
%%% - ?game_key is used as game reference for all web calls
%%% - the ?game_key is also default value for many MANUAL commands:
%%%   refresh_messages, refresh_votes, refresh_stat, print_votes,
%%%   print_stats_opts, web_vote_tracker,
%%% ?page_to_read (DB kv_store):
%%%    used in manual downl/0, mafia_print:pp/pps use when not game
    G = get_game_rec(CurGame),
    ?set(?timezone_game, G#mafia_game.time_zone),
    ?set(?dst_game, G#mafia_game.is_init_dst).

write_default_user_table() ->
    [ ?dwrite_user(
         #user{name_upper = ?l2ub(U),
               name = ?l2b(U),
               aliases = mafia_upgrade:get_aliases(?l2b(U)),
               verification_status = ?unverified})
      || U <- lists:usort(?M24_players ++ ?M24_GMs ++
                              ?M25_players ++ ?M25_GMs ++
                              ?M26_players ++ ?M26_GMs)],
    ok.

reset_game(ThId) ->
    mnesia:dirty_delete(mafia_game, ThId),
    write_game(ThId).

%% Write game record to DB
write_game(?false) -> ?error;
write_game(GName) when is_atom(GName) ->
    {ok, L} = file:consult("game_info.txt"),
    write_game2(GName, lists:keyfind(GName, 1, L ++ ?getv(?reg_threads)));
write_game(ThId) when is_integer(ThId) ->
    {ok, L} = file:consult("game_info.txt"),
    write_game2(ThId, lists:keyfind(ThId, 2, L ++ ?getv(?reg_threads)));
write_game({GName, ThId}) ->
    do_write_game(GName, ThId).

write_game2(GName, false) when is_atom(GName) ->
    do_write_game(GName, GName);
write_game2(ThId, false) when is_integer(ThId) ->
    {error, no_game_name};
write_game2(_, {GName, ThId}) ->
    do_write_game(GName, ThId).

-spec do_write_game(atom(), Key :: thread_id() | atom())
                   -> ok | {error, term()}.
do_write_game(GName, ThId) when is_integer(ThId) ->
    case {?rgame(GName), ?rgame(ThId)} of
        {[], []} ->
            G = get_game_rec(GName),
            do_write_game_2(G, GName, ThId);
        {[G], []} ->
            mnesia:dirty_delete(mafia_game, GName),
            do_write_game_2(G, GName, ThId);
        {_, [_]} ->
            {error, e_exists}
    end;
do_write_game(GName, Key) when is_atom(Key) ->
    case ?rgame(Key) of
        [] ->
            G = get_game_rec(GName),
            do_write_game_2(G, GName, Key);
        [_] ->
            {error, e_exists}
    end.

do_write_game_2(G, GName, Key) ->
    %% G = get_game_rec(GName),
    io:format("Initializing Mafia Game\n  ~p: ~s\n",
              [GName, ?b2l(G#mafia_game.name)]),
    G2 = G#mafia_game{key = Key},
    Game = mafia_time:initial_deadlines(G2),
    ?dwrite_game(Game).

get_game_rec(m28) ->
    _ = #mafia_game{
      game_num = 28,
      name = <<"Mafia 28: Item Madness">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      day1_dl_time = {{2017,3,28},{18,0,0}},
      is_init_dst = true,
      dst_changes = [{{{2017,3,12},{2,0,0}}, true}, %% USA 2017
                     {{{2017,11,05},{2,0,0}}, false}],
      gms = to_bin(?M28_GMs),
      players_orig = to_bin_sort(?M28_players),
      players_rem = to_bin_sort(?M28_players),
      player_deaths = [],
      page_to_read = 1
     };
get_game_rec(m27) ->
    %% M27 Game Thread 1447615
    %% M27 signup threadid = 1442470
    _ = #mafia_game{
      game_num = 27,
      name = <<"Mafia 27: Welcome to Westworld">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      day1_dl_time = {{2017,2,8},{18,0,0}},
      is_init_dst = false,
      dst_changes = [{{{2017,3,12},{2,0,0}}, true}, %% USA 2017
                     {{{2017,11,05},{2,0,0}}, false}],
      gms = to_bin(?M27_GMs),
      players_orig = to_bin_sort(?M27_players),
      players_rem = to_bin_sort(?M27_players),
      player_deaths = [],
      page_to_read = 1
     };
get_game_rec(m26) ->
    %% Game Thread 1432756
    %% M26 signup threadid = 1429158
    _ = #mafia_game{
      game_num = 26,
      name = <<"MAFIA XXVI: W. Jessop Asylum for the Chronically Insane">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = 0,
      day1_dl_time = {{2017,1,5},{23,0,0}},
      is_init_dst = false,
      dst_changes = [{{{2017,3,26},{2,0,0}}, true}, %% EU 2017
                     {{{2017,10,29},{2,0,0}}, false}],
      gms = to_bin(?M26_GMs),
      players_orig = to_bin(?M26_players),
      players_rem = to_bin(?M26_players),
      player_deaths = [],
      page_to_read = 1
     };
get_game_rec(m25) ->
    %% M25 GOD QT https://www.quicktopic.com/52/H/gBqFhw3Bidb
    %% M25 spectator QT https://www.quicktopic.com/52/H/ZPja4vQgBFQ7
    _ = #mafia_game{
      key = ?M25ThId,
      game_num = 25,
      name = <<"MAFIA XXV: Kanye's Quest">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      day1_dl_time = {{2016,12,1},{18,0,0}},
      is_init_dst = false,
      dst_changes = [],
      gms = to_bin(?M25_GMs),
      players_orig = to_bin(?M25_players),
      players_rem = to_bin(?M25_players),
      player_deaths = [],
      page_to_read = 1
     };
get_game_rec(m24) ->
    _ = #mafia_game{
      key = ?M24ThId,
      game_num = 24,
      name = <<"MAFIA XXIV: Webdiplomacy's Tom Clancy's The Division">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      day1_dl_time = {{2016,10,19},{18,0,0}},
      is_init_dst = true,
      dst_changes = [{{{2016,11,6},{2,0,0}}, false},
                     {{{2017, 4,1},{2,0,0}}, true}],
      gms = to_bin(?M24_GMs),
      players_orig = to_bin(?M24_players),
      players_rem = to_bin(?M24_players),
      player_deaths = [],
      page_to_read = 1
     }.

set(K=?thread_id, V) when is_integer(V), V > 0 -> set_kv(K, V);
set(K=?thread_id, V) when is_atom(V) -> set_kv(K, ?thid(V));
set(K=?game_key, V) when is_integer(V), V > 0 -> set_kv(K, V);
set(K=?game_key, V) when is_atom(V) -> set_kv(K, ?thid(V));
set(K=?page_to_read, V) when is_integer(V), V > 0 -> set_kv(K, V);
set(K=?timer_minutes, V) when is_integer(V)-> set_kv(K, V);
set(K=?timezone_user, V) when is_integer(V), -12 =< V, V =< 12 -> set_kv(K, V);
set(K=?timezone_game, V) when is_integer(V), -12 =< V, V =< 12 -> set_kv(K, V);
set(K=?time_offset, V) when is_integer(V) -> set_kv(K, V);
set(K=?dst_user, V) when is_boolean(V) -> set_kv(K, V);
set(K=?dst_game, V) when is_boolean(V) -> set_kv(K, V);
set(K=?mod_msg, ?undefined) -> remk(K);
set(K=?mod_msg, V) when is_list(V) -> set_kv(K, V);
set(K=?console_tz, V)
  when V == user; V == game; V == utc;
       V == zulu; V == gmt ->
    set_kv(K,V).

unset(K=?timer_minutes) -> remk(K).

remk(Key) -> mnesia:dirty_delete(kv_store, Key).

set_kv(?game_key, {?error, {?undefined, GName}}) ->
    set_kv(?game_key, GName);
set_kv(Key, Value) ->
    ?dwrite_kv(#kv_store{key = Key, value = Value}).

-spec add_thread(atom(), thread_id())
                -> {reg_add | reg_exists_already | reg_thid_changes,
                    Details :: term()}.
add_thread(ThName, ThId) ->
    Regs = case ?getv(?reg_threads) of ?undefined -> []; L -> L end,
    New = {ThName, ThId},
    case lists:keyfind(ThName, 1, Regs) of
        false ->
            set_kv(?reg_threads, [New | Regs]),
            {reg_add, New};
        {_, ThId} ->
            {reg_exists_already, New};
        {_, OldThId} ->
            Regs2 = lists:keyreplace(ThName, 1, Regs, New),
            set_kv(?reg_threads, Regs2),
            {reg_thid_changes, {ThName, {OldThId, ThId}}}
    end.

-spec rm_thread(atom() | thread_id())
               ->  {reg_rm_ok | reg_rm_error, Details :: term()}.
rm_thread(ThName) when is_atom(ThName) ->
    Regs = case getv(?reg_threads) of ?undefined -> []; L -> L end,
    case lists:keyfind(ThName, 1, Regs) of
        false ->
            {reg_rm_error, thread_name_not_found};
        Item ->
            Regs2 = Regs -- [Item],
            set_kv(?reg_threads, Regs2),
            {reg_rm_ok, {item, Item}}
    end;
rm_thread(ThId) when is_integer(ThId) ->
    Regs = case getv(?reg_threads) of ?undefined -> []; L -> L end,
    case lists:keyfind(ThId, 2, Regs) of
        false ->
            {reg_rm_error, thread_id_not_found};
        Item ->
            Regs2 = Regs -- [Item],
            set_kv(?reg_threads, Regs2),
            {reg_rm_ok, {item, Item}}
    end.

create_tables() ->
    create_table(kv_store),
    create_table(page_rec),
    create_table(message),
    create_table(mafia_day),
    create_table(mafia_game),
    create_table(user),
    create_table(stat),
    create_table(cnt).

create_table(RecName) ->
    Opts = create_table_opts(RecName),
    io:format("mnesia:create_table(~p, ~p).\n", [RecName, Opts]),
    case mnesia:create_table(RecName, Opts) of
        {aborted,{already_exists,_Tab}} ->
            TI = mnesia:table_info(RecName, attributes),
            RI = rec_info(RecName),
            if TI /= RI ->
%%% HERE we should check for upgrade method and stop if upgrade does not exist
                    io:format("Delete table '~p' due to mismatching attribute "
                              "list\n",[RecName]),
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
rec_info(mafia_game) -> record_info(fields, mafia_game);
rec_info(user) -> record_info(fields, user);
rec_info(stat) -> record_info(fields, stat);
rec_info(cnt) -> record_info(fields, cnt).
