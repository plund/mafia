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
         get_game_rec/1,

         reset_game/1,
         write_game/1,
         rewrite_game/1,
         write_game/2,
         write_default_user_table/0
        ]).


%% Pre-check user list given by GM in initial game PM
verify_new_user_list(29) ->
    Users = ?M29_GMs ++ ?M29_players ++ ?M29_subs,
    verify_new_user_list2(Users);
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
                    create_tables();
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

to_bin_sort(LoL) -> mafia_lib:to_bin_sort(LoL).

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

-define(Cpy(Template, Field), Field = Template#mafia_game.Field).
reset_game(GNum) ->
    T = make_game_rec(GNum),
    G = get_game_rec(GNum),
    G2 = G#mafia_game{?Cpy(T, players_orig),
                      ?Cpy(T, players_rem),
                      ?Cpy(T, player_deaths),
                      ?Cpy(T, page_to_read),
                      ?Cpy(T, game_end),
                      ?Cpy(T, last_msg_id),
                      ?Cpy(T, last_msg_time)},
    ?dwrite_game(G2).

%% Write game record to DB
write_game(GNum, ThId) when is_integer(GNum), is_integer(ThId) ->
    G = get_game_rec(GNum),
    io:format("Setting thread id for Mafia Game\n  ~p: ~s\n",
              [GNum, ?b2l(G#mafia_game.name)]),
    G2 = G#mafia_game{thread_id = ThId},
    ?dwrite_game(G2).

write_game(GNum) when is_integer(GNum) ->
    G = get_game_rec(GNum),
    ?dwrite_game(G).

rewrite_game(GNum) when is_integer(GNum) ->
    G = make_game_and_deadlines(GNum),
    ?dwrite_game(G).

get_game_rec(GNum) when is_integer(GNum) ->
    case ?rgame(GNum) of
        [G] ->
            io:format("Updating deadlines for Mafia Game\n  ~p: ~s\n",
                      [GNum, ?b2l(G#mafia_game.name)]),
            mafia_time:initial_deadlines(G);
        [] ->
            make_game_and_deadlines(GNum)
    end.

make_game_and_deadlines(GNum) ->
    G = make_game_rec(GNum),
    io:format("Initializing Mafia Game\n  ~p: ~s\n",
              [GNum, ?b2l(G#mafia_game.name)]),
    mafia_time:initial_deadlines(G).

make_game_rec(29 = GNum) ->
    _ = #mafia_game{
      game_num = GNum,
      name = <<"Mafia XXIX: Revenge of the Aliens!">>,
      thread_id   = 1479977,
      signup_thid = 1478635,
      day_hours = 48,
      night_hours = 24,
      time_zone = -6,
      start_time = {{2017,5,30},{17,0,0}},
      dst_zone = ?usa,
      gms = to_bin(?M29_GMs),
      players_orig = to_bin_sort(?M29_players),
      players_rem = to_bin_sort(?M29_players)
     };
make_game_rec(28) ->
    _ = #mafia_game{
      game_num = 28,
      name = <<"Mafia 28: -JEBEDIAH'S WRATH-">>, %% Item Madness
      thread_id   = 1465173,
      signup_thid = 1460042,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      start_time = {{2017,3,26},{18,0,0}},
      dst_zone = ?usa,
      gms = to_bin(?M28_GMs),
      players_orig = to_bin_sort(?M28_players),
      players_rem = to_bin_sort(?M28_players)
     };
make_game_rec(27) ->
    _ = #mafia_game{
      game_num = 27,
      name = <<"Mafia 27: Welcome to Westworld">>,
      thread_id   = 1447615,
      signup_thid = 1442470,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      start_time = {{2017,2,6},{18,0,0}},
      dst_zone = ?usa,
      gms = to_bin(?M27_GMs),
      players_orig = to_bin_sort(?M27_players),
      players_rem = to_bin_sort(?M27_players)
     };
make_game_rec(26) ->
    _ = #mafia_game{
      game_num = 26,
      name = <<"MAFIA XXVI: W. Jessop Asylum for the Chronically Insane">>,
      thread_id   = 1432756,
      signup_thid = 1429158,
      day_hours = 48,
      night_hours = 24,
      time_zone = 0,
      start_time = {{2017,1,3},{23,0,0}},
      dst_zone = ?eu,
      gms = to_bin(?M26_GMs),
      players_orig = to_bin(?M26_players),
      players_rem = to_bin(?M26_players)
     };
make_game_rec(25) ->
    %% M25 GOD QT https://www.quicktopic.com/52/H/gBqFhw3Bidb
    %% M25 spectator QT https://www.quicktopic.com/52/H/ZPja4vQgBFQ7
    _ = #mafia_game{
      game_num = 25,
      thread_id = ?M25ThId,
      name = <<"MAFIA XXV: Kanye's Quest">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      start_time = {{2016,11,29},{18,0,0}},
      dst_zone = ?usa,
      gms = to_bin(?M25_GMs),
      players_orig = to_bin(?M25_players),
      players_rem = to_bin(?M25_players)
     };
make_game_rec(24) ->
    _ = #mafia_game{
      game_num = 24,
      thread_id = ?M24ThId,
      name = <<"MAFIA XXIV: Webdiplomacy's Tom Clancy's The Division">>,
      day_hours = 48,
      night_hours = 24,
      time_zone = -5,
      start_time = {{2016,10,17},{18,0,0}},
      dst_zone = ?usa,
      gms = to_bin(?M24_GMs),
      players_orig = to_bin(?M24_players),
      players_rem = to_bin(?M24_players)
     }.

set(K=?server_keeper, U) ->
    case ?ruserUB(U) of
        [User] -> set_kv(K, ?b2l(User#user.name));
        [] -> {error, user_not_in_db}
    end;
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
