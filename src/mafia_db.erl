-module(mafia_db).

-include("mafia.hrl").
-include("mafia_game.hrl").

-export([set/2,
         %% unset/1,
         getv/1,
         getv/2,

         setup_mnesia/0,
         remove_mnesia/0,
         create_tables/0,
         create_table/1,
         add_table_indices/0,
         get_game_rec/1,

         reset_game/1,
         write_game/1,
         rewrite_game/1,
         write_game/2
        ]).

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
    io:format("~p\n", [setup_mnesia]),
    case mnesia:system_info(is_running) of
        no ->
            io:format("~p\n", [setup_mnesia_no]),
            case mnesia:create_schema([node()]) of
                {error,{_,{already_exists,_}}} ->
                    start_mnesia(already_exists),
                    mnesia:add_table_index(user, #user.name),
                    schema_existed_already;
                Other ->
                    start_mnesia(do_create),
                    add_table_indices(),
                    Other
            end;
        yes ->
            io:format("~p\n", [setup_mnesia_running]),
            already_started
    end.

add_table_indices() ->
    mnesia:add_table_index(user, #user.name),
    mnesia:add_table_index(?escape_sequence, #?escape_sequence.esc_seq_upper).

remove_mnesia() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

-spec start_mnesia(Op :: already_exists | do_create )
                  -> mnesia_start_ok |
                     {error, Reason::term()}.
start_mnesia(Op) ->
    io:format("=== start_mnesia ~p\n", [Op]),
    case mnesia:start() of
        ok ->
            if Op == do_create ->
                    create_tables();
               true ->
                    ok
            end,
            timer:sleep(500),
            io:format("~p\n", [{start_mnesia, ok}]),
            mnesia_start_ok;
        Other ->
            io:format("start_mnesia, other"),
            Other
    end.

%% List of lists
to_bin(LoL = [[_|_]|_]) -> [?l2b(L) || L <- LoL].

to_bin_sort(LoL) -> mafia_lib:to_bin_sort(LoL).

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
    ?dwrite_game(game_db1, G2).

%% Write game record to DB
write_game(GNum, ThId) when is_integer(GNum), is_integer(ThId) ->
    G = get_game_rec(GNum),
    io:format("Setting thread id for Mafia Game\n  ~p: ~s\n",
              [GNum, ?b2l(G#mafia_game.name)]),
    G2 = G#mafia_game{thread_id = ThId},
    ?dwrite_game(game_db2, G2).

write_game(GNum) when is_integer(GNum) ->
    G = get_game_rec(GNum),
    ?dwrite_game(game_db3, G).

rewrite_game(GNum) when is_integer(GNum) ->
    G = make_game_and_deadlines(GNum),
    ?dwrite_game(game_db4, G).

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

conv_to_name(L) when is_list(L) ->
    [conv_to_name(User) || User = {_, _} <- L];
conv_to_name({U, Site}) when ?IS_SITE_OK(Site) ->
    case ?ruserUB(U, Site) of
        [#user{name = {Name, _}}] ->
            {?b2l(Name), Site};
        [] ->
            throw({error, {user_not_in_db, {U, Site}}})
    end;
conv_to_name({_U, Site}) when not ?IS_SITE_OK(Site) ->
    throw({error, {site_not_defined, Site}}).

set(K = ?server_keeper, {U, Site}) ->
    try set_kv(K, conv_to_name({U, Site}))
    catch {error, Err} -> Err
    end;
set(K = ?server_admins, Admins) when is_list(Admins) ->
    try set_kv(K, conv_to_name(Admins))
    catch Err = {error, _} -> Err
    end;
set(K=?thread_id, V) when is_integer(V), V > 0 -> set_kv(K, V);
set(K=?game_key, V) when is_integer(V), V > 0 -> set_kv(K, V);
set(K=?page_to_read, V) when is_integer(V), V > 0 -> set_kv(K, V);
%% set(K=?timer_minutes, V) when is_integer(V)-> set_kv(K, V);
set(K=?timezone_user, V) when is_integer(V), -12 =< V, V =< 12 -> set_kv(K, V);
set(K=?timezone_game, V) when is_integer(V), -12 =< V, V =< 12 -> set_kv(K, V);
set(K=?time_offset, V) when is_integer(V) -> set_kv(K, V);
set(K=?dst_user, V) when is_boolean(V) -> set_kv(K, V);
set(K=?dst_game, V) when is_boolean(V) -> set_kv(K, V);
set(K=?mod_msg, ?undefined) -> remk(K);
set(K=?mod_msg, V) when is_list(V) -> set_kv(K, V);
set(K=?ntp_offset_secs, V) when is_number(V) -> set_kv(K, V);
set(K=?console_tz, V)
  when V == user; V == game; V == utc;
       V == zulu; V == gmt ->
    set_kv(K,V).

%% unset(K=?timer_minutes) -> remk(K).

remk(Key) -> mnesia:dirty_delete(kv_store, Key).

set_kv(Key, Value) ->
    ?dwrite_kv(#kv_store{key = Key, value = Value}).

create_tables() ->
    create_table(kv_store),
    create_table(page_rec),
    create_table(message),
    create_table(mafia_day),
    create_table(mafia_game),
    create_table(user),
    create_table(stat),
    create_table(cnt),
    create_table(?escape_sequence).

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
    end,
    case mnesia:table_info(?escape_sequence, size) of
        0 -> read_escape_file();
        _ -> ok
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
rec_info(cnt) -> record_info(fields, cnt);
rec_info(?escape_sequence) -> record_info(fields, ?escape_sequence).

read_escape_file() ->
    {ok, Fd} = file:open("priv/escapes.txt", [read]),
    read_ef(Fd).

read_ef(Fd) ->
    case file:read_line(Fd) of
        {ok, "#" ++ _ = Comment} ->
            io:format("Comment ~s", [Comment]),
            read_ef(Fd);
        {ok, Line} ->
            Toks = string:split(Line, "\t", all),
            UnicodePoint =
                hd(unicode:characters_to_list(
                     list_to_binary(
                       lists:nth(2, Toks)))),
            EscSeq = string:strip(hd(Toks), right, $\s),
            io:format("~ts, ~s, ~p : ~s",
                      [[UnicodePoint],
                       lists:nth(3, Toks),
                       EscSeq,
                       lists:nth(7, Toks)]),
            mnesia:dirty_write(
              #?escape_sequence{?escape_sequence = ?l2b(EscSeq),
                                esc_seq_upper = ?l2ub(EscSeq),
                                unicode_point = UnicodePoint}),
            read_ef(Fd);
        eof -> file:close(Fd);
        {error, _} = Err ->
            io:format("Err ~p\n", [Err]),
            file:close(Fd)
    end.
