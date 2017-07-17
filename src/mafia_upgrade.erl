-module(mafia_upgrade).

%% pds upgrade is complex/advanced -> too much at the moment

-export([
         recreate_game_table/0,
         upgrade/0,
         upgrade/1,
         update_db_attributes/0,
         update_db_attributes/1,
         fix_deaths_in_games/0,

         get_aliases/1,
         save_copy_on_file/2,
         read_file_copy/1
        ]).

-include("mafia.hrl").

%% The mafia_game table do not need soft upgrades!
recreate_game_table() ->
    T = mafia_game,
    mnesia:delete_table(T),
    mafia_db:create_table(T),
    mafia:refresh_votes().

%% mnesia_table arity is number of fields + 1
upgrade() ->
    upgrade(mafia_game),
    upgrade(user).

upgrade(mafia_game = Tab) ->
    upgrade(Tab,
            mnesia:table_info(Tab, attributes),
            record_info(fields, mafia_game));
upgrade(user) ->
    upgrade(user,
            mnesia:table_info(user, attributes),
            record_info(fields, user)).

upgrade(Tab = user,
        As = [name_upper, name, aliases, verification_status, pw_hash],
        Fs = [name_upper, name, aliases, verification_status, pw_hash,
              logins %% added
             ]) ->
    upgrade_tab_user_170717(Tab, As, Fs);
upgrade(Tab = mafia_game,
        As = [game_num,thread_id,name,day_hours,night_hours,time_zone,
              start_time,dst_zone,dst_changes,deadlines,gms,players_orig,
              players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time],
        Fs = [game_num,thread_id,
              signup_thid, %% added
              name,day_hours,night_hours,time_zone,
              start_time,dst_zone,dst_changes,deadlines,gms,players_orig,
              players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time]) ->
    upgrade_tab_game_170717(Tab, As, Fs);
upgrade(Tab = user,
        As = [name_upper, name, aliases, verification_status],
        Fs = [name_upper, name, aliases, verification_status, pw_hash]) ->
    upgrade_tab_user_170628(Tab, As, Fs);
upgrade(Tab = mafia_game,
        As = [game_num,thread_id,name,day_hours,night_hours,time_zone,
              day1_dl_time, is_init_dst, %% renamed
              dst_changes,               %% content change
              deadlines,gms,players_orig,
              players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time],
        Fs = [game_num,thread_id,name,day_hours,night_hours,time_zone,
              start_time, dst_zone,      %% renamed
              dst_changes,               %% content change
              deadlines,gms,players_orig,
              players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time]
       ) ->
    upgrade_tab_game_170627(Tab, As, Fs);
upgrade(Tab = mafia_game,
        As = [key,name,day_hours,night_hours,time_zone,day1_dl_time,
              is_init_dst,dst_changes,deadlines,gms,players_orig,
              players_rem,game_num,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time],
        Fs = [game_num,thread_id,name,day_hours,night_hours,time_zone,
              day1_dl_time,is_init_dst,dst_changes,deadlines,gms,players_orig,
              players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time]) ->
    upgrade_tab_game_170412(Tab, As, Fs);
upgrade(Tab = user,
        As = [name_upper, name, verification_status],
        Fs = [name_upper, name, aliases, verification_status]) ->
    upgrade_tab_user_161210(Tab, As, Fs);
upgrade(Tab, As, Fs) when As == Fs ->
    io:format("No upgrade for table '~p' with attributes: ~999p\n", [Tab, As]);
upgrade(Tab, As, Fs) ->
    io:format("No upgrade for table '~p' from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]).

%% -----------------------------------------------------------------------------
%% insert ?undefined for signup_thid
%% -----------------------------------------------------------------------------
upgrade_tab_game_170717(Tab, As, Fs) ->
    io:format("Upgrade table '~p' 170717 from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                ValuesOldList = tl(tuple_to_list(RecOld)),
                OldAsVals = lists:zip(As, ValuesOldList),

                %% Pick values for new rec
                NewValues = [proplists:get_value(F, OldAsVals) || F <- Fs],
                list_to_tuple([Tab | NewValues])
        end,
    mnesia:transform_table(Tab, Trans, Fs).

%% -----------------------------------------------------------------------------
%% start_time, dst_zone, dst_changes
%% -----------------------------------------------------------------------------
upgrade_tab_game_170627(Tab, As, Fs) ->
    %% day1_dl_time, is_init_dst, %% renamed
    %% dst_changes,               %% content change

    %% start_time, dst_zone,      %% renamed
    %% dst_changes,               %% content change
    io:format("Upgrade table '~p' 170627 from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                ValuesOldList = tl(tuple_to_list(RecOld)),
                OldAsVals = lists:zip(As, ValuesOldList),

                DL1DT = proplists:get_value(day1_dl_time, OldAsVals),
                %% {{2017,4,17},{18,0,0}}
                DayHs = proplists:get_value(day_hours, OldAsVals),
                StartTime =
                    calendar:gregorian_seconds_to_datetime(
                      calendar:datetime_to_gregorian_seconds(DL1DT)
                      - DayHs * ?HourSecs),
                TimeZone = proplists:get_value(time_zone, OldAsVals),
                DstZone = if TimeZone < -2 -> ?usa;
                             TimeZone < 5 -> ?eu;
                             true -> ?australia
                          end,

                %% Pick values for new rec
                NewVal = fun(start_time) -> StartTime;
                            (dst_zone) -> DstZone;
                            (A) -> proplists:get_value(A, OldAsVals)
                         end,
                NewValues = [NewVal(F) || F <- Fs],
                Game = list_to_tuple([Tab | NewValues]),
                RecNew = mafia_time:set_dst_changes(Game),

                %% Modify values
                io:format("Update GameNum ~p\n", [RecNew#mafia_game.game_num]),
                RecNew
        end,
    mnesia:transform_table(Tab, Trans, Fs).

%% -----------------------------------------------------------------------------
%% Upgrade mnesia_table using new keys
%% 1. copy objects with new value order, 2. transform attribute list in mnesia
%% -----------------------------------------------------------------------------
upgrade_tab_game_170412(Tab, As, Fs) ->
    io:format("Upgrade table '~p' from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                ValuesOldList = tl(tuple_to_list(RecOld)),
                NameChange = fun(key) -> thread_id; (N) -> N end,
                As2 = [NameChange(A) || A <- As],
                KVs = lists:zip(As2, ValuesOldList),
                Values = [proplists:get_value(F, KVs) || F <- Fs],
                RecNew = list_to_tuple([Tab | Values]),
                io:format("~p\n", [RecNew]),
                RecNew
        end,
    KeysToChange = [ K || K <- mnesia:dirty_all_keys(mafia_game),
                          not is_integer(K) orelse K > 999999],
    lists:foreach(fun(Key) ->
                          RecOld = hd(mnesia:dirty_read(Tab, Key)),
                          RecNew = Trans(RecOld),
                          mnesia:dirty_write(RecNew),
                          mnesia:dirty_delete(Tab, Key)
                  end,
                  KeysToChange),
    mnesia:transform_table(Tab, ignore, Fs).

%% -----------------------------------------------------------------------------
%% Add field is_deleted into #death{}, 161211
%% -----------------------------------------------------------------------------
fix_deaths_in_games() ->
    AddFalseAtEnd =
        fun(DT) -> DL = tuple_to_list(DT) ++ [false],
                   list_to_tuple(DL)
        end,
    AddAttr =
        fun(ThId) ->
                G = hd(?rgame(ThId)),
                Deaths = [AddFalseAtEnd(D) || D <- G#mafia_game.player_deaths],
                ?dwrite_game(G#mafia_game{player_deaths = Deaths})
        end,
    [ AddAttr(ThId) || ThId <- mnesia:dirty_all_keys(mafia_game)].


%% -----------------------------------------------------------------------------
%% Change to players_mafia_game attribute list, 161211
%% -----------------------------------------------------------------------------
update_db_attributes() ->
    update_db_attributes(mafia_game),
    update_db_attributes(mafia_day).

update_db_attributes(mafia_game) ->
    mnesia:transform_table(mafia_game,
                           ignore,
                           record_info(fields, mafia_game));
update_db_attributes(mafia_day) ->
    mnesia:transform_table(mafia_day,
                           ignore,
                           record_info(fields, mafia_day)).

%% -----------------------------------------------------------------------------
%% Add aliases to user table, 161210
%% -----------------------------------------------------------------------------

-define(Aliases, [{"RagingIke297", ["Ike"]},
                  {"WardenDresden", ["WD"]},
                  {"Glen_Alexander", ["GA"]},
                  {"DemonRHK", ["RHK"]},
                  {"CaptainMeme", ["Meme"]},
                  {"Hellenic Riot", ["HR", "H.R."]},
                  {"brainbomb", ["BB"]},
                  {"No-Lynch", ["No Lynch"]}
                 ]).

upgrade_tab_user_170717(Tab, As, Fs) ->
    io:format("Upgrading table '~p' 170717 from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans = append_last([]),
    mnesia:transform_table(Tab, Trans, Fs).

upgrade_tab_user_170628(Tab, As, Fs) ->
    io:format("Upgrading table '~p' 170628 from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans = append_last(?undefined),
    mnesia:transform_table(Tab, Trans, Fs).

append_last(LastValue) ->
    fun(RecOld) ->
            NewList = tuple_to_list(RecOld) ++ [LastValue],
            list_to_tuple(NewList)
    end.

upgrade_tab_user_161210(Tab, As, Fs) ->
    io:format("Upgrading table '~p' from:\n~999p\nto\n~999p\n", [Tab, As, Fs]),
    save_copy_on_file(user, "user_add_alias"),
    Trans =
        fun({_, NameUB, NameB, VerSt}) ->
                Aliases = get_aliases(NameB),
                #user{name_upper = NameUB,
                      name = NameB,
                      aliases = Aliases,
                      verification_status = VerSt}
        end,
    mnesia:transform_table(Tab, Trans, record_info(fields, user)).

get_aliases(NameB) ->
    case lists:keyfind(?b2l(NameB), 1, ?Aliases) of
        false -> [];
        {_, As} -> [?l2b(A) || A <- As]
    end.

%% -----------------------------------------------------------------------------

save_copy_on_file(Tab, FileName) when FileName /= "" ->
    FileName2 = FileName ++ ".upg.bak",
    %% Make sure we do not overwrite
    {error, enoent} = file:read_file_info(FileName2),
    RecList = ets:tab2list(Tab),
    file:write_file(FileName2, term_to_binary(RecList)).

read_file_copy(FileName) ->
    FileName2 = FileName ++ ".upg.bak",
    {ok, Bin} = file:read_file(FileName2),
    binary_to_term(Bin).
