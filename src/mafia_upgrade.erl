-module(mafia_upgrade).

-export([
         recreate_game_table/0,
         upgrade/0,
         upgrade/1,
         update_db_attributes/0,
         update_db_attributes/1,
         fix_deaths_in_games/0,

         save_copy_on_file/2,
         read_file_copy/1
        ]).

%% temp exports during development
-export([
         role_pm_bin/1,
         signup/1
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
    upgrade(user),
    upgrade(page_rec),
    upgrade(message),
    ok.

upgrade(mafia_game = Tab) ->
    upgrade(Tab,
            mnesia:table_info(Tab, attributes),
            record_info(fields, mafia_game));
upgrade(user = Tab) ->
    upgrade(Tab,
            mnesia:table_info(Tab, attributes),
            record_info(fields, user));
upgrade(page_rec = Tab) ->
    upgrade(Tab,
            mnesia:table_info(Tab, attributes),
            record_info(fields, page_rec));
upgrade(message = Tab) ->
    upgrade(Tab,
            mnesia:table_info(Tab, attributes),
            record_info(fields, message)).

upgrade(Tab = message,
        As = [msg_id,thread_id,page_num,user_name,time,message],
        Fs = [msg_key,thread_id,page_num,user_name,time,message]
       ) ->
    upgrade_tab_message_171008(Tab, As, Fs);
upgrade(Tab = page_rec,
        As = [key,message_ids,thread_id,complete],
        Fs = [key,site,thread_id,message_ids,complete]
       ) ->
    upgrade_tab_page_rec_171008(Tab, As, Fs);
upgrade(Tab = mafia_game,
        As = [game_num,thread_id,signup_thid,name,day_hours,night_hours,
              time_zone,start_time,dst_zone,dst_changes,deadlines,gms,
              players_orig,players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time, role_pm],
        Fs = [game_num,
              site, % new
              thread_id,signup_thid,name,day_hours,night_hours,
              time_zone,start_time,dst_zone,dst_changes,deadlines,gms,
              players_orig,players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time, role_pm]) ->
    upgrade_tab_game_171005(Tab, As, Fs);
upgrade(Tab = user,
        As = [name_upper, name, aliases, verification_status, pw_hash,
              logins],
        Fs = [name_upper, name, site, % new
              aliases, verification_status, pw_hash,
              logins]) ->
    upgrade_tab_user_171005(Tab, As, Fs);
upgrade(Tab = mafia_game,
        As = [game_num,thread_id,signup_thid,name,day_hours,night_hours,
              time_zone,start_time,dst_zone,dst_changes,deadlines,gms,
              players_orig,players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time],
        Fs = [game_num,thread_id,signup_thid,name,day_hours,night_hours,
              time_zone,start_time,dst_zone,dst_changes,deadlines,gms,
              players_orig,players_rem,player_deaths,page_to_read,game_end,
              last_msg_id,last_msg_time, role_pm]) ->
    upgrade_tab_game_170729(Tab, As, Fs);
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
upgrade(Tab, As, Fs) when As == Fs ->
    io:format("No upgrade for table '~p' with attributes: ~999p\n", [Tab, As]);
upgrade(Tab, As, Fs) ->
    io:format("No upgrade for table '~p' from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]).

upgrade_tab_message_171008(Tab, _As, Fs) ->
    mnesia:transform_table(Tab, ignore, Fs),
    case is_integer(mnesia:dirty_first(message)) of
        true ->  do_upgrade_tab_message_171008();
        false -> already_done
    end.

do_upgrade_tab_message_171008() ->
    %% mnesia:table_info(message, size) -> 53281
    %% Now rewrite primary keys.
    Site = fun(MsgId) when MsgId < ?MaxThIdvDip -> ?vDip;
              (_) -> ?webDip
           end,
    [begin
         [M] = mnesia:dirty_read(message, MsgId),
         M2 = setelement(#message.msg_key, M, {MsgId, Site(MsgId)}),
         mnesia:dirty_write(M2),
         mnesia:dirty_delete(message, MsgId)
     end
     || MsgId <- mnesia:dirty_all_keys(message), is_integer(MsgId)],
    ok.

upgrade_tab_page_rec_171008(Tab, As, Fs) ->
    Site = fun(ThId) when ThId < ?MaxThIdvDip -> ?vDip;
              (_) -> ?webDip
           end,
    Trans =
        fun(RecOld) ->
                P = move_old_vals_to_new_pos(RecOld, As, Fs),
                {ThId, _PNum} = element(#page_rec.key, P),
                setelement(#page_rec.site, P, Site(ThId))
        end,
    mnesia:transform_table(Tab, Trans, Fs),
    case tuple_size(mnesia:dirty_first(page_rec)) of
        2 -> do_upgrade_tab_page_rec_171008(Site);
        _ -> already_done
    end.

do_upgrade_tab_page_rec_171008(Site) ->
    %% mnesia:table_info(page_rec, size) -> 1785
    %% Now rewrite primary keys.
    [begin
         [P] = mnesia:dirty_read(page_rec, {ThId, PNum}),
         P2 = setelement(#page_rec.key, P, {ThId, PNum, Site(ThId)}),
         mnesia:dirty_write(P2),
         mnesia:dirty_delete(page_rec, {ThId, PNum})
     end
     || {ThId, PNum} <- mnesia:dirty_all_keys(page_rec)],
    ok.

%% -----------------------------------------------------------------------------
%% insert ?undefined for signup_thid
%% -----------------------------------------------------------------------------
upgrade_tab_game_171005(Tab, As, Fs) ->
    io:format("Upgrade table '~p' 171003, adding 'site'\n"
              "from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                G = move_old_vals_to_new_pos(RecOld, As, Fs),
                G#mafia_game{site = ?webDip}
        end,
    mnesia:transform_table(Tab, Trans, Fs).

%% -----------------------------------------------------------------------------
%% add role_pm and set some signup_thid with values found in index.html
%% -----------------------------------------------------------------------------
upgrade_tab_game_170729(Tab, As, Fs) ->
    io:format("Upgrade table '~p' 170729 from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                ValuesOldList = tl(tuple_to_list(RecOld)),
                OldAsVals = lists:zip(As, ValuesOldList),
                GNum = proplists:get_value(game_num, OldAsVals),
                Val = fun(role_pm) -> role_pm_bin(GNum);
                         (F = signup_thid) ->
                              case signup(GNum) of
                                  ?undefined ->
                                      proplists:get_value(F, OldAsVals);
                                  SuThid -> SuThid
                              end;
                         (F) -> proplists:get_value(F, OldAsVals)
                      end,
                %% Pick values for new rec
                NewValues = [Val(F) || F <- Fs],
                list_to_tuple([Tab | NewValues])
        end,
    mnesia:transform_table(Tab, Trans, Fs).

role_pm_bin(N) ->
    case role_pm(N) of
        ?undefined -> ?undefined;
        Str -> ?l2b(Str)
    end.

%% From index.html
role_pm(30) -> "https://docs.google.com/document/d/"
                   "1KKPL7Lfpq99w9D1nMMm9dmeJ5uQcSnXYwR452ghAnH4/edit";
role_pm(29) -> "msgs?g=29&user=bo_sox48&part=1";
role_pm(28) ->
    "https://docs.google.com/document/d/"
        "1qwQXijL-wxmPJ1VrDYq5cMQmlFxmTb668bWW-lpO7nw/edit?usp=sharing";
role_pm(27) ->
    "https://docs.google.com/document/d/"
        "1szNjvCVOmuEbCJwLJgDU1wQHwyjdd4cN_x2M1d0EYfE/edit?usp=sharing";
role_pm(26) -> "https://docs.google.com/document/d/"
                   "1R_l0giAp0DwocZ4Koz1PEJJgIBbbsCPRvBWJXUsQmfs/pub";
role_pm(25) -> "msgs?g=25&user=VashtaNeurotic&part=p1";
role_pm(24) ->
    "https://docs.google.com/document/d/"
        "1n13cK_Zqo9WIsvIkWck0HtN8JwAhSpT0gwUOM-KVKYY/edit?usp=sharing";
role_pm(23) ->
    "https://docs.google.com/document/d/"
        "1DuNXadUns-6HxSlpHG5BwvS8Kkelfn3PJcH0FLTaxDI/edit?usp=sharing";
role_pm(22) -> "msgs?g=22&user=ghug&part=p1";
role_pm(_) -> ?undefined.

signup(29) -> 1478635;
signup(28) -> 1460042;
signup(27) -> 1442470;
signup(_) -> ?undefined.

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
                ?dwrite_game(game_u1, G#mafia_game{player_deaths = Deaths})
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

upgrade_tab_user_171005(Tab, As, Fs) ->
    %% Cmp with upgrade_tab_game_170412 where primary keys also were modified
    %% but ...
    %% This is more advanced, since both the primary key and the attribute
    %% list are modified at the same time.
    io:format("Upgrading table '~p' 171003\n"
              "adding site to both primary and secondary keys\n"
              "from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]),
    Trans =
        fun(RecOld) ->
                U1 = move_old_vals_to_new_pos(RecOld, As, Fs),
                %% Using element and setelement to get rid of dialyzer warnings
                NameU = element(#user.name_upper, U1),
                Name = element(#user.name, U1),
                U2 = setelement(#user.name_upper, U1, {NameU, ?webDip}),
                U3 = setelement(#user.name, U2, {Name, ?webDip}),
                setelement(#user.site, U3, ?webDip)
        end,
    OldPrimKeys = mnesia:dirty_all_keys(user),
    Delete =
        fun(Key) ->
                RecOld = hd(mnesia:dirty_read(Tab, Key)),
                RecNew = Trans(RecOld),
                mnesia:dirty_delete(Tab, Key),
                RecNew
        end,
    NewUsers = [ Delete(PK)|| PK <- OldPrimKeys],
    %% Change table format *before* writing new users back!
    mnesia:transform_table(Tab, ignore, Fs),
    _ = [mnesia:dirty_write(NUser) || NUser <- NewUsers],
    ok.

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

%% -----------------------------------------------------------------------------
%% @doc Return new record with same name with different number of fields.
%% Values are moved to a new position when the field name is the same in Fs
%% as in As.
move_old_vals_to_new_pos(RecOld, As, Fs) ->
    [Tab | ValuesOldList] = tuple_to_list(RecOld),
    OldAsVals = lists:zip(As, ValuesOldList),
    %% Pick values for new rec
    NewValues = [proplists:get_value(F, OldAsVals) || F <- Fs],
    list_to_tuple([Tab | NewValues]).

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
