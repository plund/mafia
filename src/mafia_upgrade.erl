-module(mafia_upgrade).

%% pds upgrade is complex/advanced -> too much at the moment

-export([
         upgrade/0,
         update_db_attributes/0,
         update_db_attributes/1,
         fix_deaths_in_games/0,

         get_aliases/1,
         save_copy_on_file/2,
         read_file_copy/1
        ]).

-include("mafia.hrl").

-import(mafia, [b2l/1, l2b/1]).

%% mnesia_table arity is number of fields + 1
upgrade() ->
    upgrade(user,
            mnesia:table_info(user, attributes),
            record_info(fields, user)).


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
                G = hd(mafia:rgame(ThId)),
                Deaths = [AddFalseAtEnd(D) || D <- G#mafia_game.player_deaths],
                mnesia:dirty_write(G#mafia_game{player_deaths = Deaths})
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

upgrade(Tab = user,
        As = [name_upper, name, verification_status],
        Fs = [name_upper, name, aliases, verification_status]) ->
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
    mnesia:transform_table(user, Trans, record_info(fields, user));
upgrade(Tab, As, Fs) ->
    io:format("No upgrade for table '~p' from:\n~999p\nto\n~999p\n",
              [Tab, As, Fs]).

get_aliases(NameB) ->
    case lists:keyfind(b2l(NameB), 1, ?Aliases) of
        false -> [];
        {_, As} -> [l2b(A) || A <- As]
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
