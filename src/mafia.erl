-module(mafia).

-include("mafia.hrl").

%%Chaqa died Day 5 - msg: 1427082
%%guak  died Day 5 - msg: 1427098  - DAY 5 END !

%% M25 Game END msg id 1427800
%% Time  : 2016-12-14T04:50:58 (1 N)
%% "THE GAME HAS ENDED! TOWN WINS!
%% THANK YOU ALL FOR PLAYING!"
%% https://www.quicktopic.com/52/H/gBqFhw3Bidb

%% M25 spectator QT https://www.quicktopic.com/52/H/ZPja4vQgBFQ7
%% todo:
%% - Use new DL calc and remove old calculation
%% - define deadline() :: {phase(), secs1970()} and change at all places.
%%    - make proper interface fun for deadline() -> phase().
%% - Fix proper server on lundata - start at MacOS reboot
%% - fix a better player name recognition
%% - check if abbrev code can loop forever
%% - print "global" game stats
%% ?- downl and pps should look similar, Died/Vote messages and deadline markers
%% - webpage with GM command listing
%% - GM orders :
%%   - Phase END now (and possibly move deadline -24h)
%%   - expand alias list
%%   - XXX " has died",
%%   - XXX replaces YYY
%%   - Move current deadline, full 24 hours (local time)
%%   - Move future deadline

%% interface
-export([
         setup_mnesia/0,
         remove_mnesia/0,

         end_phase/2,

         pp/0, pp/1, pp/2,
         pps/0, pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,
         print_votes/2,
         print_messages/1,

         verify_new_user_list/1,
         downl/0,
         show_settings/0,
         set_thread_id/1,
         set_death_comment/2,
         refresh_votes/0,
         refresh_votes/1,

         show_all_users/0,
         show_all_users/1,
         show_aliases/1,
         add_alias/2,
         remove_alias/2
        ]).

%% libary
-export([
         find_pages_for_thread/1,
         getv/1,
         set/2,
         rgame/0,
         rgame/1,
         rday/2,

         b2l/1,
         l2b/1,
         l2u/1,
         i2l/1,
         l2i/1,
         b2ub/1,
         lrev/1
        ]).

%% utilities
-export([grep/1, grep/2,
         l/0
        ]).

-export([cmp_vote_raw/0
        ]).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================
pm(MsgId) -> mafia_print:pm(MsgId).
pp() -> mafia_print:pp().
pp(Page) -> mafia_print:pp(Page).
pp(ThId, Page) -> mafia_print:pp(ThId, Page).
pps() -> mafia_print:pps().
pps(ThId) -> mafia_print:pps(ThId).
pps(ThId, Page) -> mafia_print:pps(ThId, Page).
print_votes() -> mafia_print:print_votes().
print_votes(DayNum) -> mafia_print:print_votes(DayNum).
print_votes(DayNum, DoN) -> mafia_print:print_votes(DayNum, DoN).
print_messages(User) -> mafia_print:print_messages(User).

downl() -> mafia_data:downl().

b2l(B) -> binary_to_list(B).
l2b(L) -> list_to_binary(L).

l2u(L) -> string:to_upper(L).
l2ub(L) -> l2b(l2u(L)).

i2l(I) -> integer_to_list(I).
l2i(L) -> list_to_integer(L).
l2a(L) -> list_to_atom(L).

b2ul(B) -> l2u(b2l(B)).
b2ub(B) -> l2b(b2ul(B)).

lrev(L) -> lists:reverse(L).

set(K,V) -> mafia_db:set(K,V).

getv(K) -> mafia_db:getv(K).

setup_mnesia() -> mafia_db:setup_mnesia().

remove_mnesia() -> mafia_db:remove_mnesia().

refresh_votes() -> mafia_data:refresh_votes().
refresh_votes(P) -> mafia_data:refresh_votes(P).

grep(Str) -> mafia_data:grep(Str).
grep(Str, Mode) -> mafia_data:grep(Str, Mode).

%% -----------------------------------------------------------------------------
%% @doc End current phase with GM message and set next phase at
%% the game's local date and time given by the GM in the same message
%% @end
%% -----------------------------------------------------------------------------
%% Example: mafia:end_phase(1427098, {{2016, 12, 13}, {18,0,0}}).
-spec end_phase(MsgId :: msg_id(),
                TimeNextDL :: datetime()) -> ok.
end_phase(MsgId, TimeNextDL) ->
    mafia_time:end_phase(MsgId, TimeNextDL).


%% load all beams in local dir
l() ->
    {ok, Files} = file:list_dir("."),
    Beams = [l2a(lrev(ModRev))
             || "maeb." ++ ModRev
                    <- [lrev(F) || F <- Files]],
    Beams2 = (Beams -- [mafia]) ++ [mafia],
    [begin code:purge(M), code:load_file(M), M end
     || M <- Beams2].

%% Read current game
rgame() ->
    ThId = getv(thread_id),
    rgame(ThId).

rgame(ThId) ->
    mnesia:dirty_read(mafia_game, ThId).

rday(ThId, {DayNum, _}) ->
    rday(ThId, DayNum);
rday(ThId, DayNum) when is_integer(ThId) ->
    rday(rgame(ThId), DayNum);
rday([#mafia_game{} = G], DayNum) ->
    rday(G, DayNum);
rday(#mafia_game{} = G, DayNum) ->
    ThId = G#mafia_game.key,
    case mnesia:dirty_read(mafia_day, Key = {ThId, DayNum}) of
        [] ->
            [#mafia_day{key = Key,
                        thread_id = ThId,
                        day = DayNum,
                        votes = [],
                        end_votes = [],
                        players_rem = G#mafia_game.players_rem,
                        player_deaths = []
                       }];
        [Day] ->
            [Day]
    end.

%% Pre-check user list given by GM in initial game PM
verify_new_user_list(25) ->
    Users = ?M25_GMs ++ ?M25_players,
    verify_new_user_list2(Users);
verify_new_user_list(24) ->
    Users = ?M24_GMs ++ ?M24_players,
    verify_new_user_list2(Users).

verify_new_user_list2(Users) ->
    [begin
         UserB = l2b(U),
         UserUB = l2ub(U),
         case mnesia:dirty_read(user, UserUB) of
             [] ->
                 io:format("User ~p is new\n",[U]);
             [#user{name = UserB,
                    verification_status = Ver}] ->
                 io:format("User ~p exist with correct case "
                           "and is ~p\n", [U, Ver]);
             [#user{name = UserB2,
                    verification_status = Ver}] ->
                 io:format("User exist with incorrect case. "
                           "Correct is ~p and it is ~p\n", [b2l(UserB2), Ver])
         end
     end
     || U <- Users].

%% Seems to be unused
-spec set_thread_id(ThId :: integer())  -> ok.
set_thread_id(ThId) when is_integer(ThId) ->
    set(thread_id, ThId),
    PageToRead =
        case find_pages_for_thread(ThId) of
            [] -> 1;
            Pages ->
                lists:max(Pages)
        end,
    set(page_to_read, PageToRead),
    ok.

-spec set_death_comment(Player :: string(),
                        Comment :: string())
                       -> ok | {error, not_found}.
set_death_comment(Player, Comment) ->
    PlayerB = l2b(Player),
    CommentB = l2b(Comment),
    [G] = rgame(),
    case lists:member(PlayerB,
                      [D#death.player
                       || D <- G#mafia_game.player_deaths]) of
        false ->
            {error, not_found};
        true ->
            SetComment =
                fun(D = #death{}) when PlayerB == D#death.player ->
                        mafia_web:regenerate_history(D#death.phase),
                        D#death{comment = CommentB};
                   (D) -> D
                end,
            Deaths2 = [SetComment(D)  || D <- G#mafia_game.player_deaths],
            mnesia:dirty_write(G#mafia_game{player_deaths = Deaths2})
    end.

show_settings() ->
    PrintSettings =
        fun(K) -> Setting = hd(mnesia:dirty_read(?kv_store, K)),
                  SetKey = element(2, Setting),
                  SetVal = element(3, Setting),
                  io:format("~p: ~p\n", [SetKey, SetVal])
        end,
    [PrintSettings(K) || K <- mnesia:dirty_all_keys(?kv_store)],
    ok.

find_pages_for_thread(ThId) ->
    MatchHead = #page_rec{key = {'$1', '$2'}, _='_'},
    Guard = {'==', '$1', ThId},
    Result = '$2',
    mnesia:dirty_select(page_rec,[{MatchHead, [Guard], [Result]}]).


show_all_users() ->
    Users = all_users(),
    io:format("All Users: ~p\n", [Users]).

show_all_users(Search) ->
    Users = all_users(Search),
    io:format("All Users: ~999p\n", [Users]).

all_users() ->
    [b2l(UserUB) || UserUB <- mnesia:dirty_all_keys(user)].

all_users(Search) ->
    [b2l(UserUB) || UserUB <- mnesia:dirty_all_keys(user),
                   0 /= string:str(b2l(UserUB), l2u(Search))].


-spec show_aliases(User :: string()) -> ok | {error, Reason :: term()}.
show_aliases(all) ->
    [begin
         U = hd(mnesia:dirty_read(user, UserUB)),
         if U#user.aliases /= [] ->
                 io:format("Found: ~s\nAliases: ~p\n",
                           [b2l(U#user.name),
                            [b2l(AlB) || AlB <- U#user.aliases]]);
            true -> ok
         end
     end
     || UserUB <- mnesia:dirty_all_keys(user)],
    ok;
show_aliases(Search) ->
    io:format("Search: ~s\n", [Search]),
    Users = all_users(Search),
    [show_aliasesI(User) || User <- Users],
    ok.


show_aliasesI(User) ->
    UserUB = l2ub(User),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] ->
            io:format("Found: ~s\nAliases: ~p\n",
                      [b2l(U#user.name),
                       [b2l(AlB) || AlB <- U#user.aliases]])
    end.

-spec add_alias(User :: string(), Alias :: string())
               -> ok | {error, Reason :: term()}.
add_alias(User, Alias) ->
    UserB = l2b(User),
    UserUB = l2ub(User),
    AliasB = l2b(Alias),
    AliasUB = l2ub(Alias),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            AliasesUB = [b2ub(AlB) || AlB <- U#user.aliases],
            case lists:member(AliasUB, AliasesUB) of
                true ->
                    {error, alias_exist_already};
                false ->
                    mnesia:dirty_write(U#user{aliases = [AliasB|AliasesB]}),
                    ok
            end
    end.

-spec remove_alias(User :: string(), Alias :: string())
                  -> ok | {error, Reason :: term()}.
remove_alias(User, Alias) ->
    UserB = l2b(User),
    UserUB = l2ub(User),
    AliasB = l2b(Alias),
    case mnesia:dirty_read(user, UserUB) of
        [] -> {error, user_not_found};
        [#user{} = U] when U#user.name /= UserB ->
            {error, user_case_not_matching};
        [#user{aliases = AliasesB} = U] ->
            case lists:member(AliasB, AliasesB) of
                true ->
                    NewAliasesB = AliasesB -- [AliasB],
                    mnesia:dirty_write(U#user{aliases = NewAliasesB}),
                    ok;
                false ->
                    {error, alias_does_not_exist}
            end
    end.

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

%% No-one seems to use this one.
cmp_vote_raw() ->
    ThId = getv(thread_id),
    DayNum = 1,
    Key = {ThId, DayNum},
    case mnesia:dirty_read(mafia_day, Key) of
        [] ->
            ignore;
        [#mafia_day{votes = GVotes}] ->
            [begin
                 VoteSum =
                     [{b2l(V#vote.vote), b2l(V#vote.raw), V#vote.valid}
                      || V <- Votes],
                 {b2l(User), VoteSum}
             end || {User, Votes} <- GVotes]
    end.
