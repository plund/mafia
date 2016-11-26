-module(mafia).

-include("mafia.hrl").

%% todo:
%% - show last raw vote, if there is no 'approved' vote for a user
%% - print_vote variant for one voter
%% - webpage with game status:
%%   - Current phase -current votes -link to vote breakdown
%%   - if day -Next DL -link to deadline schedule -GMs - Players Living -Dead
%% - GM orders :
%%   - expand alias list
%%   - XXX " has died",
%%   - XXX replaces YYY
%%   - Move current deadline, full 24 hours (local time)
%%   - Move future deadline
%% - ##Unvote ##UNEND, ##END??

%% interface
-export([
         setup_mnesia/0,
         remove_mnesia/0,

         pp/0, pp/1, pp/2,
         pps/1, pps/2,
         pm/1,
         print_votes/0,
         print_votes/1,

         downl/0,
         show_settings/0,
         set_thread_id/1,
         refresh_votes/0
        ]).

%% libary
-export([
         find_pages_for_thread/1,
         getv/1,
         set/2,

         b2l/1,
         l2b/1,
         i2l/1,
         l2i/1
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
pps(Page) -> mafia_print:pps(Page).
pps(ThId, Page) -> mafia_print:pps(ThId, Page).
print_votes() -> mafia_print:print_votes().
print_votes(DayNum) -> mafia_print:print_votes(DayNum).

downl() -> mafia_data:downl().

i2l(I) -> integer_to_list(I).
l2i(L) -> list_to_integer(L).

b2l(B) -> binary_to_list(B).
l2b(L) -> list_to_binary(L).

set(K,V) -> mafia_db:set(K,V).

getv(K) -> mafia_db:getv(K).

setup_mnesia() -> mafia_db:setup_mnesia().

remove_mnesia() -> mafia_db:remove_mnesia().

refresh_votes() -> mafia_data:refresh_votes().

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

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

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

%%PrintFun(Msg)

%% Order in page source
%% 0. threadID1404320
%% 1. threadID="1404320"
%% 2. <the thread title>
%% 3. <em>Page <strong>177</strong> of <strong>177</strong>

%% Find threadid "threadID1404320"
%% split html on "<div class=\"reply"
%% inside we should find "<div class=\"message-head", "profile.php?user", ">",
%% copy user name until we find next "<"
%% find "messageID=\""
%% copy msgid to next "\""
%% find "unixtime=\""
%% copy unixtime to next "\""
%% find "<div class=\"message-contents\"", ">"
%% copy message text unit next "</div>" BUT ignore "<b />"
