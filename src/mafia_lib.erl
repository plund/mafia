-module(mafia_lib).

-export([rmess/1,
         rpage/1,
         rpage/2,
         rday/2,
         rgame/1,

         rmessI/1,
         dl2phase/1,
         all_msgids/1,
         all_msgids/2
        ]).

-include("mafia.hrl").


rmess(MsgId) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rmessI(MsgId) of
        [] -> [];
        [M] when M#message.time > OffsetNow -> [];
        [M] -> [M]
    end.

rmessI(MsgId) ->
    mnesia:dirty_read(message, MsgId).

%% -----------------------------------------------------------------------------

rpage(Key) ->
    case rpageI(Key) of
        [] -> [];
        [P] ->
            MsgIds2 = [MId || MId <- P#page_rec.message_ids,
                              [] /= ?rmess(MId) ],
            if MsgIds2 == [] -> [];
               true ->
                    [P#page_rec{message_ids = MsgIds2}]
            end
    end.

rpage(ThId, Page) -> rpage({ThId, Page}).

rpageI(Key) -> mnesia:dirty_read(page_rec, Key).

%% -----------------------------------------------------------------------------

rday(ThId, {DayNum, _}) ->
    rday(ThId, DayNum);
rday(ThId, DayNum) when is_integer(ThId) ->
    rday(?rgame(ThId), DayNum);
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

%% -----------------------------------------------------------------------------

rgame(ThId) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rgameI(ThId) of
        [] -> [];
        [G] ->
            %% - reading game need to filter out "future" deaths
            Deaths2 = [ D || D <- G#mafia_game.player_deaths,
                             D#death.time =< OffsetNow],
            %% - fix game_end if it is too early for it.
            GameEnd2 =
                case G#mafia_game.game_end of
                    {EndTime, _MsgId} = GE when EndTime =< OffsetNow ->
                        GE;
                    _ -> ?undefined
                end,
            G2 = G#mafia_game{player_deaths = Deaths2,
                              game_end = GameEnd2},
            [G2]
    end.

rgameI(ThId) ->
    mnesia:dirty_read(mafia_game, ThId).

%% -----------------------------------------------------------------------------

dl2phase({?game_ended, _Time}) -> ?game_ended;
dl2phase({Num, Don, _Time}) -> {Num, Don}.

%% -----------------------------------------------------------------------------

%% Get all msg ids on a page list.
all_msgids(ThId) ->
    AllPages = mafia:pages_for_thread(ThId),
    all_msgids(ThId, AllPages).

all_msgids(_ThId, []) -> [];
all_msgids(ThId, PageNums = [PageN|_]) ->
    all_msgids(ThId, PageNums, rpageI({ThId, PageN})).

all_msgids(ThId, PageNums, []) ->
    all_msgids(ThId, PageNums);
all_msgids(ThId, PageNums, [PR]) ->
    PR#page_rec.message_ids,
    all_msgids2(ThId, PageNums, PR#page_rec.message_ids).

all_msgids2(ThId, PageNums, []) ->
    all_msgids(ThId, tl(PageNums));
all_msgids2(ThId, PageNums, [MId|MIds]) ->
    all_msgids2(ThId, PageNums, [MId|MIds], ?rmess(MId)).

all_msgids2(_ThId, _PageNums, _MIds, []) -> [];
all_msgids2(ThId, PageNums = [P|_], [MId|MIds], _) ->
    [{P, MId} | all_msgids2(ThId, PageNums, MIds)].

