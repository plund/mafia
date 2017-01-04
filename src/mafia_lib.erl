-module(mafia_lib).

-export([rmess/1,
         rpage/1,
         rpage/2,
         rday/2,
         rgame/1,
         rmessI/1,

         thid/1,
         gamename_to_thid/1,
         dl2phase/1,
         phase_time2dl/2,

         merge_intervals/1,

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

thid(ThId) when is_integer(ThId) ->
    ThId;
thid(GN) when is_atom(GN) ->
    case gamename_to_thid(GN) of
        ThId when is_integer(ThId) ->
            ThId;
        ?undefined -> {?error, ?undefined}
    end.

gamename_to_thid(GN) when is_atom(GN) ->
    case ?getv(?reg_threads) of
        ?undefined -> ?undefined;
        Regs ->
            case lists:keyfind(GN, 1, Regs) of
                {_, ThId} ->
                    io:format("Translating ~p to ~p\n", [GN, ThId]),
                    ThId;
                false -> ?undefined
            end
    end.

%% -----------------------------------------------------------------------------

dl2phase({?game_ended, _Time}) -> ?game_ended;
dl2phase({Num, Don, _Time}) -> {Num, Don}.

phase_time2dl(?game_ended, Time) -> {?game_ended, Time};
phase_time2dl({Num, Don}, Time) -> {Num, Don, Time}.


-type interval() :: {term(), term()}.
-type intervals() :: [interval()].

%% -----------------------------------------------------------------------------
%% Merge overlapping intervals (treat Intervals as an unordered set)
%% Example:
%%    merge_intervals([{1,5},{3,7},{10,14},{1,9},{13,20}]) -> [{1,9},{10,20}])
%% -----------------------------------------------------------------------------
-spec merge_intervals(Intervals::intervals()) -> intervals().
merge_intervals(Intervals) ->
    lists:sort(merge_intervals2(Intervals)).

merge_intervals2([]) -> [];
merge_intervals2([H | T]) ->
    case find_overlaps(H, T) of
        [] ->
            [H | merge_intervals2(T)];
        OverlappingIntervals ->
            MergedInterval = merge_overlap([H | OverlappingIntervals]),
            Rest = T -- OverlappingIntervals,
            merge_intervals2([MergedInterval | Rest])
    end.

%% -----------------------------------------------------------------------------
%% Returns all intervals in Intervals that overlaps with First
%% -----------------------------------------------------------------------------
-spec find_overlaps(First::interval(), Intervals::intervals()) -> intervals().
find_overlaps(_, []) -> [];
find_overlaps(First, [HInt | Intervals]) ->
    IsNeig = is_overlap(First, HInt),
    if IsNeig -> [HInt | find_overlaps(First, Intervals)];
       not IsNeig -> find_overlaps(First, Intervals)
    end.

%% -----------------------------------------------------------------------------
%% Checks if interval A and B are overlapping
%% -----------------------------------------------------------------------------
-define(IS_IN(P, Lo, Hi), ((Lo =< P) andalso (P =< Hi)) ).
-spec is_overlap(A::interval(), B::interval()) -> boolean().
is_overlap({ALo, _AHi}, {BLo, BHi}) when ?IS_IN(ALo, BLo, BHi) -> true;
is_overlap({_ALo, AHi}, {BLo, BHi}) when ?IS_IN(AHi, BLo, BHi) -> true;
is_overlap({ALo, AHi}, {BLo, _BHi}) when ?IS_IN(BLo, ALo, AHi) -> true;
is_overlap({ALo, AHi}, {_BLo, BHi}) when ?IS_IN(BHi, ALo, AHi) -> true;
is_overlap(_, _) -> false.

%% -----------------------------------------------------------------------------
%% Returns one interval spanning over the overlapping intervals
%% -----------------------------------------------------------------------------
-spec merge_overlap(intervals()) -> interval().
merge_overlap(L) ->
    {Lows, Highs} = lists:unzip(L),
    {lists:min(Lows), lists:max(Highs)}.


%% -----------------------------------------------------------------------------

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
