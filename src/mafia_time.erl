-module(mafia_time).

-export([get_next_deadline/1,
         get_next_deadline/2,
         hh_mm_to_deadline/2,
         utc_secs1970/0,
         get_tz_dst/0,
         get_tz_dst/2,
         secs1970_to_local_datetime/3,
         update_deadlines/1,
         initial_deadlines/1,
         calculate_phase/1,
         calculate_phase/2,
         nearest_deadline/1,
         nearest_deadline/2,
         timer_minutes/1,

         end_phase/2,
         end_game/1,
         unend_game/1,

         inc_phase/1,
         conv_gtime_secs1970/2,

         test/0
        ]).

-include("mafia.hrl").

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------

-spec hh_mm_to_deadline(G :: integer() | #mafia_game{},
                        Time :: seconds1970())
                       -> {Hours::integer(),
                           Minutes :: minute()}.
hh_mm_to_deadline(G, Time) ->
    {{Days, {HH, MM, _SS}}, _} = get_next_deadline(G, Time),
    {Days * 24 + HH, MM}.

%% -----------------------------------------------------------------------------

-spec get_next_deadline(ThId::integer())
                       -> {Remain :: {Days :: integer(), time()},
                           deadline()}.
get_next_deadline(ThId) when is_integer(ThId) ->
    NowSecs = utc_secs1970(),
    get_next_deadline(ThId, NowSecs).

get_next_deadline(ThId, Secs) when is_integer(ThId) ->
    get_next_deadline(?rgame(ThId), Secs);
get_next_deadline([], _Secs) -> false;
get_next_deadline([#mafia_game{} = G], Secs) ->
    get_next_deadline(G, Secs);
get_next_deadline(#mafia_game{game_end = {EndTime, _EndMsgId}},
                  Secs) when Secs > EndTime ->
    SecsOver = Secs - EndTime,
    {{SecsOver div ?DaySecs,
      calendar:seconds_to_time(SecsOver rem ?DaySecs)},
     ?game_ended};
get_next_deadline(#mafia_game{key = _ThId,
                              deadlines = DLs},
                  Secs) ->
    case lists:dropwhile(
           fun({_, _, DlSecs}) -> DlSecs =< Secs end,
           ?lrev(DLs)) of
        [DL = {_Num, _DoN, DlSecs}|_] ->
            SecsLeft = DlSecs - Secs,
            {{SecsLeft div ?DaySecs,
              calendar:seconds_to_time(SecsLeft rem ?DaySecs)},
             DL};
        [] -> % should get more DLs here
            %% update_deadlines(ThId),
            %% get_next_deadline(ThId)
            {{0, {0, 0, 0}}, ?game_ended}
    end.

%% -----------------------------------------------------------------------------

-spec get_tz_dst() -> {TZ :: integer(), IsDst :: boolean()}.
get_tz_dst() ->
    case ?getv(?print_time) of
        ?game -> {?getv(?timezone_game), ?getv(?dst_game)};
        ?user -> {?getv(?timezone_user), ?getv(?dst_user)};
        Loc when Loc == ?utc;
                 Loc == ?zulu;
                 Loc == ?gmt ->
            {0, ?false}
    end.

-spec get_tz_dst(#mafia_game{}, seconds1970())
                -> {TZ :: integer(), IsDst :: boolean()}.
get_tz_dst(G = #mafia_game{}, Time) ->
    TZ = G#mafia_game.time_zone,
    DateTime = secs1970_to_local_datetime(Time, TZ, ?false),
    IsDst = is_dst(DateTime, G),
    {TZ, IsDst}.

%% -----------------------------------------------------------------------------

adjust_secs1970_to_tz_dst(Time, TzH, Dst) ->
    Time + (TzH + if Dst -> 1; true -> 0 end) * ?HourSecs.

-spec calculate_phase(Game :: integer() | #mafia_game{},
                      Time :: seconds1970())
                     -> phase().
calculate_phase(ThId) ->
    Time = mafia_time:utc_secs1970(),
    calculate_phase(ThId, Time).

calculate_phase(ThId, Time) when is_integer(ThId) ->
    case ?rgame(ThId) of
        [G] -> calculate_phase(G, Time);
        [] -> false
    end;
calculate_phase(Game, Time) ->
    Game2 = if Game#mafia_game.game_end == ?undefined,
               Time > element(3, hd(Game#mafia_game.deadlines)) ->
                    Game1 = add_deadlines(Game),
                    mnesia:dirty_write(Game1),
                    Game1;
               true ->
                    Game
            end,
    DLs = Game2#mafia_game.deadlines,
    DLsAfterTime =
        lists:takewhile(fun({_, _, DLTime}) -> DLTime >= Time end, DLs),
    case DLsAfterTime of
        [] -> ?game_ended;
        _ ->
            {Num, DN,_} = lists:last(DLsAfterTime),
            {Num, DN}
    end.

%% and it should also update the game in mnesia with it.
update_deadlines(ThId) ->
    case ?rgame(ThId) of
        [] -> [];
        [#mafia_game{} = G] ->
            NewDLs = expand_deadlines(G),
            mnesia:dirty_write(G#mafia_game{deadlines = NewDLs}),
            NewDLs
    end.

initial_deadlines(MGame) ->
    add_deadlines(MGame#mafia_game{deadlines = []}).

add_deadlines(MGame) ->
    MGame#mafia_game{deadlines = expand_deadlines(MGame)}.

%% -----------------------------------------------------------------------------
%% @doc Return an expanded list of deadlines in reversed order
-spec expand_deadlines(ThId :: integer() | #mafia_game{})
                    -> NewDLs :: [deadline()].
expand_deadlines(ThId) when is_integer(ThId) ->
    case ?rgame(ThId) of
        [] -> ignore;
        [#mafia_game{} = G] ->
            expand_deadlines(G)
    end;
expand_deadlines(G) ->
    DLsIn = G#mafia_game.deadlines,
    FirstNewPh = if DLsIn == [] -> {1, ?day};
                    true -> inc_phase(hd(DLsIn))
                 end,
    TargetTime = utc_secs1970() + 11 * ?DaySecs,
    expand_deadlinesI(G, TargetTime, DLsIn, calc_one_deadlineI(FirstNewPh, G)).

expand_deadlinesI(_G, TargetTime, Dls, Dl = {_, _, Time})
  when Time > TargetTime ->
    [Dl|Dls];
expand_deadlinesI(G, TargetTime, Dls, Dl) ->
    Ph = inc_phase(Dl),
    expand_deadlinesI(G, TargetTime, [Dl|Dls], calc_one_deadlineI(Ph, G)).

-spec calc_one_deadlineI({Num :: integer(),
                     DayNight::day_night()},
                    Game :: #mafia_game{})
                   -> seconds1970().
calc_one_deadlineI({Num, DayNight}, Game)
  when DayNight == ?day;
       DayNight == ?night ->
    #mafia_game{
      day_hours = DayHours,
      night_hours = NightHours,
      time_zone = TZ,
      day1_dl_time = DeadD1LocalDateTime,
      is_init_dst = IsInitDst,
      dst_changes = DstChanges
     } = Game,

    %% know time and phase D1
    %% know where we want to go

    UtcGS = utc_gs(DeadD1LocalDateTime, TZ, IsInitDst),  %% D1 utc secs
    UtcGS2 = UtcGS + (Num-1) * (DayHours + NightHours) * ?HourSecs,
    UtcGS2b = UtcGS2 + if DayNight == ?night ->
                               NightHours * ?HourSecs;
                          true -> 0
                       end,
    UtcGS3 = dst_change_adapt(TZ,
                              IsInitDst,
                              DstChanges,
                              UtcGS2b), %% Target DL utc secs
    {Num, DayNight, UtcGS3 - ?GSECS_1970}.

%% -----------------------------------------------------------------------------

end_phase(M = #message{}, TimeNextDL) ->
    end_phase(M, TimeNextDL, ?rgame(M#message.thread_id)).

end_phase(_M, _TimeNextDL, []) -> ok;
end_phase(#message{time = MsgTime}, DateTime, [G]) ->
    %% Early test. Is MsgTime already present
    end_phase(#message{time = MsgTime}, DateTime, [G],
              lists:keyfind(MsgTime, 3, G#mafia_game.deadlines)).

end_phase(#message{time = MsgTime}, DateTime, [G], false) ->
    %% remove all DLs after msg time
    %% DstChanges = G#mafia_game.dst_changes,
    OrigDLs = G#mafia_game.deadlines,
    DLs2 = lists:dropwhile(fun({_, _, T}) -> T >= MsgTime end,
                           OrigDLs),
    %% Insert new DL at message time
    MsgPhase = {DNum, DoN} = inc_phase(hd(DLs2)),
    EarlyDL = {DNum, DoN, MsgTime},
    io:format("Early ~p\n", [EarlyDL]),
    DLs3 = [EarlyDL | DLs2],

    %% Add next DL for time given in message
    {NextDNum, NextDoN} = inc_phase(MsgPhase),
    NextTime = conv_gtime_secs1970(G, DateTime),
    NextDL = {NextDNum, NextDoN, NextTime},
    DLs4 = [NextDL | DLs3],

    %% Add more DLs at end.
    TargetTime = utc_secs1970() + 11 * ?DaySecs,
    NewDLs = get_some_extra_dls(G, DLs4, TargetTime),
    mnesia:dirty_write(G#mafia_game{deadlines = NewDLs}),
    mafia_web:regenerate_history(MsgTime, EarlyDL),
    mafia_web:update_current(G#mafia_game.key),
    inserted;
end_phase(_, _, _, _) ->
    already_inserted.

get_some_extra_dls(_G, DLs=[{_,_, Time} | _], Target) when Time > Target -> DLs;
get_some_extra_dls(G, DLs = [DL | _], Target) ->
    NewDL = inc_deadline(G, DL),
    get_some_extra_dls(G, [NewDL | DLs], Target).

%% -----------------------------------------------------------------------------

end_game(M) ->
    end_game(M, ?rgame(M#message.thread_id)).

end_game(_M, []) -> no_game;
end_game(M, [G]) ->
    end_game(M, G, lists:keyfind(end_game, 1, G#mafia_game.deadlines)).

end_game(M, G, false) ->
    EndTime = M#message.time,
    MsgId = M#message.msg_id,

    %% remove all DLs after EndTime
    DLs2 = lists:foldr(
             fun(DL = {_, _, DlTime}, Acc)
                   when DlTime < EndTime -> [DL | Acc];
                (_, Acc) -> Acc
             end,
             [],
             G#mafia_game.deadlines),

    %% Re-add current phase with end of game time
    {DNum, DoN} = inc_phase(hd(DLs2)),
    LastDL = {DNum, DoN, EndTime},
    DLs3 = [LastDL | DLs2],
    mnesia:dirty_write(G#mafia_game{deadlines = DLs3,
                                    game_end = {EndTime, MsgId}}),
    mafia_web:regenerate_history(EndTime, LastDL),
    mafia_web:update_current(G#mafia_game.key),
    ?game_ended;
end_game(_, _, _) ->
    already_game_ended.

%% -----------------------------------------------------------------------------

unend_game(M) ->
    unend_game(M, ?rgame(M#message.thread_id)).

unend_game(_M, []) -> no_game;
unend_game(M, [G]) ->
    MsgTime = M#message.time,
    unend_game2(G, lists:keyfind(MsgTime, 3, G#mafia_game.deadlines)).

unend_game2(G, EndDL) ->
    G2 = if EndDL == false ->
                 G;
            true ->
                 [EndDL|DLs] = G#mafia_game.deadlines,
                 TargetTime = utc_secs1970() + 11 * ?DaySecs,
                 NewDLs = get_some_extra_dls(G, DLs, TargetTime),
                 G#mafia_game{deadlines = NewDLs}
         end,
    mnesia:dirty_write(G2#mafia_game{game_end = ?undefined}),
    game_unended.

%% -----------------------------------------------------------------------------

-spec inc_phase(phase() | deadline()) -> phase().
inc_phase({Num, D, _Time}) -> inc_phase({Num, D});
inc_phase({Num, ?day}) when is_integer(Num) -> {Num, ?night};
inc_phase({Num, ?night}) when is_integer(Num) -> {Num + 1, ?day}.

inc_deadline(G, DL = {_DNum, _DoN, Time}) ->
    {NDNum, NDoN} = inc_phase(DL),
    NHours = case NDoN of
                 ?day   -> G#mafia_game.day_hours;
                 ?night -> G#mafia_game.night_hours
             end,
    %% If move from DST to normal +3600
    %% If move from normal to DST -3600
    DiffSecs = NHours * ?HourSecs,
    NTime = Time + DiffSecs,
    Adjust = case dst_change(Time,
                             NTime,
                             G#mafia_game.dst_changes,
                             G#mafia_game.time_zone) of
                 same -> 0;
                 to_normal -> ?HourSecs;
                 to_dst -> -?HourSecs
             end,
    {NDNum, NDoN, NTime + Adjust}.

-spec dst_change(Start :: seconds1970(),
                 End :: seconds1970(),
                 DstChanges :: [{datetime(), boolean()}],
                 TZ :: integer())
                -> same | to_normal | to_dst.
dst_change(Start, End, DstChanges, TZ) ->
    %% dst_changes = [{{{2016,11,6},{2,0,0}}, false},
    %%                {{{2017, 4,1},{2,0,0}}, true}],
    StartDT = secs1970_to_local_datetime(Start, TZ, false),
    EndDT   = secs1970_to_local_datetime(End,   TZ, false),
    do_dst_change(DstChanges, StartDT, EndDT).

do_dst_change([{DT, _IsDst} | _T], _StartDT, EndDT) when EndDT =< DT ->
    same;
do_dst_change([{DT, _IsDst} | T], StartDT, EndDT) when DT =< StartDT ->
    do_dst_change(T, StartDT, EndDT);
do_dst_change([{_DT, IsDst} | _DstChanges], _StartDT, _EndDT) ->
    if IsDst == false -> to_normal;
       IsDst == true  -> to_dst
    end;
do_dst_change([], _, _) -> same.

is_dst(DateTime, G) ->
    #mafia_game{
            is_init_dst = IsInitDst,
            dst_changes = DstChanges
           } = G,
    lists:foldl(fun({DT, Dst}, Acc) ->
                        if DT < DateTime -> Dst;
                           true  -> Acc
                        end
                end,
                IsInitDst,
                DstChanges).


dst_change_adapt(TZ, IsInitDst, DstChanges, UtcGS) ->
    %% dst_changes = [{{{2016,11,6},{2,0,0}},false},
    %%                {{{2017,4,1},{2,0,0}},true}],
    IsDstAtDL =
        lists:foldl(fun({SwitchDT, IsDstSw}, IsDstAcc) ->
                            SwiGS = utc_gs(SwitchDT, TZ, false),
                            if UtcGS > SwiGS -> IsDstSw;
                               true -> IsDstAcc
                            end
                    end,
                    IsInitDst,
                    DstChanges),
    case {IsInitDst, IsDstAtDL} of
        {true, false} ->
            UtcGS + ?HourSecs;
        {false, true} ->
            UtcGS - ?HourSecs;
        _ -> UtcGS
    end.

secs1970_to_local_datetime(Time, TzH, Dst) ->
    Time2 = adjust_secs1970_to_tz_dst(Time, TzH, Dst),
    {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(Time2),
    {Y, M, D} = calendar:gregorian_days_to_date(?GDAYS_1970 + Days1970),
    {{Y, M, D}, {HH,MM,SS}}.

conv_gtime_secs1970(G, DateTime) ->
    IsDst = is_dst(DateTime, G),
    TZ = G#mafia_game.time_zone,
    utc_gs(DateTime, TZ, IsDst) - ?GSECS_1970.

utc_gs(DateTime, TZ, Dst) ->
    calendar:datetime_to_gregorian_seconds(DateTime)
        - (TZ + if Dst -> 1; true -> 0 end) * ?HourSecs.

utc_secs1970() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time())
        - ?GSECS_1970.

-spec timer_minutes(ThId :: thread_id()) -> none | integer().
timer_minutes(ThId) ->
    %% ThId = ?getv(?thread_id),
    case nearest_deadline(ThId) of
        none -> none;
        {RelTimeSecs, ?game_ended} ->
            t_mins(?game_ended, RelTimeSecs);
        {RelTimeSecs, {_, DoN, _}} ->
            t_mins(DoN, RelTimeSecs)
    end.

-define(m2s(Min), (Min * ?MinuteSecs)).
%%   Day nearest
t_mins(?day, T) when T < ?m2s(-90) -> 10;
t_mins(?day, T) when T < ?m2s(-30) -> 5;
t_mins(?day, T) when T < ?m2s(-7) -> 2;
t_mins(?day, T) when T < ?m2s(7) -> 1;
t_mins(?day, T) when T < ?m2s(20) -> 2;
t_mins(?day, T) when T >= ?m2s(20) -> 10;
%%  Night nearest
t_mins(?night, T) when T < ?m2s(-20) -> 10;
t_mins(?night, T) when T < ?m2s(10) -> 2;
t_mins(?night, T) when T < ?m2s(90) -> 4;
t_mins(?night, T) when T >= ?m2s(90) -> 10;
%%  Game has ended
t_mins(?game_ended, T) when T < ?m2s(60) -> 10;
t_mins(?game_ended, T) when T < ?m2s(180) -> 20;
t_mins(?game_ended, T) when T < ?m2s(24*60) -> 60;
t_mins(?game_ended, T) when T >= ?m2s(24*60) -> 120.

-spec nearest_deadline(integer() | #mafia_game{})
                      -> none | {integer(), deadline()}.
nearest_deadline(GameKey) when is_integer(GameKey) ->
    nearest_deadline(?rgame(GameKey));
nearest_deadline([]) -> none;
nearest_deadline([G]) ->
    Now = utc_secs1970(),
    case G#mafia_game.game_end of
        ?undefined ->
            nearest_deadline(G, Now);
        {EoGTime, _MsgId} ->
            {Now - EoGTime, ?game_ended}
    end.

-spec nearest_deadline(integer() | #mafia_game{},
                       seconds1970())
                      -> none | {integer(), deadline()}.
nearest_deadline(ThId, Time) when is_integer(ThId) ->
    nearest_deadline(?rgame(ThId), Time);
nearest_deadline([], _) -> none;
nearest_deadline([G = #mafia_game{}], Time) ->
    nearest_deadline(G, Time);
nearest_deadline(G = #mafia_game{}, Time) ->
    DLs = G#mafia_game.deadlines,
    {_, TDiff, NearestDL} =
        hd(lists:sort([{abs(Time - DlTime), Time - DlTime, DL}
                       || DL = {_, _, DlTime} <- DLs])),
    {TDiff, NearestDL}.

%% -------------------------------------------------

test() ->
    eunit:test(?MODULE).

-define(D1, {{2016,12,21},{18,0,0}}).
-define(D2, {{2016,12,22},{18,0,0}}).

-define(DST0, {{{2016,11,22},{2,0,0}}, false}).
-define(DST1, {{{2016,11,30},{2,0,0}}, false}).
-define(DST2, {{{2016,12,22},{2,0,0}}, true}).
-define(DST3, {{{2016,12,22},{2,0,0}}, false}).
-define(DST4, {{{2016,12,25},{2,0,0}}, true}).
-define(DST5, {{{2016,12,28},{2,0,0}}, false}).

do_dst_change_test_() ->
    [?_assert(same == do_dst_change([?DST0, ?DST1], ?D1, ?D2)),
     ?_assert(to_dst == do_dst_change([?DST1, ?DST2], ?D1, ?D2)),
     ?_assert(to_normal == do_dst_change([?DST1, ?DST3], ?D1, ?D2)),
     ?_assert(same == do_dst_change([?DST1, ?DST5], ?D1, ?D2)),
     ?_assert(same == do_dst_change([?DST4, ?DST5], ?D1, ?D2)),
     ?_assert(to_normal == do_dst_change([?DST3], ?D1, ?D2)),
     ?_assert(same == do_dst_change([], ?D1, ?D2))
    ].

-define(Time1, 1481655677). % ca 2000 CET 161213
-define(Time0, ?Time1 + ?DaySecs). % ca 2000 CET 161212
-define(Time2, ?Time1 + ?DaySecs). % ca 2000 CET 161214
-define(Time3, ?Time1 + 2*?DaySecs). % ca 2000 CET 161215
-define(DST6, {{{2016,12,14},{2,0,0}}, true}).
-define(DST7, {{{2016,12,14},{2,0,0}}, false}).

dst_change_test_() ->
    [
     ?_assert(same == dst_change(?Time0, ?Time1, [?DST6], -5)),
     ?_assert(to_dst == dst_change(?Time1, ?Time2, [?DST6], -5)),
     ?_assert(to_normal == dst_change(?Time1, ?Time2, [?DST7], -5)),
     ?_assert(same == dst_change(?Time2, ?Time3, [?DST7], -5))
    ].
