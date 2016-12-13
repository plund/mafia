-module(mafia_time).

-export([get_next_deadline/1,
         get_next_deadline/2,
         hh_mm_to_deadline/2,
         utc_secs1970/0,
         get_tz_dst/0,
         secs1970_to_local_datetime/3,
         update_deadlines/1,
         add_deadlines/1,
         calculate_phase/2,
         nearest_deadline/1,
         nearest_deadline/2,
         timer_minutes/0,

         end_phase/2,

         inc_phase/1,
         conv_gtime_secs1970/2,

         test/0
        ]).

-include("mafia.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(mafia, [getv/1, lrev/1, rgame/1]).

-spec get_next_deadline(ThId::integer())
                       -> {Remain :: {Days :: integer(), time()},
                           deadline()}.
get_next_deadline(ThId) when is_integer(ThId) ->
    NowSecs = utc_secs1970(),
    get_next_deadline(ThId, NowSecs).

get_next_deadline(ThId, Secs) when is_integer(ThId) ->
    get_next_deadline(rgame(ThId), Secs);
get_next_deadline([], _Secs) -> false;
get_next_deadline([#mafia_game{} = G], Secs) ->
    get_next_deadline(G, Secs);
get_next_deadline(#mafia_game{key = ThId,
                              deadlines = DLs},
                  Secs) ->
    case lists:dropwhile(
           fun({_, _, DlSecs}) -> DlSecs =< Secs end,
           lrev(DLs)) of
        [DL = {_Num, _DoN, DlSecs}|_] ->
            SecsLeft = DlSecs - Secs,
            {{SecsLeft div ?DaySecs,
              calendar:seconds_to_time(SecsLeft rem ?DaySecs)},
             DL};
        [] -> % should get more DLs here
            update_deadlines(ThId),
            get_next_deadline(ThId)
    end.

hh_mm_to_deadline(G, Time) ->
    {{Days, {HH, MM, _SS}}, _} = get_next_deadline(G, Time),
    {Days * 24 + HH, MM}.

get_tz_dst() ->
    case getv(print_time) of
        game -> {getv(timezone_game), getv(dst_game)};
        user -> {getv(timezone_user), getv(dst_user)};
        Loc when Loc == utc;
                 Loc == zulu;
                 Loc == gmt ->
            {0, false}
    end.

adjust_secs1970_to_tz_dst(Time, TzH, Dst) ->
    Time + (TzH + if Dst -> 1; true -> 0 end) * ?HourSecs.

-spec calculate_phase(Game :: integer() | #mafia_game{},
                      Time :: seconds1970())
                     -> phase().
calculate_phase(ThId, Time) when is_integer(ThId) ->
    case rgame(ThId) of
        [G] -> calculate_phase(G, Time);
        [] -> false
    end;
calculate_phase(Game, Time) ->
    if Time > element(3, hd(Game#mafia_game.deadlines)) ->
            Game2 = add_deadlines(Game),
            mnesia:dirty_write(Game2);
       true ->
            Game2 = Game
    end,
    DLs = Game2#mafia_game.deadlines,
    {Num, DN,_} =
        lists:last(
          lists:takewhile(
            fun({_,_,DLTime}) -> DLTime > Time end,
            DLs)),
    {Num, DN}.

%% and it should also update the game in mnesia with it.
update_deadlines(ThId) ->
    case rgame(ThId) of
        [] -> [];
        [#mafia_game{} = G] ->
            NewDLs = calc_deadlines(G),
            mnesia:dirty_write(G#mafia_game{deadlines = NewDLs}),
            NewDLs
    end.

add_deadlines(MGame) ->
    MGame#mafia_game{deadlines = calc_deadlines(MGame)}.

%% -----------------------------------------------------------------------------
%% @doc Return an expanded list of deadlines in reversed order
-spec calc_deadlines(ThId :: integer() | #mafia_game{})
                    -> NewDLs :: [deadline()].
calc_deadlines(ThId) when is_integer(ThId) ->
    case rgame(ThId) of
        [] -> ignore;
        [#mafia_game{} = G] ->
            calc_deadlines(G)
    end;
calc_deadlines(G) ->
    DLsIn = G#mafia_game.deadlines,
    FirstNewPh = if DLsIn == [] -> {1, ?day};
                    true -> inc_phase(hd(DLsIn))
                 end,
    TargetTime = utc_secs1970() + 7 * ?DaySecs,
    calc_deadlinesI(G, TargetTime, [], calc_one_deadlineI(FirstNewPh, G)).

calc_deadlinesI(_G, TargetTime, Dls, Dl = {_, _, Time})
  when Time > TargetTime ->
    [Dl|Dls];
calc_deadlinesI(G, TargetTime, Dls, Dl) ->
    Ph = inc_phase(Dl),
    calc_deadlinesI(G, TargetTime, [Dl|Dls], calc_one_deadlineI(Ph, G)).

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
    _UtcDLTime = calendar:gregorian_seconds_to_datetime(UtcGS3),
    io:format("Calculated UTC DL for ~p ~p to be ~p\n",
              [DayNight, Num, _UtcDLTime]),
    {Num, DayNight, UtcGS3 - ?GSECS_1970}.

%% -----------------------------------------------------------------------------

end_phase(MsgId, TimeNextDL) ->
    case mnesia:dirty_read(message, MsgId) of
        [] -> ok;
        [M] -> end_phase2(M, TimeNextDL, rgame(M#message.thread_id))
    end.

end_phase2(_M, _TimeNextDL, []) -> ok;
end_phase2(#message{time = MsgTime}, DateTime, [G]) ->
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
    io:format("NewDLs ~p\n", [NewDLs]),
    mnesia:dirty_write(G#mafia_game{deadlines = NewDLs}),
    ok.

get_some_extra_dls(_G, DLs=[{_,_, Time} | _], Target) when Time > Target -> DLs;
get_some_extra_dls(G, DLs = [DL | _], Target) ->
    NewDL = inc_deadline(G, DL),
    get_some_extra_dls(G, [NewDL | DLs], Target).


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


-spec timer_minutes() -> none | integer().
timer_minutes() ->
    ThId = getv(thread_id),
    case nearest_deadline(ThId) of
        none -> none;
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
t_mins(?night, T) when T < ?m2s(-4) -> 10;
t_mins(?night, T) when T < ?m2s(10) -> 2;
t_mins(?night, T) when T < ?m2s(80) -> 4;
t_mins(?night, T) when T >= ?m2s(80) -> 10.

-spec nearest_deadline(integer() | #mafia_game{})
                      -> none | {integer(), deadline()}.
nearest_deadline(G) ->
    Now = utc_secs1970(),
    nearest_deadline(G, Now).

-spec nearest_deadline(integer() | #mafia_game{},
                       seconds1970())
                      -> none | {integer(), deadline()}.
nearest_deadline(ThId, Time) when is_integer(ThId) ->
    nearest_deadline(mafia:rgame(ThId), Time);
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
