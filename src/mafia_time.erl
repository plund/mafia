-module(mafia_time).

-include("mafia.hrl").

-export([get_next_deadline/1,
         get_next_deadline/2,
         hh_mm_to_deadline/2,
         utc_secs1970/0,
         get_tz_dst/0,
         local_datetime_for_secs1970/3,
         update_deadlines/1,
         add_deadlines/1,
         calculate_phase/2,
         timer_minutes/0
        ]).

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

utc_secs1970() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time())
        - ?GSECS_1970.

get_tz_dst() ->
    case getv(print_time) of
        game -> {getv(timezone_game), getv(dst_game)};
        user -> {getv(timezone_user), getv(dst_user)};
        Loc when Loc == utc;
                 Loc == zulu;
                 Loc == gmt ->
            {0, false}
    end.

local_datetime_for_secs1970(Time, TzH, Dst) ->
    Time2 = adjust_secs1970_to_tz_dst(Time, TzH, Dst),
    {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(Time2),
    {Y, M, D} = calendar:gregorian_days_to_date(?GDAYS_1970 + Days1970),
    {{Y, M, D}, {HH,MM,SS}}.

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
    MGame#mafia_game{
      deadlines = calc_deadlines(MGame)
     }.

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

-spec inc_phase(phase() | deadline()) -> phase().
inc_phase({Num, D, _Time}) -> inc_phase({Num, D});
inc_phase({Num, ?day}) when is_integer(Num) -> {Num, ?night};
inc_phase({Num, ?night}) when is_integer(Num) -> {Num + 1, ?day}.

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
    UtcGS = utc_gs(DeadD1LocalDateTime, TZ, IsInitDst),
    UtcGS2 = UtcGS + (Num-1) * (DayHours + NightHours) * ?HourSecs,
    UtcGS2b = UtcGS2 + if DayNight == ?night -> NightHours * ?HourSecs;
                          true -> 0
                       end,
    UtcGS3 = dst_change_adapt(TZ, IsInitDst, DstChanges, UtcGS2b),
    UtcDLTime = calendar:gregorian_seconds_to_datetime(UtcGS3),
    io:format("Calculated UTC DL for ~p ~p to be ~p\n",
              [DayNight, Num, UtcDLTime]),
    {Num, DayNight, UtcGS3 - ?GSECS_1970}.

dst_change_adapt(TZ, IsInitDst, DstChanges, UtcGS) ->
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

utc_gs(DateTime, TZ, Dst) ->
    calendar:datetime_to_gregorian_seconds(DateTime)
        - (TZ + if Dst -> 1; true -> 0 end) * ?HourSecs.


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
nearest_deadline(ThId) when is_integer(ThId) ->
    nearest_deadline(mafia:rgame(ThId));
nearest_deadline([]) -> none;
nearest_deadline([G = #mafia_game{}]) ->
    nearest_deadline(G);
nearest_deadline(G = #mafia_game{}) ->
    DLs = G#mafia_game.deadlines,
    Now = utc_secs1970(),
    {_, TDiff, NearestDL} =
        hd(lists:sort([{abs(Now - Time), Now - Time, DL}
                       || DL = {_, _, Time} <- DLs])),
    {TDiff, NearestDL}.
