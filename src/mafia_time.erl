-module(mafia_time).

-include("mafia.hrl").

-export([get_tz_dst/0,
         local_datetime_for_secs1970/3,
         add_deadline/2,
         calculate_phase/2]).

-import(mafia, [getv/1]).

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

-spec calculate_phase(Game :: #mafia_game{}, Time::seconds1970()) -> phase().
calculate_phase(Game, Time) ->
    if Time > element(3, hd(Game#mafia_game.deadlines)) ->
            Game2 = add_deadline(Game, 10),
            mnesia:write(Game2);
       true ->
            Game2 = Game
    end,
    DLs = Game2#mafia_game.deadlines,
    {Num,DN,_} =
        lists:last(
          lists:takewhile(
            fun({_,_,DLTime}) -> DLTime > Time end,
            DLs)),
    {Num,DN}.

add_deadline(MGame, NumNewDLs) ->
    MGame#mafia_game{
      deadlines = calc_deadlines(MGame, NumNewDLs)
     }.

calc_deadlines(ThId, NumNewDLs) when is_integer(ThId) ->
    case mnesia:dirty_read(mafia_game, ThId) of
        [] -> ignore;
        [#mafia_game{} = G] ->
            calc_deadlines(G, NumNewDLs)
    end;
calc_deadlines(G, NumNewDLs) ->
    DLsIn = G#mafia_game.deadlines,
    First = if DLsIn == [] -> {1, ?day};
               true -> inc_phase(hd(DLsIn))
            end,
    Phases = lists:foldl(fun(_, Acc=[DL|_]) ->
                                 [inc_phase(DL) | Acc]
                         end,
                         [First],
                         lists:seq(2, NumNewDLs)),
    lists:foldr(fun(Ph, DLs) ->
                        [calc_deadline(Ph, G) | DLs]
                end,
                DLsIn,
                Phases).

-spec inc_phase(phase() | deadline()) -> phase().
inc_phase({Num, D, _Time}) -> inc_phase({Num, D});
inc_phase({Num, ?day}) when is_integer(Num) -> {Num, ?night};
inc_phase({Num, ?night}) when is_integer(Num) -> {Num + 1, ?day}.

-spec calc_deadline({Num :: integer(),
                     DayNight::day_night()},
                    Game :: #mafia_game{})
                   -> seconds1970().
calc_deadline({Num, DayNight}, Game)
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
