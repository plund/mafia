-module(mafia_time).

-export([utc_secs1970/0,
         utc_day1970/0,
         utc_day2date/1,
         date2utc_day/1,

         show_time_offset/0,
         set_time_offset/1,
         conv_gtime_secs1970/2,

         get_tz_dst/0,
         get_tz_dst/2,
         secs1970_to_local_datetime/3,
         secs2day_hour_min_sec/1,

         initial_deadlines/1,
         update_deadlines/1,
         inc_phase/1,
         decr_phase/1,

         hh_mm_to_deadline/2,  %% These are similar
         get_next_deadline/1,
         get_next_deadline/2,
         next_deadlines/3,
         calculate_phase/1,
         calculate_phase/2,
         get_nxt_deadline/1,
         get_nxt_deadline/2,
         get_prev_deadline/2,
         get_time_for_phase/2,
         get_time_for_prev_phase/2,
         nearest_deadline/1,
         nearest_deadline/2,

         timer_minutes/1,

         end_phase/3,
         unend_phase/2,
         move_next_deadline/3,
         move_next_deadline/4,
         end_game/2,
         unend_game/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------

utc_secs1970() ->
    utc_secs1970I() - get_time_offset().

utc_secs1970I() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time())
        - ?GSECS_1970.

%% Return the current number of days since Jan 1 1970
-spec utc_day1970() -> Day :: integer().
utc_day1970() ->
    (calendar:datetime_to_gregorian_seconds(
      calendar:universal_time())
        - ?GSECS_1970) div ?DaySecs.

-spec utc_day2date(Day :: integer()) -> date().
utc_day2date(DayNum) ->
    element(1, calendar:gregorian_seconds_to_datetime(
                 DayNum * ?DaySecs + ?GSECS_1970)).

-spec date2utc_day(Date :: date()) -> Day :: integer().
date2utc_day(Date) ->
    (calendar:datetime_to_gregorian_seconds({Date, {0,0,0}})
     - ?GSECS_1970) div ?DaySecs.

%% -----------------------------------------------------------------------------

show_time_offset() ->
    {Days, {H, M , S}} = calendar:seconds_to_daystime(get_time_offset()),
    io:format("Time offset: ~p days, ~p hours, ~p mins, ~p secs\n",
              [Days, H, M , S]).

get_time_offset() ->
    case ?getv(?time_offset) of
        ?undefined -> 0;
        OS when is_integer(OS) -> OS
    end.

set_time_offset(Offset) ->
    Off = set_time_offsetI(Offset),
    ?set(?time_offset, Off),
    Off.

set_time_offsetI(Offset) when is_integer(Offset) -> Offset;
set_time_offsetI({msg_id, MsgId}) when is_integer(MsgId) ->
    case mafia_lib:rmessI(MsgId) of
        [] -> 0;
        [M] -> utc_secs1970I() - M#message.time
    end;
set_time_offsetI({days_hours, NumDays, NumHours})
  when is_integer(NumDays), is_integer(NumHours) ->
    (NumDays * 24 + NumHours) * ?HourSecs.

conv_gtime_secs1970(G, DateTime) ->
    IsDst = is_dst(DateTime, G),
    TZ = G#mafia_game.time_zone,
    utc_gs(DateTime, TZ, IsDst) - ?GSECS_1970.

utc_gs(DateTime, TZ, Dst) ->
    calendar:datetime_to_gregorian_seconds(DateTime)
        - (TZ + if Dst -> 1; true -> 0 end) * ?HourSecs.

%% -----------------------------------------------------------------------------

-spec get_tz_dst() -> {TZ :: integer(), IsDst :: boolean()}.
get_tz_dst() ->
    case ?getv(?console_tz) of
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

secs1970_to_local_datetime(Time, TzH, Dst) ->
    Time2 = adjust_secs1970_to_tz_dst(Time, TzH, Dst),
    {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(Time2),
    {Y, M, D} = calendar:gregorian_days_to_date(?GDAYS_1970 + Days1970),
    {{Y, M, D}, {HH,MM,SS}}.

adjust_secs1970_to_tz_dst(Time, TzH, Dst) ->
    Time + (TzH + if Dst -> 1; true -> 0 end) * ?HourSecs.

secs2day_hour_min_sec(Secs) when is_integer(Secs), Secs < 0 ->
    secs2day_hour_min_sec(-Secs);
secs2day_hour_min_sec(Secs) when is_integer(Secs), Secs >=0 ->
    {Secs div ?DaySecs,
     calendar:seconds_to_time(Secs rem ?DaySecs)}.

%% -----------------------------------------------------------------------------

initial_deadlines(MGame) ->
    add_deadlines(MGame#mafia_game{deadlines = []}).

%% @doc update the game in mnesia
update_deadlines(ThId) ->
    case ?rgame(ThId) of
        [] -> [];
        [#mafia_game{} = G] ->
            NewDLs = expand_deadlines(G),
            ?dwrite_game(G#mafia_game{deadlines = NewDLs}),
            NewDLs
    end.

add_deadlines(MGame) ->
    MGame#mafia_game{deadlines = expand_deadlines(MGame)}.

%% @doc Return an expanded list of deadlines in reversed order
-spec expand_deadlines(#mafia_game{}) -> NewDLs :: [#dl{}].
expand_deadlines(G) ->
    DLsIn = G#mafia_game.deadlines,
    FirstNewPh = if DLsIn == [] -> #phase{num = 1, don = ?day};
                    true -> inc_phase(hd(DLsIn))
                 end,
    TargetTime = utc_secs1970() + 11 * ?DaySecs,
    expand_deadlinesI(G, TargetTime, DLsIn, calc_one_deadlineI(FirstNewPh, G)).

expand_deadlinesI(_G, TargetTime, Dls, Dl = #dl{time = Time})
  when Time > TargetTime ->
    [Dl|Dls];
expand_deadlinesI(G, TargetTime, Dls, Dl) ->
    Ph = inc_phase(Dl),
    expand_deadlinesI(G, TargetTime, [Dl|Dls], calc_one_deadlineI(Ph, G)).

-spec calc_one_deadlineI(#phase{},
                         Game :: #mafia_game{})
                        -> #dl{}.
calc_one_deadlineI(Phase = #phase{num = Num, don = DayNight}, Game)
  when DayNight == ?day;
       DayNight == ?night ->
    #mafia_game{
      day_hours = DayHours,
      night_hours = NightHours,
      time_zone = TZ,
      %% day1_dl_time = DeadD1LocalDateTime,
      is_init_dst = IsInitDst,
      dst_changes = DstChanges
     } = Game,

    %% know time and phase D1
    %% know where we want to go

    %% UtcGS = utc_gs(DeadD1LocalDateTime, TZ, IsInitDst),  %% D1 utc secs
    UtcGS = calc_game_start_time_greg_secs(Game),

    UtcGS2 = UtcGS + (Num - 1) * (DayHours + NightHours) * ?HourSecs,
    UtcGS2b = UtcGS2 + if DayNight == ?night ->
                               NightHours * ?HourSecs;
                          true -> 0
                       end,
    UtcGS3 = dst_change_adapt(TZ,
                              IsInitDst,
                              DstChanges,
                              UtcGS2b), %% Target DL utc secs
    #dl{phase = Phase, time = UtcGS3 - ?GSECS_1970}.

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

%% gregorian_seconds().
calc_game_start_time_greg_secs(G) ->
    #mafia_game{time_zone = TZ,
                day1_dl_time = DeadD1LocalDateTime,
                is_init_dst = IsInitDst
               } = G,
    utc_gs(DeadD1LocalDateTime, TZ, IsInitDst).

%% -----------------------------------------------------------------------------

-spec inc_phase(#phase{} | #dl{}) -> #phase{}.
inc_phase(#dl{phase = Phase}) -> inc_phase(Phase);
inc_phase(Ph = #phase{don = ?day}) ->
    Ph#phase{don = ?night};
inc_phase(#phase{num = Num, don = ?night}) ->
    #phase{num = Num + 1, don = ?day}.

-spec decr_phase(#phase{} | #dl{}) -> #phase{}.
decr_phase(#dl{phase = Phase}) -> decr_phase(Phase);
decr_phase(#phase{num = Num, don = ?day}) ->
    #phase{num = Num - 1, don = ?night};
decr_phase(Ph = #phase{don = ?night}) ->
    Ph#phase{don = ?day}.

-spec inc_deadline(#mafia_game{}, #dl{}) -> #dl{}.
inc_deadline(G, DL = #dl{time = Time}) ->
    NextPhase = inc_phase(DL#dl.phase),
    NHours = case NextPhase#phase.don of
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
    #dl{phase = NextPhase, time = NTime + Adjust}.


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

%% -----------------------------------------------------------------------------
%% Section of similar functions that need refactoring
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
                           ?game_ended | #dl{}}.
get_next_deadline(ThId) when is_integer(ThId) ->
    NowTime = utc_secs1970(),
    get_next_deadline(ThId, NowTime).

get_next_deadline(ThId, Time) when is_integer(ThId); is_atom(ThId) ->
    get_next_deadline(?rgame(ThId), Time);
get_next_deadline([], _Time) -> false;
get_next_deadline([#mafia_game{} = G], Time) ->
    get_next_deadline(G, Time);
get_next_deadline(#mafia_game{game_end = {EndTime, _EndMsgId}},
                  Time) when Time > EndTime ->
    TimeOver = Time - EndTime,
    {{TimeOver div ?DaySecs,
      calendar:seconds_to_time(TimeOver rem ?DaySecs)},
     ?game_ended};
get_next_deadline(G = #mafia_game{}, Time) ->
    get_next_deadline(G, Time, ?sorted).

get_next_deadline(G = #mafia_game{}, Time, Mode)
  when Mode == ?sorted; Mode == ?seconds ->
    case next_deadlines(G, Time, 1) of
        [DL = #dl{time = DlTime}] ->
            TimeLeft = DlTime - Time,
            return_time_left(TimeLeft, DL, Mode);
        [] ->
            return_time_left(0, ?game_ended, Mode)
    end.

next_deadlines(#mafia_game{deadlines = DLs}, Time, Num) ->
    RevDLs = ?lrev(DLs),
    NextDLs = lists:dropwhile(fun(#dl{time = DlTime}) -> DlTime =< Time end,
                              RevDLs),
    if Num < length(NextDLs) ->
            string:left(NextDLs, Num);
       true ->
            if length(RevDLs) > Num ->
                    string:right(RevDLs, Num);
               true -> RevDLs
            end
    end.

return_time_left(TimeLeft, DL, ?sorted) ->
    {secs2day_hour_min_sec(TimeLeft), DL};
return_time_left(TimeLeft, DL, ?seconds) ->
    {TimeLeft, DL}.

%% -----------------------------------------------------------------------------

-spec calculate_phase(Game :: integer() | #mafia_game{})
                     -> #phase{}.
calculate_phase(ThId) ->
    Time = utc_secs1970(),
    calculate_phase(ThId, Time).

-spec calculate_phase(Game :: integer() | #mafia_game{},
                      Time :: seconds1970())
                     -> false | #phase{}.
calculate_phase(ThId, Time) when is_integer(ThId); is_atom(ThId) ->
    case ?rgame(ThId) of
        [G] -> calculate_phase(G, Time);
        [] -> false
    end;
calculate_phase(#mafia_game{game_end = {EndTime, _}}, Time)
  when Time >= EndTime ->
    #phase{don = ?game_ended};
calculate_phase(Game, Time) ->
    DL = get_nxt_deadline(Game, Time),
    DL#dl.phase.

-spec get_nxt_deadline(#mafia_game{}) -> #dl{}.
get_nxt_deadline(Game) ->
    get_nxt_deadline(Game, utc_secs1970()).

%% @doc Get next deadline *after* Time
%% @end
-spec get_nxt_deadline(#mafia_game{}, seconds1970()) -> #dl{}.
get_nxt_deadline(Game = #mafia_game{}, Time) ->
    GetNext =
        fun({Take, _Drop}) ->
                case Take of
                    [] -> #dl{phase = #phase{don = ?game_ended}};
                    _ ->
                        lists:last(Take)
                end
        end,
    get_a_deadline(Game, Time, GetNext).

-spec get_prev_deadline(#mafia_game{}, seconds1970()) -> #dl{} | ?undefined.
get_prev_deadline(Game = #mafia_game{}, Time) when is_integer(Time) ->
    GetPrev =
        fun({_Take, Drop}) ->
                case Drop of [DL|_] -> DL; _ -> ?undefined
                end
        end,
    get_a_deadline(Game, Time, GetPrev).

get_a_deadline(G = #mafia_game{game_end = {_, _}}, Time, Fun) ->
    get_a_deadline2(G, Time, Fun);
get_a_deadline(Game, Time, Fun)
  when Time >= (hd(Game#mafia_game.deadlines))#dl.time ->
    Game2 = add_deadlines(Game),
    ?dwrite_game(Game2),
    get_a_deadline2(Game2, Time, Fun);
get_a_deadline(Game, Time, Fun) ->
    get_a_deadline2(Game, Time, Fun).

get_a_deadline2(Game, Time, Fun) ->
    Fun(split_dls(Game, Time)).

%% -> {TakeWhile, DropWhile}
split_dls(Game, Time) ->
    lists:splitwith(
      fun(#dl{time = DLTime}) -> DLTime > Time end,
      Game#mafia_game.deadlines).

get_time_for_phase(G, Phase) when is_integer(G); is_atom(G) ->
    get_time_for_phase(?rgame(G), Phase);
get_time_for_phase([G], Phase) -> get_time_for_phase(G, Phase);
get_time_for_phase(G = #mafia_game{}, #phase{don = ?game_ended}) ->
    case G#mafia_game.game_end of
        ?undefined -> ?undefined;
        {Time, _MsgId} -> Time
    end;
get_time_for_phase(G = #mafia_game{}, Phase) ->
    case lists:dropwhile(
           fun(#dl{phase = Ph}) -> Ph /= Phase end,
           G#mafia_game.deadlines) of
        [] -> ?undefined;
        [#dl{time = Time} | _] -> Time
    end.

%% seconds_1970().
get_time_for_prev_phase(G, Phase) ->
    PrevPhase = decr_phase(Phase),
    case get_time_for_phase(G, PrevPhase) of
        ?undefined ->
            calc_game_start_time_greg_secs(G)
                - ?GSECS_1970
                - G#mafia_game.day_hours * ?DaySecs;
        Time -> Time
    end.

-spec nearest_deadline(integer() | #mafia_game{} | [#mafia_game{}])
                      -> {integer(), #dl{} | ?game_ended} | none.
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

-spec nearest_deadline(integer() | #mafia_game{} | [#mafia_game{}],
                       seconds1970())
                      -> {integer(), #dl{}} | none.
nearest_deadline(ThId, Time) when is_integer(ThId) ->
    nearest_deadline(?rgame(ThId), Time);
nearest_deadline([], _) -> none;
nearest_deadline([G = #mafia_game{}], Time) ->
    nearest_deadline(G, Time);
nearest_deadline(G = #mafia_game{}, Time) ->
    DLs = G#mafia_game.deadlines,
    {_, TDiff, NearestDL} =
        hd(lists:sort([{abs(Time - DlTime), Time - DlTime, DL}
                       || DL = #dl{time = DlTime} <- DLs])),
    {TDiff, NearestDL}.

%% -----------------------------------------------------------------------------

-spec timer_minutes(ThId :: thread_id()) -> none | integer().
timer_minutes(ThId) ->
    case ?getv(?timer_minutes) of
        Mins when is_integer(Mins), Mins > 0 ->
            Mins;
        _ ->
            case nearest_deadline(ThId) of
                none -> none;
                {RelTimeSecs, ?game_ended} ->
                    t_mins(?game_ended, RelTimeSecs);
                {RelTimeSecs, #dl{phase = #phase{don = DoN}}} ->
                    t_mins(DoN, RelTimeSecs)
            end
    end.

-define(m2s(Min), (Min * ?MinuteSecs)).
%%   Day nearest
t_mins(?day, T) when T < ?m2s(-120) -> 6;
t_mins(?day, T) when T < ?m2s(-30) -> 3;
t_mins(?day, T) when T < ?m2s(-7) -> 2;
t_mins(?day, T) when T < ?m2s(7) -> 1;
t_mins(?day, T) when T < ?m2s(20) -> 2;
t_mins(?day, T) when T < ?m2s(60) -> 3;
t_mins(?day, T) when T >= ?m2s(60) -> 6;
%%  Night nearest
t_mins(?night, T) when T < ?m2s(-40) -> 6;
t_mins(?night, T) when T < ?m2s(-10) -> 3;
t_mins(?night, T) when T < ?m2s(20) -> 2;
t_mins(?night, T) when T < ?m2s(90) -> 4;
t_mins(?night, T) when T >= ?m2s(90) -> 6;
%%  Game has ended
t_mins(?game_ended, T) when T < ?m2s(60) -> 5;
t_mins(?game_ended, T) when T < ?m2s(180) -> 10;
t_mins(?game_ended, T) when T < ?m2s(360) -> 20;
t_mins(?game_ended, T) when T < ?m2s(24*60) -> 60;
t_mins(?game_ended, T) when T >= ?m2s(24*60) -> 120.

%% -----------------------------------------------------------------------------
%% End refactoring section
%% -----------------------------------------------------------------------------

end_phase([], _Phase, _Time) -> [];
end_phase([G], Phase, Time) -> end_phase(G, Phase, Time);
end_phase(G = #mafia_game{}, Phase = #phase{}, Time) ->
    end_phase(G, Phase, Time, lists:keyfind(Time, #dl.time,
                                            G#mafia_game.deadlines)).

end_phase(G, Phase = #phase{}, Time, false) ->
    NewDLs = change_dl_time(G#mafia_game.deadlines, Phase, Time),
    G2 = G#mafia_game{deadlines = NewDLs},
    ?dwrite_game(G2),
    G2;
end_phase(G, _Phase, _Time, _DL) ->
    %%already_inserted.
    G.

unend_phase(G, M) ->
    MsgTime = M#message.time,
    DLs = G#mafia_game.deadlines,
    %% remove all DLs downto MsgTime, leave
    DLs2 = lists:dropwhile(fun(#dl{time = Time}) -> Time >= MsgTime end, DLs),
    %% Depending on top
    NewDLs =
        case DLs2 of
            [] -> %% reset first from start
                expand_deadlines(G#mafia_game{deadlines = []});
            _ ->
                %%[EndDL|DLs] = G#mafia_game.deadlines,
                TargetTime = utc_secs1970() + 11 * ?DaySecs,
                get_some_extra_dls(G, DLs2, TargetTime)
        end,
    G2 = G#mafia_game{deadlines = NewDLs},
    ?dwrite_game(G2),
    ok.

change_dl_time(OldDLs, Phase = #phase{}, Time) ->
    NewDL = #dl{phase = Phase, time = Time},
    ModF = fun(DL) ->
                   case DL#dl.phase of
                       Phase -> NewDL;
                       _ -> DL
                   end
           end,
    modify_deadlines(OldDLs, ModF).

modify_deadlines(OldDLs, ModF) ->
    [ModF(DL) || DL <- OldDLs].

%% -----------------------------------------------------------------------------
%% Manual command
-spec move_next_deadline(#mafia_game{},
                         #message{},
                         ?later | ?earlier,
                         hour() | {hour(), minute()})
                        -> {Reply :: term(), #mafia_game{}}.
move_next_deadline(G, M, Direction, TimeDiffIn) ->
    TimeDiff2 = case TimeDiffIn of
                    {H, M} -> (H * 60 + M) * 60;
                    H -> H * 3600
                end,
    DeltaSecs = case Direction of
                    ?earlier -> -TimeDiff2;
                    ?later -> TimeDiff2
                end,
    move_next_deadline(G, M, DeltaSecs).

%% -----------------------------------------------------------------------------
%% GM command
-spec move_next_deadline(#mafia_game{},
                         #message{},
                         DeltaSecs :: integer())
                        -> {Reply :: term(), #mafia_game{}}.
move_next_deadline(G, M, DeltaSecs) ->
    Time = M#message.time,
    move_dls2(G, M, DeltaSecs, calculate_phase(G, Time)).

move_dls2(G, _M, _TimeDiff, #phase{don = ?game_ended}) ->
    {{?error, ?game_ended}, G};
move_dls2(G, M, TimeDiff, _Phase) ->
    %% Calc time remaining
    MsgTime = M#message.time,
    {TimeLeft, _NextDL} = get_next_deadline(G, MsgTime, ?seconds),
    %% check that TimeDiff do not end up in the past
    if TimeLeft + TimeDiff =< 10 ->
            {{?error, move_to_past}, G};
       true ->
            ModF = fun(DL = #dl{time = DlTime}) when DlTime > MsgTime ->
                           DL#dl{time = DlTime + TimeDiff};
                      (DL) -> DL
                   end,
            OldDLs = G#mafia_game.deadlines,
            NewDLs = modify_deadlines(OldDLs, ModF),
            G2 = G#mafia_game{deadlines = NewDLs},
            ?dwrite_game(G2),
            {ok, G2}
    end.

%% -----------------------------------------------------------------------------

end_game(M, G) when G#mafia_game.game_end == ?undefined ->
    EndTime = M#message.time,
    MsgId = M#message.msg_id,

    %% remove all DLs after EndTime
    DLs2 = lists:foldr(
             fun(DL = #dl{time = DlTime}, Acc)
                   when DlTime < EndTime -> [DL | Acc];
                (_, Acc) -> Acc
             end,
             [],
             G#mafia_game.deadlines),

    %% Re-add current phase with end of game time
    NextPhase = inc_phase(hd(DLs2)),
    LastDL = #dl{phase = NextPhase, time = EndTime},
    DLs3 = [LastDL | DLs2],
    G2 = G#mafia_game{deadlines = DLs3,
                      game_end = {EndTime, MsgId}},
    ?dwrite_game(G2),
    mafia_web:do_regen_hist(EndTime, G#mafia_game.key),
    mafia_web:update_current(),
    {?game_ended, G2};
end_game(_M, G) ->
    {{error, already_game_ended}, G}.

%% -----------------------------------------------------------------------------

unend_game(G = #mafia_game{game_end = {_EndTime, _MsgId}}) ->
    [_EndDL | DLs] = G#mafia_game.deadlines,
    TargetTime = utc_secs1970() + 11 * ?DaySecs,
    NewDLs = get_some_extra_dls(G, DLs, TargetTime),
    G2 = G#mafia_game{deadlines = NewDLs,
                      game_end = ?undefined},
    ?dwrite_game(G2),
    {game_unended, G2};
unend_game(G = #mafia_game{game_end = ?undefined}) ->
    {already_running, G}.

get_some_extra_dls(_G, DLs=[#dl{time = Time} | _], Target)
  when Time > Target -> DLs;
get_some_extra_dls(G, DLs = [DL | _], Target) ->
    NewDL = inc_deadline(G, DL),
    get_some_extra_dls(G, [NewDL | DLs], Target).

%% -----------------------------------------------------------------------------
%% EUNIT tests
%% -----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

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
