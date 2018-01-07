-module(mafia_time).

-export([system_time_ms/0,
         utc_secs1970/0,
         utc_secs1970/1,
         utc_day1970/0,
         utc_day2date/1,
         date2utc_day/1,

         show_time_offset/0,
         set_time_offset/1,
         conv_gtime_secs1970/2,
         human2datetime/1,

         get_tz_dst/0,
         get_tz_dst/2,
         secs1970_to_local_datetime/3,
         secs2day_hour_min_sec/1,

         dst_change_date/0,
         dst_change_date/1,
         dst_change_date/3,
         dst_name/1,

         initial_deadlines/1,
         set_dst_changes/1,
         update_deadlines/1,
         inc_phase/1,
         decr_phase/1,

         hh_mm_to_deadline/2,  %% These are similar
         hh_mm_to_time/2,
         get_next_deadline/1,
         get_next_deadline/2,
         next_deadlines/3,
         calculate_phase/1,
         calculate_phase/2,
         game_phases/3,
         phases_upto/1,
         find_phase_with_time/2,
         get_nxt_deadline/2,
         get_prev_deadline/2,
         get_time_for_phase/2,
         get_time_for_prev_phase/2,
         nearest_deadline/1,
         nearest_deadline/2,

         end_phase/3,
         unend_phase/2,
         move_next_deadline/3,
         move_next_deadline/4,
         end_game/2,
         unend_game/1
        ]).

-include("mafia.hrl").

%% -----------------------------------------------------------------------------
-spec system_time_ms() -> milliseconds1970().
system_time_ms() ->
    %% os:system_time() is "unixtime" with high precision
    STime = os:system_time(),
    erlang:convert_time_unit(STime, native, millisecond)
        - get_time_offset() * 1000.

-spec utc_secs1970() -> seconds1970().
utc_secs1970() -> utc_secs1970I() - get_time_offset().

-spec utc_secs1970(datetime()) -> seconds1970().
utc_secs1970(DateTime) ->
    utc_secs1970I(DateTime) - get_time_offset().

%% internal
utc_secs1970I() -> utc_secs1970I(calendar:universal_time()).

utc_secs1970I(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?GSECS_1970.

%% Return the current number of days since Jan 1 1970
-spec utc_day1970() -> Day :: integer().
utc_day1970() ->
    greg_secs2secs1970(
      calendar:datetime_to_gregorian_seconds(
        calendar:universal_time())) div ?DaySecs.

-spec utc_day2date(Day :: integer()) -> date().
utc_day2date(DayNum) ->
    element(1, calendar:gregorian_seconds_to_datetime(
                 DayNum * ?DaySecs + ?GSECS_1970)).

-spec date2utc_day(Date :: date()) -> Day :: integer().
date2utc_day(Date) ->
    greg_secs2secs1970(
      calendar:datetime_to_gregorian_seconds({Date, {0,0,0}})) div ?DaySecs.

-spec greg_secs2secs1970(greg_secs()) -> seconds1970().
greg_secs2secs1970(GregSecs) -> GregSecs - ?GSECS_1970.

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
set_time_offsetI({move, DiffSecs}) when is_integer(DiffSecs) ->
    get_time_offset() + DiffSecs;
set_time_offsetI({msg_key, MsgKey = {MsgId,_}}) when is_integer(MsgId) ->
    case mafia_lib:rmessI(MsgKey) of
        [] -> 0;
        [M] -> utc_secs1970I() - M#message.time
    end;
set_time_offsetI({days_hours, NumDays, NumHours})
  when is_integer(NumDays), is_integer(NumHours) ->
    (NumDays * 24 + NumHours) * ?HourSecs.

conv_gtime_secs1970(G, DateTime) ->
    IsDst = is_dst(DateTime, G),
    TZ = G#mafia_game.time_zone,
    greg_secs2secs1970(utc_gs(DateTime, TZ, IsDst)).

utc_gs(DateTime, TZ, Dst) ->
    calendar:datetime_to_gregorian_seconds(DateTime)
        - (TZ + if Dst -> 1; true -> 0 end) * ?HourSecs.

%% -----------------------------------------------------------------------------
%% Converts "Mon Jan 01, 2018 12:58 am" to datetime()
human2datetime(Str) ->
    [_WD, Mon, Day, Year, Time, APM] = string:tokens(Str, " ,"),
    [HhStr, MmStr] = string:tokens(Time, ":"),
    HH = hour(?l2i(HhStr), APM),
    MM = ?l2i(MmStr),
    {{?l2i(Year), month2int(Mon), ?l2i(Day)}, {HH, MM, 0}}.

hour(12, "am") -> 0;
hour(12, "pm") -> 12;
hour(HH, "pm") -> 12 + HH;
hour(HH, "am") -> HH.

month2int("Jan") -> 1;
month2int("Feb") -> 2;
month2int("Mar") -> 3;
month2int("Apr") -> 4;
month2int("May") -> 5;
month2int("Jun") -> 6;
month2int("Jul") -> 7;
month2int("Aug") -> 8;
month2int("Sep") -> 9;
month2int("Oct") -> 10;
month2int("Nov") -> 11;
month2int("Dec") -> 12.

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

-spec secs1970_to_local_datetime(
        Time :: seconds1970(),
        TzH :: -12..13,
        Dst :: boolean()) -> datetime().
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

dst_change_date() ->
    [?eu, ?usa, ?australia, ?new_zeeland].

dst_name(?eu) -> ?eu_str;
dst_name(?usa) -> ?usa_str;
dst_name(?australia) -> ?australia_str;
dst_name(?new_zeeland) -> ?new_zeeland_str.

dst_change_date(?eu) ->
    ["Last Sunday March",
     "Last Sunday October"];
dst_change_date(?usa) ->
    ["Second Sunday March",
     "First Sunday November"];
dst_change_date(?australia) ->
    ["First Sunday October",
     "First Sunday April"];
dst_change_date(?new_zeeland) ->
    ["Last Sunday September",
     "First Sunday April"].

dst_change_date(?eu, Year, ?to_dst) ->
    %% Last Sunday March
    Day = calendar:day_of_the_week(Year, 3, 31),
    DateInMonth = 31 - Day rem 7,
    {Year, 3, DateInMonth};
dst_change_date(?eu, Year, ?to_normal) ->
    %% Last Sunday October
    Day = calendar:day_of_the_week(Year, 10, 31),
    DateInMonth = 31 - Day rem 7,
    {Year, 10, DateInMonth};
dst_change_date(?usa, Year, ?to_dst) ->
    %% Second Sunday March
    Day = calendar:day_of_the_week(Year, 3, 1),
    DateInMonth = 1 + 7 - Day + 7,
    {Year, 3, DateInMonth};
dst_change_date(?usa, Year, ?to_normal) ->
    %% First Sunday November
    Day = calendar:day_of_the_week(Year, 11, 1),
    DateInMonth = 1 + 7 - Day,
    {Year, 11, DateInMonth};
dst_change_date(?australia, Year, ?to_dst) ->
    %% First Sunday October
    Day = calendar:day_of_the_week(Year, 10, 1),
    DateInMonth = 1 + 7 - Day,
    {Year, 10, DateInMonth};
dst_change_date(?australia, Year, ?to_normal) ->
    %% First Sunday April
    Day = calendar:day_of_the_week(Year, 4, 1),
    DateInMonth = 1 + 7 - Day,
    {Year, 4, DateInMonth};
dst_change_date(?new_zeeland, Year, ?to_dst) ->
    %% Last Sunday September
    Day = calendar:day_of_the_week(Year, 9, 30),
    DateInMonth = 30 - Day rem 7,
    {Year, 9, DateInMonth};
dst_change_date(?new_zeeland, Year, ?to_normal) ->
    %% First Sunday April
    Day = calendar:day_of_the_week(Year, 4, 1),
    DateInMonth = 1 + 7 - Day,
    {Year, 4, DateInMonth}.

%% -----------------------------------------------------------------------------

initial_deadlines(G) ->
    G2 = set_dst_changes(G),
    add_deadlines(G2#mafia_game{deadlines = []}).

%% Set dst_changes field according to dst_zone (and time_zone for EU)
set_dst_changes(G) ->
    DstZone = G#mafia_game.dst_zone,
    {StartDate = {Year, _, _}, _} = G#mafia_game.start_time,
    if DstZone /= ?none ->
            AllDstDates = lists:sort(
                            [{dst_change_date(DstZone, Y, D), D}
                             || Y <- [Year - 1, Year, Year + 1],
                                D <- [?to_normal, ?to_dst]]),
            DstDates = relevant_dst(StartDate, AllDstDates),
            DstChanges = set_dst_time(G, DstDates),
            G#mafia_game{dst_changes = DstChanges};
       DstZone == ?none ->
            G#mafia_game{dst_changes = []}
    end.

%% return 1 before and 2 after StartDate
relevant_dst(SD, [_D1, D2 = {SD2, _} | T]) when SD2 < SD ->
    relevant_dst(SD, [D2|T]);
relevant_dst(_, DstChanges) -> lists:sublist(DstChanges, 3).

%% Add times to the DST dates
set_dst_time(G, DstDates) ->
    Hour = case G#mafia_game.dst_zone of
               ?eu -> G#mafia_game.time_zone + 1;
               _ -> 2
           end,
    [{{DT, {Hour, 0, 0}}, Dir} || {DT, Dir} <- DstDates].

%% @doc update the game in mnesia
update_deadlines(ThId) ->
    case ?rgame(ThId) of
        [] -> [];
        [#mafia_game{} = G] ->
            NewDLs = expand_deadlines(G),
            ?dwrite_game(game_t1, G#mafia_game{deadlines = NewDLs}),
            NewDLs
    end.

add_deadlines(G) ->
    G#mafia_game{deadlines = expand_deadlines(G)}.

%% @doc Return an expanded list of deadlines (newest first)
-spec expand_deadlines(#mafia_game{}) -> [#dl{}].
expand_deadlines(G) ->
    DLs = G#mafia_game.deadlines,
    DLs2 = if DLs == [] ->
                   [#dl{phase = first_phase(),
                        time = first_deadline_secs1970(G)}];
              true -> DLs
           end,
    LastDL = hd(DLs2),
    TargetTime = LastDL#dl.time + 11 * ?DaySecs,
    get_some_extra_dls(G, DLs2, TargetTime).

first_phase() -> #phase{num = 0, ptype = ?game_start}.

%% gregorian_seconds().
first_deadline_secs1970(G) ->
    greg_secs2secs1970(first_deadline_greg_secs(G)).

first_deadline_greg_secs(G) ->
    #mafia_game{time_zone = TZ,
                start_time = StartDateTime
               } = G,
    IsDst = is_dst(StartDateTime, G),
    utc_gs(StartDateTime, TZ, IsDst).

%% -----------------------------------------------------------------------------

-spec inc_phase(#phase{} | #dl{}) -> ?undefined | #phase{}.
inc_phase(#dl{phase = Phase}) -> inc_phase(Phase);
inc_phase(Ph = #phase{ptype = ?game_start}) ->
    Ph#phase{num = 1, ptype = ?day};
inc_phase(#phase{ptype = ?game_ended}) ->
    ?undefined;
inc_phase(Ph = #phase{ptype = ?day}) ->
    Ph#phase{ptype = ?night};
inc_phase(#phase{num = Num, ptype = ?night}) ->
    #phase{num = Num + 1, ptype = ?day}.

-spec decr_phase(#phase{} | #dl{}) -> ?undefined | #phase{}.
decr_phase(#dl{phase = Phase}) -> decr_phase(Phase);
decr_phase(#phase{ptype = ?game_start}) ->
    ?undefined;
decr_phase(#phase{ptype = ?game_ended}) ->
    ?undefined;
decr_phase(#phase{num = 1, ptype = ?day}) ->
    #phase{num = 0, ptype = ?game_start};
decr_phase(#phase{num = Num, ptype = ?day}) ->
    #phase{num = Num - 1, ptype = ?night};
decr_phase(Ph = #phase{ptype = ?night}) ->
    Ph#phase{ptype = ?day}.

-spec inc_deadline(#mafia_game{}, #dl{}) -> #dl{}.
inc_deadline(G, DL = #dl{time = Time}) ->
    NextPhase = inc_phase(DL#dl.phase),
    NHours = case NextPhase#phase.ptype of
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
                 ?same -> 0;
                 ?to_normal -> ?HourSecs;
                 ?to_dst -> -?HourSecs
             end,
    #dl{phase = NextPhase, time = NTime + Adjust}.


-spec dst_change(Start :: seconds1970(),
                 End :: seconds1970(),
                 DstChanges :: [dst_change()],
                 TZ :: integer())
                -> ?same | ?to_normal | ?to_dst.
dst_change(Start, End, DstChanges, TZ) ->
    %% dst_changes = [{{{2016,11,6},{2,0,0}}, false},
    %%                {{{2017, 4,1},{2,0,0}}, true}],
    StartDT = secs1970_to_local_datetime(Start, TZ, false),
    EndDT   = secs1970_to_local_datetime(End,   TZ, false),
    do_dst_change(DstChanges, StartDT, EndDT).

do_dst_change([{DT, _} | _], _, EndDT) when EndDT =< DT -> ?same;
do_dst_change([{DT, _} | T], StartDT, EndDT) when DT =< StartDT ->
    do_dst_change(T, StartDT, EndDT);
do_dst_change([{_DT, DstDirection} | _], _, _) -> DstDirection;
do_dst_change([], _, _) -> ?same.

is_dst(DateTime, G) ->
    %% dst_zone without DSTs should have dst_changes = []
    %% => initiate Acc with ?to_normal
    DstDir = lists:foldl(fun({DT, Dir}, Acc) ->
                                 if DT < DateTime -> Dir;
                                    true  -> Acc
                                 end
                         end,
                         ?to_normal,
                         G#mafia_game.dst_changes),
    case DstDir of
        ?to_dst -> true;
        ?to_normal -> false
    end.

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

hh_mm_to_time(TimeA, TimeB) ->
    TimeDiff = abs(TimeA - TimeB),
    Days = TimeDiff div ?DaySecs,
    {HH, MM, _SS} = calendar:seconds_to_time(TimeDiff rem ?DaySecs),
    {Days * 24 + HH, MM}.

%% -----------------------------------------------------------------------------

-spec get_next_deadline(GNum :: integer())
                       -> {Remain :: {Days :: integer(), time()},
                           ?game_ended | #dl{}}.
get_next_deadline(GNum) when is_integer(GNum) ->
    NowTime = utc_secs1970(),
    get_next_deadline(GNum, NowTime).

get_next_deadline(GNum, Time) when is_integer(GNum); is_atom(GNum) ->
    get_next_deadline(?rgame(GNum), Time);
get_next_deadline([], _Time) -> false;
get_next_deadline([#mafia_game{} = G], Time) ->
    get_next_deadline(G, Time);
get_next_deadline(#mafia_game{game_end = {EndTime, _EndMsgId}},
                  Time) when Time >= EndTime ->
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

%% Returns Num next deadlines in time for game
%% Return also previous deadlines in case future deadlines are not many enough
next_deadlines(#mafia_game{deadlines = DLs}, Time, Num) ->
    RevDLs = ?lrev(DLs),
    NextDLs = lists:dropwhile(fun(#dl{time = DlTime}) -> DlTime =< Time end,
                              RevDLs),
    if Num =< length(NextDLs) ->
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

-spec calculate_phase(G :: integer() | #mafia_game{})
                     -> #phase{}.
calculate_phase(G) ->
    Time = utc_secs1970(),
    calculate_phase(G, Time).

-spec calculate_phase(G :: integer() | #mafia_game{},
                      Time :: seconds1970())
                     -> false | #phase{}.
calculate_phase(GNum, Time) when is_integer(GNum) ->
    case ?rgame(GNum) of
        [G] -> calculate_phase(G, Time);
        [] -> false
    end;
calculate_phase(#mafia_game{game_end = {EndTime, _}}, Time)
  when Time >= EndTime ->
    #phase{ptype = ?game_ended};
calculate_phase(Game, Time) ->
    DL = get_nxt_deadline(Game, Time),
    DL#dl.phase.

%% game_phases from T1 upto T2 when T1 < T2
game_phases(G = #mafia_game{}, T1, T2)
  when is_integer(T1), is_integer(T2) ->
    element(
      2,
      lists:foldr(
        fun(#dl{time = DT, phase = Ph}, {init, Acc})
              when DT > T2 ->
                {stop, [Ph | Acc]};
           (#dl{time = DT}, Acc = {init, _})
              when DT < T1 ->
                Acc;
           (#dl{time = DT, phase = Ph}, {init, Acc})
              when DT > T1 ->
                {f1, [Ph | Acc]};
           (#dl{time = DT, phase = Ph}, {f1, Acc})
              when DT < T2 ->
                {f1, [Ph | Acc]};
           (#dl{time = DT, phase = Ph}, {f1, Acc})
              when DT > T2 ->
                {stop, [Ph | Acc]};
           (_, Acc = {stop, _}) ->
                Acc;
           (_, Acc) ->
                Acc
        end,
        {init, []},
        G#mafia_game.deadlines)).

phases_upto(EndPhase) ->
    phases_upto(EndPhase, #phase{num = 1, ptype = ?day}, []).

phases_upto(EndPhase, Ph, Acc) when EndPhase > Ph ->
    phases_upto(EndPhase, inc_phase(Ph), [Ph | Acc]);
phases_upto(_, _, Acc) ->
    ?lrev(Acc).

-spec find_phase_with_time(#mafia_game{}, seconds1970()) -> false | #phase{}.
find_phase_with_time(G, Time) ->
    case lists:keyfind(Time, #dl.time, G#mafia_game.deadlines) of
        false -> false;
        DL = #dl{} -> DL#dl.phase
    end.

%% @doc Get next deadline *after* Time
%% @end
-spec get_nxt_deadline(#mafia_game{}, seconds1970()) -> #dl{}.
get_nxt_deadline(#mafia_game{game_end = {EndTime, _}},
                 Time) when Time >= EndTime ->
    #dl{time = EndTime, phase = #phase{ptype = ?game_ended}};
get_nxt_deadline(Game = #mafia_game{}, Time) ->
    {ComingDLs, _} = split_dls(Game, Time),
    Game2 = if length(ComingDLs) < 4 ->
                    G2 = add_deadlines(Game),
                    ?dwrite_game(game_t2, G2),
                    G2;
               true -> Game
            end,
    GetNext =
        fun({Take, _Drop}) ->
                case Take of
                    [] -> #dl{phase = #phase{ptype = ?game_ended}};
                    _ ->
                        lists:last(Take)
                end
        end,
    GetNext(split_dls(Game2, Time)).

-spec get_prev_deadline(#mafia_game{}, seconds1970()) -> #dl{} | ?undefined.
get_prev_deadline(Game = #mafia_game{}, Time) when is_integer(Time) ->
    GetPrev =
        fun({_Take, Drop}) ->
                case Drop of
                    [DL|_] -> DL;
                    _ -> ?undefined
                end
        end,
    GetPrev(split_dls(Game, Time)).

%% -> {TakeWhile, DropWhile}
%% {ComingDLs, PrevDLs}
split_dls(Game, Time) ->
    lists:splitwith(
      fun(#dl{time = DLTime}) -> DLTime > Time end,
      Game#mafia_game.deadlines).

get_time_for_phase(GNum, Phase) when is_integer(GNum) ->
    get_time_for_phase(?rgame(GNum), Phase);
get_time_for_phase([G], Phase) -> get_time_for_phase(G, Phase);
get_time_for_phase(G = #mafia_game{}, #phase{ptype = ?game_ended}) ->
    case G#mafia_game.game_end of
        ?undefined -> ?undefined;
        {Time, _MsgId} -> Time
    end;
get_time_for_phase(G = #mafia_game{}, Phase = #phase{}) ->
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
            first_deadline_secs1970(G);
        Time -> Time
    end.

-spec nearest_deadline(integer() | #mafia_game{} | [#mafia_game{}])
                      -> {integer(), #dl{}} | none.
nearest_deadline(GameKey) when is_integer(GameKey) ->
    nearest_deadline(?rgame(GameKey));
nearest_deadline([]) -> none;
nearest_deadline([G]) ->
    Now = utc_secs1970(),
    case G#mafia_game.game_end of
        ?undefined ->
            nearest_deadline(G, Now);
        {EoGTime, _MsgId} ->
            {Now - EoGTime,
             #dl{phase = #phase{ptype = ?game_ended},
                 time = EoGTime}}
    end.

-spec nearest_deadline(integer() | #mafia_game{} | [#mafia_game{}],
                       seconds1970())
                      -> {integer(), phase_type()} | none.
nearest_deadline(GNum, Time) when is_integer(GNum) ->
    nearest_deadline(?rgame(GNum), Time);
nearest_deadline([], _) -> ?none;
nearest_deadline([G = #mafia_game{}], Time) ->
    nearest_deadline(G, Time);
nearest_deadline(#mafia_game{deadlines = []}, _) -> ?none;
nearest_deadline(G = #mafia_game{}, Time) ->
    DLs = G#mafia_game.deadlines,
    {_, TDiff, NearestDL} =
        hd(lists:sort([{abs(Time - DlTime), Time - DlTime, DL}
                       || DL = #dl{time = DlTime} <- DLs])),
    {TDiff, NearestDL}.

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
    ?dwrite_game(game_t3, G2),
    ?regen_history(end_phase, Time, G2),
    G2;
end_phase(G, _Phase, _Time, _DL) ->
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
    ?dwrite_game(game_t4, G2),
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

move_dls2(G, _M, _TimeDiff, #phase{ptype = ?game_ended}) ->
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
            ?dwrite_game(game_t5, G2),
            {ok, G2}
    end.

%% -----------------------------------------------------------------------------

end_game(M, G) ->
    EndTime = M#message.time,
    MsgKey = M#message.msg_key,

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

    %% Regen right before ending!
    ?regen_history(end_game_1, EndTime, G),
    G2 = G#mafia_game{deadlines = DLs3,
                      game_end = {EndTime, ?e1(MsgKey)}},
    ?dwrite_game(game_t6, G2),
    %% regenerate final game_status files
    ?regen_history(end_game_2, EndTime, G2),
    {?game_ended, G2}.

%% -----------------------------------------------------------------------------

unend_game(G = #mafia_game{game_end = {_EndTime, _MsgId}}) ->
    [_EndDL | DLs] = G#mafia_game.deadlines,
    TargetTime = utc_secs1970() + 11 * ?DaySecs,
    NewDLs = get_some_extra_dls(G, DLs, TargetTime),
    G2 = G#mafia_game{deadlines = NewDLs,
                      game_end = ?undefined},
    ?dwrite_game(game_t7, G2),
    {game_unended, G2};
unend_game(G = #mafia_game{game_end = ?undefined}) ->
    {already_running, G}.

%% Must exist ONE dl before
-spec get_some_extra_dls(#mafia_game{}, [#dl{}], seconds1970()) -> [#dl{}].
get_some_extra_dls(_G, DLs = [#dl{time = Time} | _], Target)
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

-define(DST0, {{{2016,11,22},{2,0,0}}, ?to_normal}).
-define(DST1, {{{2016,11,30},{2,0,0}}, ?to_normal}).
-define(DST2, {{{2016,12,22},{2,0,0}}, ?to_dst}).
-define(DST3, {{{2016,12,22},{2,0,0}}, ?to_normal}).
-define(DST4, {{{2016,12,25},{2,0,0}}, ?to_dst}).
-define(DST5, {{{2016,12,28},{2,0,0}}, ?to_normal}).

do_dst_change_test_() ->
    [?_assert(?same == do_dst_change([?DST0, ?DST1], ?D1, ?D2)),
     ?_assert(?to_dst == do_dst_change([?DST1, ?DST2], ?D1, ?D2)),
     ?_assert(?to_normal == do_dst_change([?DST1, ?DST3], ?D1, ?D2)),
     ?_assert(?same == do_dst_change([?DST1, ?DST5], ?D1, ?D2)),
     ?_assert(?same == do_dst_change([?DST4, ?DST5], ?D1, ?D2)),
     ?_assert(?to_normal == do_dst_change([?DST3], ?D1, ?D2)),
     ?_assert(?same == do_dst_change([], ?D1, ?D2))
    ].

-define(Time1, 1481655677). % ca 2000 CET 161213
-define(Time0, ?Time1 + ?DaySecs). % ca 2000 CET 161212
-define(Time2, ?Time1 + ?DaySecs). % ca 2000 CET 161214
-define(Time3, ?Time1 + 2*?DaySecs). % ca 2000 CET 161215
-define(DST6, {{{2016,12,14},{2,0,0}}, ?to_dst}).
-define(DST7, {{{2016,12,14},{2,0,0}}, ?to_normal}).

dst_change_test_() ->
    [
     ?_assert(?same == dst_change(?Time0, ?Time1, [?DST6], -5)),
     ?_assert(?to_dst == dst_change(?Time1, ?Time2, [?DST6], -5)),
     ?_assert(?to_normal == dst_change(?Time1, ?Time2, [?DST7], -5)),
     ?_assert(?same == dst_change(?Time2, ?Time3, [?DST7], -5))
    ].

human2datetime_test_() ->
    [
     ?_assertMatch({{2017, 10, 19}, {16, 58, 00}},
                   human2datetime("Thu Oct 19, 2017 4:58 pm")),
     ?_assertMatch({{2017, 12, 31}, {21, 12, 00}},
                   human2datetime("Sun Dec 31, 2017 9:12 pm")),
     ?_assertMatch({{2018, 1, 1}, {0, 58, 00}},
                   human2datetime("Mon Jan 01, 2018 12:58 am")),
     ?_assertMatch({{2018, 1, 1}, {1, 24, 00}},
                   human2datetime("Mon Jan 01, 2018 1:24 am"))
    ].
