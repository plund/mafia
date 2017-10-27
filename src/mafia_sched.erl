%% Still work in progress
%% 1. We need different schedules for signup and game threads
%% 2. Do we need to "coordinate" ntp_checks with all game threads?
-module(mafia_sched).

-export([
         next_timer_info/1,
         next_time/2
        ]).

-include("mafia.hrl").

-spec next_timer_info(GNum :: game_num()) ->
                             {TargetSec :: seconds1970(),
                              Actions :: list(),
                              Millisecs :: integer()}.
next_timer_info(GNum) ->
    {NowRelDLSecs, #dl{time = DlTime, phase = #phase{ptype = PT}}} =
        mafia_timer:nearest_deadline(GNum),
    TimeoutRelDLSecs = rel_secs(PT, NowRelDLSecs),
    Actions = actions(TimeoutRelDLSecs),
    NowMillisecs1970 = mafia_time:system_time_ms(),
    TargetSecs = DlTime + TimeoutRelDLSecs,
    Millisecs = TargetSecs * 1000 - NowMillisecs1970,
    {TargetSecs, Actions, Millisecs}.

%% -spec rel_secs(phase_type(), integer()) -> number().
%% %% Day nearest
rel_secs(PT, NowRelDLSecs) ->
    %% Intervals = intervals(PT),  % {Start, Per, End}
    next_time(PT, NowRelDLSecs).

-define(actions,
        [
         {-2, [ntp_check]},
         {0, [dl_poll]},
         fun(T) when T < -10; T > 10 ->  [poll, ntp_check_if_needed];
            (_) -> [poll]
         end
        ]).

-define(day_times,
        [{t_sta,  3},
         {-90,  2},
         {-30,  1},
         { -5,  0.25},
         {  2,  1},
         { 30,  2},
         { 90,  3},
         t_end
        ]).

-define(night_times,
        [{t_sta,  3},
         {-20,  2},
         {-10,  1},
         { 20,  2},
         { 40,  3},
         t_end
        ]).

-define(start_times,
        [{t_sta,  6},
         {-20,  2},
         { 40,  3},
         t_end
        ]).

-define(end_times,
        [{t_sta,  2},
         { 60,  6},
         {360, 30},
         {24 * 60, 120},
         t_end
        ]).

intervals(?day) -> calc_intervals(?day_times, []);
intervals(?night) -> calc_intervals(?night_times, []);
intervals(?game_start) -> calc_intervals(?start_times, []);
intervals(?game_ended) -> calc_intervals(?end_times, []).

calc_intervals([{StInt,  I}, t_end], Acc) ->
    A2 = [{StInt,  I, t_end} | Acc],
    lists:reverse(A2);
calc_intervals([{StInt,  I} | T = [{EndInt,  _} | _]], Acc) ->
    A2 = [{StInt,  I, EndInt} | Acc],
    calc_intervals(T, A2).

actions(Secs) ->
    lists:foldl(
      fun({N, Acts}, Acc) when N == Secs -> Acc ++ Acts;
         (F, Acc) when is_function(F) -> Acc ++ F(Secs);
         (_, Acc) -> Acc
      end,
      [],
      ?actions).

next_time(PT, NowRelDLSecs) ->
    F = fun({A, _, B}) ->
                (is_atom(A) or (A =< NowRelDLSecs))
                    and
                      (is_atom(B) or (NowRelDLSecs < B))
        end,
    {St, Per, En} = find_first(F, intervals(PT)),
    {Start, Period} =
        case {is_atom(St), is_atom(En)} of
            {true, true} ->
                if NowRelDLSecs < 0 -> {0, -Per};
                   true -> {0, Per}
                end;
            {true, false} -> {En, -Per};
            {false, true} -> {St, Per};
            {false, false} ->
                if abs(St - NowRelDLSecs)
                   < abs(En - NowRelDLSecs) ->
                        {St, Per};
                   true -> {En, -Per}
                end
        end,
    next_timeI(NowRelDLSecs, Start, Period).

find_first(F, [H | T]) ->
    case F(H) of
        true -> H;
        false -> find_first(F, T)
    end;
find_first(_, []) -> false.


next_timeI(NowRelDLSecs, Time, Period) when Period > 0 ->
    if Time > NowRelDLSecs ->
            Time;
       true ->
            next_timeI(NowRelDLSecs, Time + Period, Period)
    end;
next_timeI(NowRelDLSecs, Time, Period) when Period < 0 ->
    Prev = Time + Period,
    if Prev =< NowRelDLSecs ->
            Time;
       true ->
            next_timeI(NowRelDLSecs, Prev, Period)
    end.


            %% L = lists:reverse(
    %%       lists:foldl(
    %%         fun(Int = {_Sta, _I, _End}, Acc) ->
    %%                 [over_lap({StartT, EndT}, Int) | Acc]
    %%         end,
    %%         [],
    %%         intervals(PT))),
    %% L2 = [X || {true, X} <- L],
    %% remove_conseq_duplicates(
    %%   lists:flatten([calc_times(Period) || Period <- L2])).

%% over_lap({StartT, EndT}, {Sta, I, End}) ->
%%     St = if is_atom(Sta) -> StartT;
%%             true -> max(StartT, Sta)
%%          end,
%%     En = if is_atom(End) -> EndT;
%%             true -> min(EndT, End)
%%          end,
%%     Vals = if abs(St) > abs(En) -> {End, -I};
%%               true -> {Sta, I}
%%            end,
%%     {St =< En, {St, En, Vals}}.

%% calc_times(Period) ->
%%     calc_times(Period, out, []).

%% calc_times({Start, End, {Pos, Step}}, out, Acc)  ->
%%     case is_pos_in(Pos, Start, End) of
%%         true ->
%%             calc_times({Start, End, {Pos + Step, Step}}, in, [Pos | Acc]);
%%         false ->
%%             calc_times({Start, End, {Pos + Step, Step}}, out, Acc)
%%     end;
%% calc_times({Start, End, {Pos, Step}}, in, Acc)  ->
%%     case is_pos_in(Pos, Start, End) of
%%         true ->
%%             calc_times({Start, End, {Pos + Step, Step}}, in, [Pos | Acc]);
%%         false ->
%%             if Step > 0 -> lists:reverse(Acc);
%%                true -> Acc
%%             end
%%     end.

%% is_pos_in(Pos, Start, End) ->
%%     Start =< Pos andalso Pos =< End.

%% remove_conseq_duplicates([A | T = [B | _]]) when A == B ->
%%     remove_conseq_duplicates(T);
%% remove_conseq_duplicates([A | T = [_ | _]]) ->
%%     [maybe_int(A) | remove_conseq_duplicates(T)];
%% remove_conseq_duplicates([A]) -> [maybe_int(A)];
%% remove_conseq_duplicates([]) -> [].

%% maybe_int(I) when is_integer(I) -> I;
%% maybe_int(F) when is_float(F) ->
%%     I = round(F),
%%     if abs(F - I) < 0.0000001 -> I;
%%        true -> F
%%     end.
