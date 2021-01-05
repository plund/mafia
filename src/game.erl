%%%-------------------------------------------------------------------
%%% @author Peter Lund <peter@liber>
%%% @copyright (C) 2017, Peter Lund
%%% @doc
%%% This module handles the deadline and poll timers for one game
%%% 2 weeks after game end it stops polling
%%% There is a message ?deadline3min sent 3 min before deadline
%%% and a message ?deadline sent at deadline.
%%% @end
%%% Created : 26 Oct 2017 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(game).

-behaviour(gen_server).

%% API
-export([get_state/1,
         poll/1,
         start_polling/1,
         stop_polling/1,
         poll_minutes_tdiff/1,
         regen_history/4,
         update_current/0,
         get_id/1
        ]).

-export([start_link/2,
         start_new_game/1,
         stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mafia.hrl").

-define(SERVER, ?MODULE).

-record(state,
        {game_num :: ?undefined | integer(),
         poll_timer :: ?undefined | timer:tref(),
         poll_minutes :: ?undefined | ?stopped | number(),
         dl_timer :: ?undefined | timer:tref(),
         dl_time :: ?undefined | seconds1970()
        }).

%%%===================================================================
%%% API
%%%===================================================================
get_state(GNum) when is_integer(GNum) -> sys:get_state(get_id(GNum)).

poll(GNum) -> get_id(GNum) ! do_polling.

start_polling(GNum) -> gen_server:call(get_id(GNum), ?start_polling).
stop_polling(GNum) -> gen_server:call(get_id(GNum), ?stop_polling).

get_id(GNum) when is_integer(GNum) -> ?l2a("game_" ++ ?i2l(GNum)).

%%--------------------------------------------------------------------
%% @doc Regenerate history text page
%% @end
%%--------------------------------------------------------------------
regen_history(Mod, Term, #message{time = T}, G) ->
    regen_history(Mod, Term, T, G);
regen_history(Mod, Term, Time, G)
  when is_integer(Time) ->
    mafia_lib:dbg(Mod, {regen_history, Term}),
    game_gen:regenerate_history(Time, G).

%%--------------------------------------------------------------------
%% @doc Update current text page
%% @end
%%--------------------------------------------------------------------
update_current() ->
    gen_server:cast(?SERVER, update_current).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Id, GNum) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, GNum) ->
    gen_server:start_link({local, Id}, ?MODULE, [GNum], []).

%% Start new game - from init game procedure
start_new_game(GNum) -> mafia_sup:start_child(GNum).

stop(GNum) -> gen_server:call(get_id(GNum), 'stop').

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([GNum]) ->
    ?dbg({init, GNum}),
    S1 = #state{game_num = GNum},
    S2 = set_dl_timer(S1),
    {_Reply, S3} = maybe_change_poll_int(S2),
    {ok, S3}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_poll_timer, N}, _From, State) ->
    {Reply, S2} = set_poll_timer(State, N),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call(?start_polling, _From, State) ->
    {Reply, S2} = maybe_change_poll_int(State#state{poll_minutes = ?undefined}),
    S3 = set_dl_timer(S2),
    self() ! do_polling,
    {reply, Reply, S3};
handle_call(?stop_polling, _From, State) ->
    timer:cancel(State#state.poll_timer),
    {reply, {ok, polling_stopped}, State#state{poll_timer = ?undefined,
                                               poll_minutes = ?stopped}};
handle_call('stop', _From, State) ->
    S2 = cancel_poll_timer(State),
    {stop, normal, stop_reply, S2};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(update_current, State) ->
    update_current(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({?deadline3min, DL}, State) ->
    ?dbg({?deadline3min, DL}),
    S2 = set_dl_timer(State),
    {noreply, S2};
handle_info({?deadline, DL}, State) ->
    ?dbg({?deadline, DL}),
    mafia_data:downl_web(State#state.game_num,
                         DL,
                         ?getv(?ntp_offset_secs)),
    game_gen:regenerate_history(DL#dl.time, State#state.game_num),
    S2 = set_dl_timer(State, DL#dl.time),
    {noreply, S2};
handle_info(do_polling, State) ->
    case ?rgame(State#state.game_num) of
        [_] ->
            {_Reply, S2} = maybe_change_poll_int(State),
            if is_integer(S2#state.game_num) ->
                    mafia_data:downl_web(S2#state.game_num),
                    flush(do_polling);
               true -> ok
            end,
            update_current(S2),
            {noreply, S2};
        [] -> % 'stop'
            S2 = cancel_poll_timer(State),
            {stop, normal, S2}
    end;
handle_info(die, S) ->
    io:format(?MODULE_STRING ++ " got die ~p ~p\n",
              [S#state.game_num, time()]),
    {stop, normal, S};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_current(#state{game_num = GameNum,
                      poll_minutes = Minutes}) ->
    Phase = mafia_time:calculate_phase(GameNum),
    Opts = [{?game_key, GameNum},
            {?phase, Phase},
            {?period, Minutes},
            {?use_time, mafia_time:utc_secs1970()}],
    game_gen:update_current_txt(GameNum, Opts),
    game_gen:update_current_html(GameNum, Phase, Opts),
    ok.

%%--------------------------------------------------------------------

-spec maybe_change_poll_int(#state{}) -> {Reply::term(), #state{}}.
maybe_change_poll_int(S = #state{poll_timer = TRef,
                                 poll_minutes = TMins,
                                 game_num = GNum}) ->
    Mins = case poll_minutes_tdiff(GNum) of
               {MinsR, _} -> MinsR;
               ?none -> ?none
           end,
    if TMins == ?stopped -> {no_change, S};
       Mins == ?none ->
            case mafia_lib:is_ongoing_game(GNum) of
                true -> ?dbg({cancel_poll_timer, GNum, ?rgame(GNum)});
                _ -> ok
            end,
            {cancelled, cancel_poll_timer(S)};
       TRef == ?undefined; Mins /= TMins ->
            set_poll_timer(S, Mins);
       true -> {no_change, S}
    end.

-define(_10minMs, 10 * 60 * 1000).
-define(_3minMs, 3 * 60 * 1000).

set_dl_timer(S) -> set_dl_timer(S, mafia_time:utc_secs1970()).

set_dl_timer(S, Time) ->
    set_dl_timer(S, Time, ?rgame(S#state.game_num)).

set_dl_timer(S, _Time, []) -> S;
set_dl_timer(S, Time, [G]) ->
    set_dl_timer(S, Time, G);
set_dl_timer(S, Time, G)
  when G#mafia_game.start_time /= ?undefined,
       G#mafia_game.time_zone /= ?undefined,
       G#mafia_game.dst_zone /= ?undefined,
       G#mafia_game.thread_id /= ?undefined ->
    S2 = cancel_dl_timer(S),
    case mafia_time:get_nxt_deadline(G, Time) of
        #dl{phase = #phase{ptype = ?game_ended}} -> S2;
        DL = #dl{} ->
            NtpOffsetSecs = ?getv(?ntp_offset_secs),
            NtpOffsetMilliSecs = round(NtpOffsetSecs * 1000),
            SysTimeMs = mafia_time:system_time_ms(),
            RemMs = DL#dl.time * 1000
                - SysTimeMs
                - NtpOffsetMilliSecs,
            ?dbg({set_dl_timer, SysTimeMs, NtpOffsetMilliSecs, RemMs}),
            set_dl_timer2(S2, DL, RemMs)
    end;
set_dl_timer(State, _, _) -> State.

set_dl_timer2(S, DL, RemMs) when RemMs > ?_10minMs ->
    DelayMs = RemMs - ?_3minMs,
    set_dl_timer3(S, ?deadline3min, DL, DelayMs);
set_dl_timer2(S, DL, RemMs) when RemMs > 0 ->
    set_dl_timer3(S, ?deadline, DL, RemMs);
set_dl_timer2(S, _, _) -> S.

set_dl_timer3(S, DeadlineType, DL, DelayMs) ->
    ?dbg({set_dl_timer3, DeadlineType, DL, DelayMs}),
    {ok, TRef} = timer:send_after(DelayMs, {DeadlineType, DL}),
    S#state{dl_timer = TRef,
            dl_time = DL#dl.time}.

cancel_dl_timer(S) ->
    if S#state.dl_timer /= ?undefined ->
            timer:cancel(S#state.dl_timer);
       true -> ok
    end,
    flush({?deadline, any}),
    S#state{dl_timer = ?undefined}.

-spec set_poll_timer(#state{}, number()) -> {Reply :: term(), #state{}}.
set_poll_timer(S, TMins) when is_number(TMins), TMins > 0 ->
    S2 = cancel_poll_timer(S),
    {ok, TRef} = timer:send_interval(trunc(TMins * ?MINUTE_MS), do_polling),
    ?dbg({set_poll_timer, S#state.game_num, TMins}),
    Reply = {interval_changed,
             {old, S2#state.poll_minutes},
             {new, TMins}},
    {Reply, S2#state{poll_timer = TRef,
                     poll_minutes = TMins}}.

cancel_poll_timer(S) ->
    if S#state.poll_timer /= ?undefined ->
            timer:cancel(S#state.poll_timer);
       true -> ok
    end,
    flush(do_polling),
    S#state{poll_timer = ?undefined}.

flush({Msg, X}) ->
    receive {Msg, _} -> flush({Msg, X})
    after 0 -> ok
    end;
flush(Msg) ->
    receive Msg -> flush(Msg)
    after 0 -> ok
    end.

%% -----------------------------------------------------------------------------

-spec poll_minutes_tdiff(GNum :: thread_id()) -> ?none | {number(), integer()}.
poll_minutes_tdiff(GNum) ->
    case mafia_time:nearest_deadline(GNum) of
        ?none -> ?none;
        {RelTimeSecs, #dl{phase = #phase{ptype = Ptype}}} ->
            {t_mins(Ptype, RelTimeSecs), RelTimeSecs}
    end.

%% 2 weeks is 14 * 24 * 60 minutes
-define(EndPollMins, (14 * 24 * 60)).
-define(GameNormalMins, 3).

-define(m2s(Min), (Min * ?MinuteSecs)).

-spec t_mins(atom(), integer()) -> number().
%% Game starting
t_mins(?game_start, T) when T < ?m2s(-12*60) -> 6;
%% t_mins(?game_start, _T) -> 2;
t_mins(?game_start, T) when T < ?m2s(90) -> 2;
t_mins(?game_start, T) when T >= ?m2s(90) -> ?GameNormalMins;
%% Day nearest
t_mins(?day, T) when T < ?m2s(-90) -> ?GameNormalMins;
t_mins(?day, T) when T < ?m2s(-30) -> 2;
t_mins(?day, T) when T < ?m2s(-5) -> 1;
t_mins(?day, T) when T < ?m2s(2) -> 0.25;
t_mins(?day, T) when T < ?m2s(30) -> 1;
t_mins(?day, T) when T < ?m2s(90) -> 2;
t_mins(?day, T) when T >= ?m2s(90) -> ?GameNormalMins;
%% Night nearest
t_mins(?night, T) when T < ?m2s(-90) -> ?GameNormalMins;
t_mins(?night, T) when T < ?m2s(-30) -> 2;
t_mins(?night, T) when T < ?m2s(30) -> 1;
t_mins(?night, T) when T < ?m2s(90) -> 2;
t_mins(?night, T) when T >= ?m2s(90) -> ?GameNormalMins;
%% Game has ended
t_mins(?game_ended, T) when T < ?m2s(60) -> 2;
t_mins(?game_ended, T) when T < ?m2s(180) -> 4;
t_mins(?game_ended, T) when T < ?m2s(360) -> 10;
t_mins(?game_ended, T) when T < ?m2s(24*60) -> 20;
t_mins(?game_ended, T) when T < ?m2s(?EndPollMins) -> 120;
t_mins(?game_ended, T) when T >= ?m2s(?EndPollMins) -> ?none.

%% -----------------------------------------------------------------------------
