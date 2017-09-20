%%%-------------------------------------------------------------------
%%% @author Peter Lund
%%% @copyright (C) 2016, Peter Lund
%%% @doc Updates the DB every 10 mins, 5 min last hour and 1 min last
%%% 7 min to day deadline.
%%% @end
%%% Created : 10 Dec 2016 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(mafia_web).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         start/1,
         stop/0,

         poll/0,
         start_polling/0,
         stop_polling/0,

         start_web/0,
         stop_httpd/0,
         stop_httpd/1,

         get_state/0,
         get_ntp_offset/0,

         change_current_game/1,
         regen_history/2,
         update_current/0,
         update_current/2,
         get_html/2
        ]).

%% test
-export([get_interface_ip/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mafia.hrl").

-record(state,
        {timer :: ?undefined | timer:tref(),
         timer_minutes :: ?undefined | ?stopped | number(),
         dl_timer :: ?undefined | timer:tref(),
         dl_time :: ?undefined | seconds1970(),
         web_pid :: ?undefined | pid(),
         tls_pid :: ?undefined | pid(),
         game_num :: ?undefined | integer(),
         offset_timer :: ?undefined | timer:tref(),
         ntp_offset_secs = 0.0 :: float()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:format(": start_link\n"),
    mafia:setup_mnesia(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [polling], []).

start() ->  %% to be removed?
    io:format(": start\n"),
    mafia:setup_mnesia(),
    gen_server:start({local, ?SERVER}, ?MODULE, [polling], []).

start(no_polling) ->
    mafia:setup_mnesia(),
    gen_server:start({local, ?SERVER}, ?MODULE, [no_polling], []).

start_web() ->
    gen_server:call(?SERVER, start_web).

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?SERVER, 'stop').

poll() -> ?SERVER ! do_polling.

start_polling() -> gen_server:call(?SERVER, ?start_polling).
stop_polling() -> gen_server:call(?SERVER, ?stop_polling).

stop_httpd() ->
    IpStr = bind_address(),
    RespStr = os:cmd("telnet " ++ IpStr ++ " " ++ ?i2l(?WEBPORT)),
    case string:str(RespStr, "Escape character is") of
        0 -> %% nothing running on port
            io:format("Verify no webserver running at ~s, port ~p\n",
                      [IpStr, ?WEBPORT]);
        _ ->
            io:format("Stopping webserver running at ~s, port ~p\n",
                      [IpStr, ?WEBPORT]),
            IP = list_to_tuple([?l2i(Str)
                                || Str <- string:tokens(IpStr, ".")]),
            inets:stop(httpd, {IP, ?WEBPORT})
    end.

stop_httpd(a) ->
    io:format("Stopping webserver at ~p, port ~p\n",
              [{192,168,0,100}, ?WEBPORT]),
    inets:stop(httpd, {{192,168,0,100}, ?WEBPORT});
stop_httpd(b) ->
    io:format("Stopping webserver at ~p, port ~p\n",
              [{192,168,0,3}, ?WEBPORT]),
    inets:stop(httpd, {{192,168,0,3}, ?WEBPORT}).

%%--------------------------------------------------------------------
%% @doc Show gen_server internal state
%% @end
%%--------------------------------------------------------------------
get_state()  ->
    gen_server:call(?SERVER, get_state).

get_ntp_offset()  ->
    case catch gen_server:call(?SERVER, get_ntp_offset, 200) of
        {'EXIT', _} -> "Unknown NTP offset";
        OffsetSecs when is_float(OffsetSecs) ->
            OffsMilliStr = float_to_list(OffsetSecs * 1000,
                                         [{decimals, 3}]),
            OffsMilliStr ++ " millisecs"
    end.

%%--------------------------------------------------------------------
%% @doc Update current text page
%% @end
%%--------------------------------------------------------------------
update_current() ->
    gen_server:cast(?SERVER, update_current).

update_current(Time, G) ->
    update_current2(Time, G).

%%--------------------------------------------------------------------
%% @doc Change current game
%% @end
%%--------------------------------------------------------------------
change_current_game(GNum) ->
    gen_server:call(?SERVER, {change_current_game, GNum}).

%%--------------------------------------------------------------------
%% @doc Regenerate history text page
%% @end
%%--------------------------------------------------------------------
regen_history(M, {G = #mafia_game{}, Phase}) ->
    regen_history(M, {G#mafia_game.game_num, Phase});
regen_history(M = #message{}, G) ->
    regen_history(M#message.time, G);
regen_history(Time, G = #mafia_game{}) ->
    regen_history(Time, G#mafia_game.game_num);
regen_history(Time, GNum) ->
    regen_historyI(Time, GNum).

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
init([Arg]) ->
    mafia:setup_mnesia(),
    GameKey = ?getv(?game_key),
    {ok, OffsetTimer} = init_ntp_offset(),
    State0 = #state{game_num = GameKey,
                    offset_timer = OffsetTimer},
    State = start_web(State0),
    S2 = set_dl_timer(State),
    TimerMins = case ?getv(?timer_minutes) of
                    ?undefined -> 10;
                    V -> V
                end,
    if Arg == polling ->
            ?dbg(start_polling),
            self() ! do_polling,
            {_Reply, S3} = set_timer_interval(S2, TimerMins),
            {ok, S3};
       Arg == no_polling ->
            ?dbg(start_no_polling),
            {ok, S2}
    end.

init_ntp_offset() ->
    ?SERVER ! check_ntp_offset,
    timer:send_interval(10 * ?MINUTE_MS, check_ntp_offset).

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
handle_call({change_current_game, GNum}, _From, State) ->
    OldGNum = State#state.game_num,
    {Reply, S4} =
        if GNum /= OldGNum ->
                S2 = State#state{game_num = GNum,
                                 timer_minutes = ?undefined},
                {TReply, S3} = maybe_change_timer(S2),
                {{{game_num, OldGNum, GNum},
                  {timer, TReply}},
                 S3};
           true -> {same_game_num, State}
        end,
    {reply, Reply, S4};
handle_call(get_state, _From, State) ->
    {reply, state_as_kvs(State), State};
handle_call(get_ntp_offset, _From, State) ->
    {reply, State#state.ntp_offset_secs, State};
handle_call({set_timer_interval, N}, _From, State) ->
    {Reply, S2} = set_timer_interval(State, N),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call('stop', _From, State) ->
    timer:cancel(State#state.timer),
    S2 = stop_web(State),
    {stop, stopped, stop_reply, S2#state{timer = ?undefined}};
handle_call(?start_polling, _From, State) ->
    {Reply, S2} = maybe_change_timer(State#state{timer_minutes = ?undefined}),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call(?stop_polling, _From, State) ->
    timer:cancel(State#state.timer),
    {reply, {ok, polling_stopped}, State#state{timer = ?undefined,
                                               timer_minutes = ?stopped}};

handle_call(start_web, _From, State) ->
    S = start_web(State),
    {reply, ok, S};
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
handle_cast(Msg, State) ->
    ?dbg({unhandled_other, Msg}),
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
    mafia_data:downl_web(State#state.game_num, DL),
    regen_historyI(DL#dl.time, State#state.game_num),
    S2 = set_dl_timer(State, DL#dl.time),
    {noreply, S2};
handle_info(do_polling, State) ->
    {_Reply, S2} = maybe_change_timer(State),
    TimeStr = mafia_print:print_time(?console),
    if is_integer(State#state.game_num) ->
            io:format("~s poll for new messages\n", [TimeStr]),
            mafia_data:downl_web(State#state.game_num),
            flush(do_polling);
       true -> ok
    end,
    update_current(S2),
    {noreply, S2};
handle_info(check_ntp_offset, State) ->
    ?dbg({check_ntp_offset, time()}),
    %% should be called once every 10 min
    %% server 17.253.38.125, stratum 1, offset -0.015001, delay 0.02968
    spawn(fun() ->
                  NtpStr = os:cmd("ntpdate -q time.euro.apple.com"),
                  ?SERVER ! {ntp_offset_cmd_out, NtpStr}
          end),
    {noreply, State};
handle_info({ntp_offset_cmd_out, NtpStr}, State) ->
    %% ?dbg({ntp_offset_cmd_out, NtpStr}),
    Lines = string:tokens(NtpStr, "\n"),
    %% negative offset value means that the local clock is ahead
    Ms = [re:run(L, ".*offset ([-.0-9]*).*", [{capture, [1], list}])
          || L <- Lines],
    Offs = [list_to_float(FloatStr) || {match, [FloatStr]} <- Ms],
    NumOffVals = length(Offs),
    AvgOffset = lists:sum(Offs) / NumOffVals,
    ?dbg({ntp_offset_avg, NumOffVals, AvgOffset}),
    {noreply, State#state{ntp_offset_secs = AvgOffset}};
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
state_as_kvs(State) ->
    Fields = record_info(fields, state),
    Values = tl(tuple_to_list(State)),
    lists:zip(Fields, Values).

update_current(#state{game_num = GameNum,
                      timer_minutes = Minutes}) ->
    Phase = mafia_time:calculate_phase(GameNum),
    Opts = [{?game_key, GameNum},
            {?phase, Phase},
            {?period, Minutes}, %% mafia_time:timer_minutes(GameNum)
            {?use_time, mafia_time:utc_secs1970()}],
    update_current_txt(GameNum, Opts),
    update_current_html(GameNum, Phase, Opts),
    ok.

update_current2(Time, GNum) when is_integer(GNum) ->
    update_current2(Time, hd(?rgame(GNum)));
update_current2(Time, G = #mafia_game{game_num = GNum}) ->
    Phase = mafia_time:calculate_phase(G, Time),
    ?dbg({update_current2, GNum, Phase}),
    Opts = [{?game_key, GNum},
            {?phase, Phase}
           ],
    update_current_txt(GNum, Opts),
    update_current_html(GNum, Phase, Opts),
    ok.

update_current_txt(GameNum, Opts) when is_integer(GameNum) ->
    update_current_txt(?rgame(GameNum), Opts);
update_current_txt([G], Opts) ->
    %% update current html here too!
    FileName = mafia_file:game_phase_full_fn(G, ?current),
    write_text(FileName, Opts).

update_current_html(GameNum, Phase, Opts) when is_integer(GameNum) ->
    update_current_html(?rgame(GameNum), Phase, Opts);
update_current_html([G], Phase, Opts) ->
    FileName = mafia_file:game_phase_full_fn(?html, G, ?current),
    Title = ["Game Status ", mafia_print:print_phase(Phase)],
    write_html(FileName, Title, Opts).

%%--------------------------------------------------------------------
%% @doc Regenerate history text page
%% @end
%%--------------------------------------------------------------------

regen_historyI(Time, GNum)
  when is_integer(Time), is_integer(GNum) ->
    [G] = ?rgame(GNum),
    DL = mafia_time:get_prev_deadline(G, Time),
    regen_historyI(Time, GNum, DL#dl.phase, [G]);
regen_historyI(Time, {GKey, Phase = #phase{}}) ->
    regen_historyI(Time, GKey, Phase, ?rgame(GKey)).

regen_historyI(_, _, #phase{don = ?game_start}, _) -> ok;
regen_historyI(Time, GNum, Phase = #phase{}, [G]) ->
    ?dbg(Time, {"DO REGENERATE_HISTORY 3", Phase, Time}),
    Opts = [{?game_key, GNum},
            {?phase, Phase}],
    FNTxt = mafia_file:game_phase_full_fn(G, Phase),
    FNHtml= mafia_file:game_phase_full_fn(?html, G, Phase),
    regen_hist_txt(FNTxt, Opts),
    regen_hist_html(FNHtml, Phase, Opts),
    case G#mafia_game.game_end of
        ?undefined -> ok;
        _ ->
            ?dbg(Time, {"REGENERATE GAME_STATUS"}),
            Opts2 = [{?game_key, GNum},
                     {?phase, #phase{don = ?game_ended}}],
            FNTxt2 = mafia_file:game_phase_full_fn(G, ?current),
            FNHtml2 = mafia_file:game_phase_full_fn(?html, G, ?current),
            regen_hist_txt(FNTxt2, Opts2),
            regen_hist_html(FNHtml2, Phase, Opts2)
    end,
    ok.

regen_hist_txt(FNTxt, Opts) ->
    write_text(FNTxt, Opts).

regen_hist_html(FNHtml, Phase = #phase{}, Opts) ->
    Title = ["History ", mafia_print:print_phase(Phase)],
    write_html(FNHtml, Title, Opts).

%%--------------------------------------------------------------------

write_text(FileName, Opts) ->
    {ok, Fd} = file:open(FileName, [write]),
    Opts2 = Opts ++ [{?dev, Fd}],
    mafia_print:print_votes(Opts2),
    file:close(Fd).

write_html(FileName, Title, Opts) ->
    Opts2 = Opts ++ [{?mode, ?html}],
    {ok, Fd} = file:open(FileName, [write]),
    io:format(Fd, "~s", [get_html(Title, Opts2)]),
    file:close(Fd),
    ok.

get_html(Title, Opts) ->
    Body = mafia_print:print_votes(Opts),
    [?HTML_TAB_START(Title, " border=\"0\""),
     Body,
     ?HTML_TAB_END].

%%--------------------------------------------------------------------

get_interface_ip(IfName) ->
    ?dbg("Looking for IPv4 on interface " ++ IfName),
    case lists:dropwhile(
           fun("inet") -> false; (_) -> true end,
           %% string:tokens(os:cmd("ifconfig en1"), "\t\n\s"))).
           string:tokens(os:cmd("ifconfig " ++ IfName), "\t\n\s")) of
        L when length(L) >=2 ->
            lists:nth(2, L);
        _ ->
            throw("No IPv4 found on " ++ IfName)
    end.

bind_address() ->
    case mafia_lib:get_arg(?http_ip) of
        false ->
            case mafia_lib:get_arg(?http_interface) of
                false ->
                    %% default look for IPv4 on en1 (MacOS)
                    get_interface_ip("en1");
                {ok, IfName} ->
                    IP  = get_interface_ip(IfName),
                    ?dbg({interface_arg, IfName, use_ip, IP}),
                    IP
            end;
        {ok, IpAddress} ->
            ?dbg({ip_arg, IpAddress}),
            IpAddress
    end.

start_web(S) ->
    inets:start(),
    DocRoot = mafia_file:get_path(h_doc_root),
    SrvRoot = mafia_file:get_path(h_srv_root),
    CertDir = mafia_file:get_path(h_tls_dir),
    RepoDir = mafia_file:get_path(repo_dir),
    SearchForm = RepoDir ++ "/priv/search_form.html ",
    Index = RepoDir ++ "/priv/index.html ",
    Java = RepoDir ++ "/priv/java.html ",
    CurrVote = RepoDir ++ "/priv/current_vote.txt ",
    GmCmds = RepoDir ++ "/priv/GM_commands.html ",
    PlVote = RepoDir ++ "/priv/PlayerVoting.html ",
    os:cmd("cp " ++ SearchForm ++ DocRoot),
    os:cmd("cp " ++ Index ++ DocRoot),
    os:cmd("cp " ++ Java ++ DocRoot),
    os:cmd("cp " ++ CurrVote ++ DocRoot),
    os:cmd("cp " ++ GmCmds ++ DocRoot),
    os:cmd("cp " ++ PlVote ++ DocRoot),
    S2 = stop_web(S),
    IpAddr = bind_address(),
    CommonParams =
        [{server_name, "mafia.peterlund.se"},
         {server_root, SrvRoot},
         {document_root, DocRoot},
         {directory_index, ["java.html"]},
         {bind_address, any},
         {erl_script_alias, {"/e", [web]}},
         {error_log, "logs/error_log.txt"},
         {security_log, "logs/security_log.txt"},
         {transfer_log, "logs/transfer_log.txt"}],
    WebParams =
        [{port, ?WEBPORT}] ++ CommonParams,
    CertFN = filename:join(CertDir, "server.crt"),
    KeyFN = filename:join(CertDir, "server.key"),
    case inets:start(httpd, WebParams) of
        {ok, Pid} ->
            io:format("Started web server on ~s port ~p\n",
                      [IpAddr, ?WEBPORT]),
            case {file:read_file_info(CertFN),
                  file:read_file_info(KeyFN)} of
                {{ok, _}, {ok, _}} ->
                    SecureParams =
                        [{port, ?SECUREPORT},
                         {socket_type, {ssl, [{certfile, CertFN},
                                              {keyfile, KeyFN}]}}
                        ] ++ CommonParams,
                    case inets:start(httpd, SecureParams) of
                        {ok, TlsPid} ->
                            io:format("Started tls server on ~s "
                                      "port ~p\n",
                                      [IpAddr, ?SECUREPORT]),
                            S2#state{web_pid = Pid,
                                     tls_pid = TlsPid};
                        _ ->
                            S2#state{web_pid = Pid}
                    end;
                _ ->
                    S2#state{web_pid = Pid}
            end;
        Else ->
            io:format("Else ~p\n", [Else]),
            S2
    end.

stop_web(State) ->
    S2 = if State#state.web_pid /= undefined ->
                 ?dbg("Stop web server"),
                 inets:stop(httpd, State#state.web_pid),
                 State#state{web_pid = ?undefined};
            true ->
                 stop_httpd(),
                 State
         end,
    if S2#state.tls_pid /= undefined ->
            ?dbg("Stop tls server"),
            inets:stop(httpd, S2#state.tls_pid),
            S2#state{tls_pid = ?undefined};
       true ->
            stop_httpd(),
            S2
    end.

-spec maybe_change_timer(#state{}) -> {Reply::term(), #state{}}.
maybe_change_timer(S = #state{timer = TRef,
                              timer_minutes = TMins,
                              game_num = GNum}) ->
    Mins = mafia_time:timer_minutes(GNum),
    if TMins == ?stopped -> {no_change, S};
       Mins == none -> {cancelled, cancel_timer_interval(S)};
       TRef == ?undefined; Mins /= TMins ->
            set_timer_interval(S, Mins);
       true -> {no_change, S}
    end.

-define(_10minMs, 10 * 60 * 1000).
-define(_3minMs, 3 * 60 * 1000).

set_dl_timer(S) -> set_dl_timer(S, mafia_time:utc_secs1970()).

set_dl_timer(S, Time) ->
    ?dbg({set_dl_timer, 2}),
    set_dl_timer(S, Time, ?rgame(S#state.game_num)).

set_dl_timer(S, _Time, []) -> S;
set_dl_timer(S, Time, [G]) ->
    set_dl_timer(S, Time, G);
set_dl_timer(S, Time, G) ->
    ?dbg({set_dl_timer, 3}),
    S2 = cancel_dl_timer(S),
    case mafia_time:get_nxt_deadline(G, Time) of
        #dl{phase = #phase{don = ?game_ended}} -> S2;
        DL = #dl{} ->
            %% os:system_time() is "unixtime" with high precision
            NtpOffsetMilliSecs = round(S#state.ntp_offset_secs * 1000),
            RemMs = DL#dl.time * 1000
                - erlang:convert_time_unit(os:system_time(),
                                           native,
                                           millisecond)
                - NtpOffsetMilliSecs,
            set_dl_timer2(S2, DL, RemMs)
    end.

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

-spec set_timer_interval(#state{}, number()) -> {Reply :: term(), #state{}}.
set_timer_interval(S, TMins) when is_number(TMins), TMins > 0 ->
    S2 = cancel_timer_interval(S),
    {ok, TRef} = timer:send_interval(trunc(TMins * ?MINUTE_MS), do_polling),
    Reply = {interval_changed,
             {old, S2#state.timer_minutes},
             {new, TMins}},
    {Reply, S2#state{timer = TRef,
                     timer_minutes = TMins}}.

cancel_timer_interval(S) ->
    if S#state.timer /= ?undefined ->
            timer:cancel(S#state.timer);
       true -> ok
    end,
    flush(do_polling),
    S#state{timer = undefined}.

flush({Msg, X}) ->
    receive {Msg, _} -> flush({Msg, X})
    after 0 -> ok
    end;
flush(Msg) ->
    receive Msg -> flush(Msg)
    after 0 -> ok
    end.
