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

         do_regen_hist/2,
         update_current/0
        ]).

%% test
-export([get_en1_ip/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mafia.hrl").

-record(state,
        {timer :: reference(),
         timer_minutes :: integer(),
         dl_timer :: reference(),
         dl_time :: seconds1970(),
         web_pid :: pid(),
         game_key :: thread_id()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    mafia:setup_mnesia(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [polling], []).

start() ->  %% to be removed?
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
    IpStr = get_en1_ip(),
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

%%--------------------------------------------------------------------
%% @doc Update current text page
%% @end
%%--------------------------------------------------------------------
update_current() ->
    gen_server:cast(?SERVER, update_current).

%%--------------------------------------------------------------------
%% @doc Regenerate history text page
%% @end
%%--------------------------------------------------------------------
do_regen_hist(Time, GKey)
  when is_integer(Time), is_integer(GKey) ->
    [G] = ?rgame(GKey),
    DL = mafia_time:get_prev_deadline(G, Time),
    do_regen_hist(Time, {G, DL#dl.phase});
do_regen_hist(Time, {G, Phase = #phase{}}) ->
    ?dbg(Time, {"DO REGENERATE_HISTORY 3", Phase, Time}),
    FileName = mafia_file:game_phase_full_fn(G, Phase),
    {ok, Fd} = file:open(FileName, [write]),
    mafia_print:print_votes([{?game_key, G#mafia_game.key},
                             {?phase, Phase},
                             {?dev, Fd}]),
    file:close(Fd),
    ok.

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
    State = start_web(#state{game_key = GameKey}),
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
handle_call(get_state, _From, State) ->
    Fields = record_info(fields, state),
    Values = tl(tuple_to_list(State)),
    {reply, lists:zip(Fields, Values), State};
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
    update_current(State#state.game_key, State#state.timer_minutes),
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
handle_info({?deadline, DL}, State) ->
    %% THIS work well when not refreshing!! Should start after...
    %% While refreshing we need to look at message timestamps in
    %% order to see when we have reached/passed the next deadline
    do_regen_hist(DL#dl.time, State#state.game_key),
    S2 = set_dl_timer(State, DL#dl.time),
    {noreply, S2};
handle_info(do_polling, State) ->
    TimeStr = mafia_print:print_time(?console),
    io:format("~s poll for new messages\n", [TimeStr]),
    mafia_data:downl_web(State#state.game_key),
    {_Reply, S2} = maybe_change_timer(State),
    flush(do_polling),
    update_current(S2#state.game_key, S2#state.timer_minutes),
    {noreply, S2};
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

update_current(GameKey, Minutes) ->
    update_currentI(GameKey, [{?period, Minutes}]).

update_currentI(GameKey, UpdateOpts) ->
    FileName = mafia_file:game_phase_full_fn(
                 GameKey, ?current),
    {ok, Fd} = file:open(FileName, [write]),
    Phase = mafia_time:calculate_phase(GameKey),
    mafia_print:print_votes([{?game_key, GameKey},
                             {?phase, Phase},
                             {?dev, Fd},
                             {?use_time, mafia_time:utc_secs1970()}
                            ] ++ UpdateOpts),
    file:close(Fd).

get_en1_ip() ->
    lists:nth(2, lists:dropwhile(
                   fun("inet") -> false; (_) -> true end,
                   string:tokens(os:cmd("ifconfig en1"), "\t\n\s"))).

start_web(S) ->
    inets:start(),
    DocRoot = mafia_file:get_path(h_doc_root),
    SrvRoot = mafia_file:get_path(h_srv_root),
    RepoDir = mafia_file:get_path(repo_dir),
    SearchForm = RepoDir ++ "/priv/search_form.html ",
    Index = RepoDir ++ "/priv/index.html ",
    CurrVote = RepoDir ++ "/priv/current_vote.txt ",
    GmCmds = RepoDir ++ "/priv/GM_commands.html ",
    PlVote = RepoDir ++ "/priv/PlayerVoting.html ",
    os:cmd("cp " ++ SearchForm ++ DocRoot),
    os:cmd("cp " ++ Index ++ DocRoot),
    os:cmd("cp " ++ CurrVote ++ DocRoot),
    os:cmd("cp " ++ GmCmds ++ DocRoot),
    os:cmd("cp " ++ PlVote ++ DocRoot),
    IP_en1 = get_en1_ip(),
    Params = [{port, ?WEBPORT},
              {server_name, "mafia.peterlund.se"},
              {server_root, SrvRoot},
              {document_root, DocRoot},
              {directory_index, ["index.html"]},
              {bind_address, IP_en1},
%%% specifying modules removes the default list in where
%%% mod_esi and mod_dir already are included by default
%%                       {modules, [
%%                                  %% mod_alias,
%%                                  %% mod_auth,
%% %%% mod_esi - http://mafia.peterlund.se/esi/mafia_web/msg_search_result
%%                                  mod_esi,
%%                                  %% mod_actions,
%%                                  %% mod_cgi, %mod_include,
%% %%% mod_dir - browse directories
%%                                  mod_dir
%%                                  %% mod_get,
%%                                  %% mod_head, mod_log, mod_disk_log
%%                                 ]},
              {erl_script_alias, {"/e", [web]}},
              {error_log, "logs/error_log.txt"},
              {security_log, "logs/security_log.txt"},
              {transfer_log, "logs/transfer_log.txt"}
             ],
    S2 = stop_web(S),
    io:format("Starting up a webserver listening on ~s port ~p\n",
              [IP_en1, ?WEBPORT]),
    case inets:start(httpd, Params) of
        {ok, Pid} ->
            S2#state{web_pid = Pid};
        Else ->
            io:format("Else ~p\n", [Else]),
            S2
    end.

stop_web(State) ->
    if State#state.web_pid /= undefined ->
            inets:stop(httpd, State#state.web_pid),
            State#state{web_pid = ?undefined};
       true ->
            stop_httpd(),
            State
    end.

-spec maybe_change_timer(#state{}) -> {Reply::term(), #state{}}.
maybe_change_timer(S = #state{timer = TRef,
                              timer_minutes = TMins,
                              game_key = ThId}) ->
    Mins = mafia_time:timer_minutes(ThId),
    if TMins == ?stopped -> {no_change, S};
       Mins == none -> {cancelled, cancel_timer_interval(S)};
       TRef == ?undefined; Mins /= TMins ->
            set_timer_interval(S, Mins);
       true -> {no_change, S}
    end.

set_dl_timer(S) -> set_dl_timer(S, mafia_time:utc_secs1970()).

set_dl_timer(S, Time) ->
    set_dl_timer(S, Time, ?rgame(S#state.game_key)).

set_dl_timer(S, _Time, []) -> S;
set_dl_timer(S, Time, [G]) ->
    set_dl_timer(S, Time, G);
set_dl_timer(S, Time, G) ->
    S2 = cancel_dl_timer(S),
    case mafia_time:get_nxt_deadline(G, Time) of
        #dl{phase = #phase{don = ?game_ended}} -> S2;
        DL = #dl{} ->
            SecsRem = DL#dl.time - Time,
            {ok, TRef} =
                timer:send_after(SecsRem * 1000, {?deadline, DL}),
            S2#state{dl_timer = TRef,
                     dl_time = DL#dl.time}
    end.

cancel_dl_timer(S) ->
    if is_reference(S#state.dl_timer) ->
            timer:cancel(S#state.dl_timer);
       true -> ok
    end,
    flush({?deadline, any}),
    S#state{dl_timer = undefined}.

-spec set_timer_interval(#state{}, integer()) -> {Reply :: term(), #state{}}.
set_timer_interval(S, N) when is_integer(N), N >= 1 ->
    S2 = cancel_timer_interval(S),
    {ok, TRef} = timer:send_interval(N * ?MINUTE_MS, do_polling),
    Reply = {interval_changed,
             {old, S2#state.timer_minutes},
             {new, N}},
    {Reply, S2#state{timer = TRef,
                     timer_minutes = N}}.

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
