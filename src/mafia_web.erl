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
         regenerate_history/1,  % used to print txt files when refresh_votes
         regenerate_history/2,
         update_current/0
        ]).

%% deprecated
-export([set_interval_minutes/1]).

%% test
-export([get_en1_ip/0
         %%is_word/2, allpos/2, find_word_searches/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mafia.hrl").

-record(state,
        {timer :: reference(),
         timer_minutes :: integer(),
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

start() ->
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

start_polling() -> gen_server:call(?SERVER, 'start_polling').
stop_polling() -> gen_server:call(?SERVER, 'stop_polling').

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
%% @doc Change the poll timer
%%      Old do not work since we go on schedule now.
%% @end
%%--------------------------------------------------------------------
set_interval_minutes(N) when is_integer(N)  ->
    gen_server:call(?SERVER, {set_timer_interval, N}).

%% rewrite one history txt file
regenerate_history(Time, DL) when ?IS_DL(DL) ->
    Phase = ?dl2phase(DL),
    ?dbg(Time, {"REGENERATE_HISTORY", Phase}),
    regenerate_historyI(Phase);
regenerate_history(Time, Phase) when ?IS_PHASE(Phase) ->
    ?dbg(Time, {"REGENERATE_HISTORY", Phase}),
    regenerate_historyI(Phase).

regenerate_history(Phase) when ?IS_PHASE(Phase) ->
    ?dbg({"REGENERATE_HISTORY", Phase}),
    regenerate_historyI(Phase).

regenerate_historyI(Phase) ->
    gen_server:cast(?SERVER, {regenerate_history, Phase}).

update_current() ->
        gen_server:cast(?SERVER, update_current).

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
    if Arg == polling ->
            ?dbg(start_polling),
            self() ! do_polling,
            {_Reply, S2} = set_timer_interval(State, 10),
            {ok, S2};
       Arg == no_polling ->
            ?dbg(start_no_polling),
            {ok, State}
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
handle_call('start_polling', _From, State) ->
    {Reply, S2} = maybe_change_timer(State),
    self() ! do_polling,
    {reply, Reply, S2};
handle_call('stop_polling', _From, State) ->
    timer:cancel(State#state.timer),
    {reply, {ok, polling_stopped}, State#state{timer = ?undefined}};

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

%% rewrite one history txt file
handle_cast(Ev = {regenerate_history, {DNum, DoN}}, State) ->
    timer:sleep(300),
    case ?rgame(State#state.game_key) of
        [] -> ok;
        [G] ->
            FileName = mafia_file:game_phase_full_fn(G, {DNum, DoN}),
            {ok, Fd} = file:open(FileName, [write]),
            mafia_print:print_votes([{?game_key, State#state.game_key},
                                     {?phase, {DNum, DoN}},
                                     {?dev, Fd}]),
            file:close(Fd),
            flush({'$gen_cast', Ev})
    end,
    {noreply, State};
handle_cast(update_current, State) ->
    update_current(State#state.game_key, State#state.timer_minutes),
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
                 GameKey, ?game_ended),
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
    case mafia_time:timer_minutes(ThId) of
        Mins when is_integer(Mins), Mins /= TMins ->
            set_timer_interval(S, Mins);
        Mins when TRef == ?undefined ->
            set_timer_interval(S, Mins);
        _ -> {no_change, S}
    end.

-spec set_timer_interval(#state{}, integer()) -> {Reply :: term(), #state{}}.
set_timer_interval(S, N) when is_integer(N), N >= 1 ->
    if S#state.timer /= ?undefined ->
            timer:cancel(S#state.timer),
            flush(do_polling);
       true -> ok
    end,
    {ok, TRef} = timer:send_interval(N * ?MINUTE_MS, do_polling),
    Reply = {interval_changed,
             {old, S#state.timer_minutes},
             {new, N}},
    {Reply, S#state{timer = TRef,
                    timer_minutes = N}}.

flush(Msg) ->
    receive Msg -> flush(Msg)
    after 0 -> ok
    end.
