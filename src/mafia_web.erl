%%%-------------------------------------------------------------------
%%% @author Peter Lund <peter@liber>
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
         start_web/0,
         stop/0,
         stop_polling/0,
         stop_httpd/1,
         set_interval_minutes/1,
         regenerate_history/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(mafia, [i2l/1]).

-include("mafia.hrl").

-define(SERVER, ?MODULE).
-define(WEBPORT, 50666).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").
%% ServerRoot is relative to running path.
-define(SERVER_ROOT, "/Users/peter/httpd/mafia.peterlund.se").
%% DocumentRoot is relative to running path
-define(DOC_ROOT, "/Users/peter/httpd/mafia.peterlund.se/html").

-record(state,
        {timer :: reference(),
         timer_minutes :: integer(),
         web_pid :: pid()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_web() ->
    gen_server:call(?SERVER, start_web).

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?SERVER, 'stop').

stop_polling() -> gen_server:call(?SERVER, 'stop_polling').

stop_httpd(a) -> inets:stop(httpd, {{192,168,0,100}, ?WEBPORT});
stop_httpd(b) -> inets:stop(httpd, {{192,168,0,3}, ?WEBPORT}).

%%--------------------------------------------------------------------
%% @doc Change the poll timer
%% @end
%%--------------------------------------------------------------------
set_interval_minutes(N) when is_integer(N)  ->
    gen_server:call(?SERVER, {set_timer_interval, N}).

regenerate_history({DNum, DoN, _}) ->
    regenerate_history({DNum, DoN});
regenerate_history(Phase) ->
    gen_server:cast(?SERVER, {regenerate_history, Phase}).

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
init([]) ->
    mafia:setup_mnesia(),
    State = start_web(#state{}),
    {_Reply, S2} = set_timer_interval(State, 10),
    self() ! check_all,
    {ok, S2}.

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
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({set_timer_interval, N}, _From, State) ->
    {Reply, S2} = set_timer_interval(State, N),
    self() ! check_all,
    {reply, Reply, S2};
handle_call('stop', _From, State) ->
    timer:cancel(State#state.timer),
    inets:stop(httpd, State#state.web_pid),
    {stop, stopped, stop_reply, State#state{timer = undefined,
                                            web_pid = undefined}};
handle_call('stop_polling', _From, State) ->
    timer:cancel(State#state.timer),
    {reply, ok, State#state{timer = undefined}};

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
handle_cast({regenerate_history, {DNum, DoN}}, State) ->
    case mafia:rgame() of
        [] -> ok;
        [G] ->
            Prefix = mafia_data:game_file_prefix(G),
            PhStr = case DoN of
                        ?day -> "d";
                        ?night -> "n"
                    end ++ i2l(DNum),
            %% calculate "m25_d1.txt"
            PhaseFN = Prefix ++ PhStr ++ ".txt",
            FileName = filename:join(?DOC_ROOT, PhaseFN),
            {ok, Fd} = file:open(FileName, [write]),
            mafia_print:print_votes(DNum, DoN, [{fd, Fd}]),
            file:close(Fd)
    end,
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
handle_info(check_all, State) ->
    io:format("check_all ~p\n", [time()]),
    mafia_data:downl(),
    FileName = filename:join(?DOC_ROOT, "current_vote.txt"),
    S2 = maybe_change_timer(State),
    {ok, Fd} = file:open(FileName, [write]),
    mafia_print:print_votes([{fd, Fd}, {next, S2#state.timer_minutes}]),
    file:close(Fd),
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

start_web(S) ->
    inets:start(),
    maybe_create_dir(?SERVER_ROOT),
    maybe_create_dir(?DOC_ROOT),
    IP_en1 =
        lists:nth(2, lists:dropwhile(
                       fun("inet") -> false; (_) -> true end,
                       string:tokens(os:cmd("ifconfig en1"), "\t\n\s"))),
    io:format("Starting up a webserver listening on ~s\n", [IP_en1]),
    {ok, Pid} =
        inets:start(httpd,
                    [{port, ?WEBPORT},
                     {server_name, ?SERVER_NAME},
                     {server_root, ?SERVER_ROOT},
                     {document_root, ?DOC_ROOT},
                     {bind_address, IP_en1}]),
    S#state{web_pid = Pid}.

%% must create dirs first.
%% mkdir -p /Users/peter/httpd/mafia.peterlund.se/html
maybe_create_dir(Dir) ->
    case file:read_file_info(Dir) of
        {error,enoent} ->
            os:cmd("mkdir -p " ++ Dir),
            io:format("Created ~s\n", [Dir]);
        _ -> ok
    end.

-spec maybe_change_timer(#state{}) -> #state{}.
maybe_change_timer(S = #state{timer_minutes = TMins}) ->
    case mafia_time:timer_minutes() of
        Mins when is_integer(Mins), Mins /= TMins ->
            {_, S2} = set_timer_interval(S, Mins),
            S2;
        _ -> S
    end.

-spec set_timer_interval(#state{}, integer()) -> {Reply :: term(), #state{}}.
set_timer_interval(S, N) when is_integer(N), N >= 1 ->
    if S#state.timer /= undefined ->
            timer:cancel(S#state.timer),
            flush(check_all);
       true -> ok
    end,
    {ok, TRef} = timer:send_interval(N * ?MINUTE_MS, check_all),
    Reply = {interval_changed,
             {old, S#state.timer_minutes},
             {new, N}},
    {Reply, S#state{timer = TRef,
                    timer_minutes = N}}.

flush(Msg) ->
    receive Msg -> flush(Msg)
    after 0 -> ok
    end.
