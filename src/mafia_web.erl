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
         start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE).
-define(WEBPORT, 6666).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").
%% ServerRoot is relative to running path.
-define(SERVER_ROOT, "/Users/peter/httpc/mafia.peterlund.se").
%% DocumentRoot is relative to running path
-define(DOC_ROOT, "/Users/peter/httpc/mafia.peterlund.se/html").

-record(state,
        {timer :: reference(),
         web_pid :: pid()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

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
    State = start_web(#state{}),
    self() ! check_all,
    {ok, TRef} = timer:send_interval(10 * ?MINUTE_MS, check_all),
    {ok, State#state{timer = TRef}}.

start_web(S) ->
    inets:start(),
    %% must create dir first:
    %% mkdir -p /Users/peter/httpc/mafia.peterlund.se/html
    os:cmd("mkdir -p " ++ ?SERVER_ROOT),
    os:cmd("mkdir -p " ++ ?DOC_ROOT),
    {ok, Pid} =
        inets:start(
          httpd,
          [{port, ?WEBPORT},
           {server_name, ?SERVER_NAME},
           {server_root, ?SERVER_ROOT},
           {document_root, ?DOC_ROOT},
           {bind_address, "localhost"}]),
    S#state{web_pid = Pid}.

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
    mafia_data:downl(),
    %% {ok, Fd} = file:open("test",[write]),
    _NewStatusPage = mafia_print:print_votes(),
    %% file:close(Fd)
    {noreply, State};
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
