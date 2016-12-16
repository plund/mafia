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
         get_state/0,
         set_interval_minutes/1,
         regenerate_history/1
        ]).

-export([msg_search_result/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(mafia, [i2l/1, l2b/1, b2l/1, l2u/1, getv/1]).

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
%% @doc Show gen_server internal state
%% @end
%%--------------------------------------------------------------------
get_state()  ->
    gen_server:call(?SERVER, get_state).

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
handle_call(get_state, _From, State) ->
    Fields = record_info(fields, state),
    Values = tl(tuple_to_list(State)),
    {reply, lists:zip(Fields, Values), State};
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
    os:cmd("cp ../priv/search_form.html " ++ ?DOC_ROOT),
    IP_en1 =
        lists:nth(2, lists:dropwhile(
                       fun("inet") -> false; (_) -> true end,
                       string:tokens(os:cmd("ifconfig en1"), "\t\n\s"))),
    io:format("Starting up a webserver listening on ~s\n", [IP_en1]),
    os:cmd("cp mafia_web.beam "++ ?SERVER_ROOT),
    os:cmd("cp mafia_web.beam "++ ?DOC_ROOT),
    case inets:start(httpd,
                     [{port, ?WEBPORT},
                      {server_name, "mafia_test.peterlund.se"},
                      {server_root, ?SERVER_ROOT},
                      {document_root, ?DOC_ROOT},
                      {bind_address, IP_en1},
%%% specifying modules removes the default list in where
%%% mod_esi and mod_dir already are included by default
%%                       {modules, [
%%                                  %% mod_alias,
%%                                  %% mod_auth,
%% %%% mod_esi - http://mafia_test.peterlund.se/esi/mafia_web/msg_search_result
%%                                  mod_esi,
%%                                  %% mod_actions,
%%                                  %% mod_cgi, %mod_include,
%% %%% mod_dir - browse directories
%%                                  mod_dir
%%                                  %% mod_get,
%%                                  %% mod_head, mod_log, mod_disk_log
%%                                 ]},
                      {erl_script_alias, {"/esi", [mafia_web, io]}}
                     ]) of
        {ok, Pid} ->
            S#state{web_pid = Pid};
        Else ->
            io:format("Else ~p\n", [Else]),
            S
    end.

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

%% http://mafia_test.peterlund.se/esi/mafia_web/msg_search_result
msg_search_result(Sid, _Env, In) ->
    ThId = getv(thread_id),
    PQ = httpd:parse_query(In),
    mod_esi:deliver(Sid, ["Content-type: text/html\r\n",
                          "Transfer-Encoding: chunked\r\n",
                          "\r\n"]),
    {_, UsersText} = lists:keyfind("user names", 1, PQ),
    {_, WordsText} = lists:keyfind("contained words", 1, PQ),
    {_, DayNumText} = lists:keyfind("day numbers", 1, PQ),
    DayCond =
        try
            case string:tokens(DayNumText, "-") of
                [LoStr, HiStr] ->
                    {list_to_integer(string:strip(LoStr)),
                     list_to_integer(string:strip(HiStr))};
                [Str] ->
                    Num = list_to_integer(string:strip(Str)),
                    {Num, Num};
                _ -> all
            end
        catch _:_ -> all
        end,
    UsersU = [l2u(U) || U <- string:tokens(UsersText, " ")],
    WordsU = [l2u(W) || W <- string:tokens(WordsText, " ")],

    IsDayCondSingle = case DayCond of {DNumC, DNumC} -> true; _ -> false end,
    IsUserCond = UsersU /= [],
    DoCont = if IsUserCond -> true;
                not IsUserCond, IsDayCondSingle -> true;
                true -> false
             end,
    Fun =
        fun(acc, init) -> 0;
           (#message{user_name = UserB,
                     page_num = PageNum,
                     time = Time,
                     message = MsgB},
            Acc) ->
                {DNum, DoN} = mafia_time:calculate_phase(ThId, Time),
                Msg = b2l(MsgB),
                TestFuns =
                    [
                     %% 1. Test UserB only if UsersU /= []
                     fun() ->
                             UsersU == [] orelse
                                 lists:member(l2u(b2l(UserB)), UsersU)
                     end,

                     %% 2. Test Words with ANY instead of all
                     fun() ->
                             MsgU = l2u(Msg),
                             lists:all(
                               fun(WordU) ->
                                       case string:str(MsgU, WordU) of
                                           0 -> false;
                                           _ -> true
                                       end
                               end,
                               WordsU)
                     end,

                     %% 3. Test Day
                     fun() ->
                             case DayCond of
                                 all -> true;
                                 {NLo, NHi}
                                   when NLo =< DNum,
                                        DNum =< NHi -> true;
                                 _ -> false
                             end
                     end],
                AllTestsOk = lists:all(fun(F) -> F() end, TestFuns),
                if AllTestsOk ->
                        DayStr = case DoN of
                                     ?day -> "D";
                                     ?night -> "N"
                                 end
                            ++ i2l(DNum),
                        MsgBr = lists:foldr(
                                  fun($\n, Acc2) -> "<br>" ++ Acc2;
                                     (Ch, Acc2) -> [Ch | Acc2]
                                  end,
                                  "",
                                  Msg),
                        Hash = erlang:phash2(UserB, 16#1000000),
                        Color = Hash bor 16#C0C0C0,
                        ColorStr = integer_to_list(Color, 16),
                        OutB = l2b(["<tr bgcolor=\"#", ColorStr,
                                    "\"><td valign=\"top\">", UserB, " ",
                                    "p", i2l(PageNum), ", ", DayStr,
                                    "</td><td valign=\"top\">", MsgBr,
                                    "</td></tr>\r\n"]),
                        mod_esi:deliver(Sid, OutB),
                        Acc + size(OutB);
                   true -> Acc
                end;
           (_, Acc) ->
                Acc
        end,
    TimeA = erlang:monotonic_time(millisecond),
    A = del_start(Sid),
    B = if DoCont ->
                mafia_data:iterate_all_msgs(ThId, Fun);
           true ->
                MsgB = l2b(["<tr><td valign=\"top\">",
                            "Error: Minimum one user needs to be "
                            "specified or a single day number.",
                            "</td></tr>"]),
                mod_esi:deliver(Sid, MsgB),
                size(MsgB)
        end,
    C = del_end(Sid),
    TimeB = erlang:monotonic_time(millisecond),
    NumBytes = A + B + C,
    MilliSecs = TimeB - TimeA,
    io:format("Sent ~p bytes in ~p millisecs, search=~s|~s|~s|\n",
              [NumBytes, MilliSecs, UsersText, WordsText, DayNumText]).

-define(RES_START, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>Mafia Result</title>
  </head>
  <body bgcolor=\"#cfffaf\">
    <center>
      <h3>Mafia Search Result</h3>
      <table border=\"1\">").

-define(RES_END, "
      </table>
    </center>
  </body>
</html>").

del_start(Sid) -> mod_esi:deliver(Sid, ?RES_START), size(l2b(?RES_START)).
del_end(Sid) -> mod_esi:deliver(Sid, ?RES_END), size(l2b(?RES_END)).
