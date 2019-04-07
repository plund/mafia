%%%-------------------------------------------------------------------
%%% @author Peter Lund
%%% @copyright (C) 2016, Peter Lund
%%% @doc Starts web and tls servers and keep track of NTP time
%%% 7 min to day deadline.
%%% @end
%%% Created : 10 Dec 2016 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(mafia_web).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,

         start_web/0,
         stop_httpd/0,

         get_state/0,
         get_ntp_offset/0
        ]).

%% test
-export([get_interface_ip/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mafia.hrl").

-define(SERVER, ?MODULE).

%% Feature:    Fields
%% --------    ------
%% start web:  web_pid
%% start tls:  tls_pid
%% ntp offset: offset_timer, ntp_offset_secs
-record(state,
        {web_pid :: ?undefined | pid(),
         tls_pid :: ?undefined | pid(),
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
    io:format(?MODULE_STRING ++ ":start_link/0\n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_web() ->
    io:format(?MODULE_STRING ++ ":start_web/0\n"),
    gen_server:call(?SERVER, start_web).

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?SERVER, 'stop').

stop_httpd() ->
    IpStr = bind_address(),
    case verify_telnet() of
        ?true ->
            RespStr = os:cmd("telnet " ++ IpStr ++ " " ++ ?i2l(?WEBPORT)),
            case string:str(RespStr, "Escape character is") of
                0 -> %% nothing running on port
                    io:format("Verify no webserver running at ~s, port ~p\n",
                              [IpStr, ?WEBPORT]);
                _ ->
                    stop_httpd(IpStr)
            end;
        ?false ->
            io:format("WARNING: the telnet command is not available\n"),
            stop_httpd(IpStr)
    end.

stop_httpd(IpStr) ->
    io:format("Stopping webserver running at ~s, port ~p\n", [IpStr, ?WEBPORT]),
    IP = list_to_tuple([?l2i(Str) || Str <- string:tokens(IpStr, ".")]),
    inets:stop(httpd, {IP, ?WEBPORT}).

%% Verify that telnet is available
verify_telnet() ->
    case string:str(os:cmd("telnet"), "command not found") of
        0 -> ?true;
        _ ->
            io:format("Please install the telnet command.\n"),
            ?false
    end.

%%--------------------------------------------------------------------
%% @doc Show gen_server internal state
%% @end
%%--------------------------------------------------------------------
get_state()  ->
    gen_server:call(?SERVER, get_state).

get_ntp_offset()  ->
    case ?getv(?ntp_offset_secs) of
        OffsetSecs when is_float(OffsetSecs) ->
            Sign = if OffsetSecs < 0 -> ?negative;
                      true -> ?positive
                   end,
            OffsMilliStr = float_to_list(abs(OffsetSecs * 1000),
                                         [{decimals, 2}]),
            {Sign, OffsMilliStr ++ " millisecs"};
        _ ->
            {?undefined, "Unknown NTP offset"}
    end.

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
    S0 = #state{},
    {ok, OffsetTimer} = init_ntp_offset(S0),
    State0 = #state{offset_timer = OffsetTimer},
    State = start_web(State0),
    {ok, State}.

init_ntp_offset(S) ->
    ?set(?ntp_offset_secs, S#state.ntp_offset_secs),
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
handle_call(get_state, _From, State) ->
    State2KVs =
        fun() ->
                Fields = record_info(fields, state),
                Values = tl(tuple_to_list(State)),
                lists:zip(Fields, Values)
        end,
    {reply, State2KVs(), State};
handle_call(get_ntp_offset, _From, State) ->
    {reply, State#state.ntp_offset_secs, State};
handle_call('stop', _From, State) ->
    S2 = stop_web(State),
    {stop, stopped, stop_reply, S2};
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
handle_info(check_ntp_offset, State) ->
    %% ?dbg({check_ntp_offset, time()}),
    %% should be called once every 10 min
    %% server 17.253.38.125, stratum 1, offset -0.015001, delay 0.02968
    spawn(fun() ->
                  NtpStr = os:cmd("ntpdate -q time.euro.apple.com"),
                  ?SERVER ! {ntp_offset_cmd_out, NtpStr}
          end),
    {noreply, State};
handle_info({ntp_offset_cmd_out, NtpStr}, State) ->
    Lines = string:tokens(NtpStr, "\n"),
    %% negative offset value means that the local clock is ahead
    Ms = [re:run(L, ".*offset ([-.0-9]*).*", [{capture, [1], list}])
          || L <- Lines],
    Offs = [list_to_float(FloatStr) || {match, [FloatStr]} <- Ms],
    NumOffVals = length(Offs),
    S2 = case NumOffVals of
             0 ->
                 State;
             _ ->
                 AvgOffset = lists:sum(Offs) / NumOffVals,
                 OffStr = float_to_list(AvgOffset * 1000, [{decimals, 2}]),
                 ?dbg({ntp_offset_avg, NumOffVals, OffStr}),
                 ?set(?ntp_offset_secs, AvgOffset),
                 State#state{ntp_offset_secs = AvgOffset}
         end,
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

bind_address() ->
    case mafia_lib:get_arg(?http_ip) of
        false ->
            case mafia_lib:get_arg(?http_interface) of
                false ->
                    get_ip_on_any_if(["en0", "en1", "eth0", "eth1"]);
                {ok, IfName} ->
                    IP  = get_interface_ip(IfName),
                    ?dbg({interface_arg, IfName, use_ip, IP}),
                    IP
            end;
        {ok, IpAddress} ->
            ?dbg({ip_arg, IpAddress}),
            IpAddress
    end.

get_ip_on_any_if([]) ->
    throw("No IPv4 found on any if");
get_ip_on_any_if([IfName | T]) ->
    case get_interface_ip(IfName) of
        {ok, IPv4} -> IPv4;
        {error, no_ipv4_found} ->
            get_ip_on_any_if(T)
    end.

get_interface_ip(IfName) ->
    ?dbg("Looking for IPv4 on interface " ++ IfName),
    case lists:dropwhile(
           fun("inet") -> false; (_) -> true end,
           string:tokens(os:cmd("ifconfig " ++ IfName), "\t\n\s")) of
        L when length(L) >=2 ->
            {ok, lists:nth(2, L)};
        _ ->
            {error, no_ipv4_found}
    end.

%% -----------------------------------------------------------------------------

start_web(S) ->
    inets:start(),
    DocRoot = mafia_file:get_path(h_doc_root),
    SrvRoot = mafia_file:get_path(h_srv_root),
    CertDir = mafia_file:get_path(h_tls_dir),
    RepoDir = mafia_file:get_path(repo_dir),

    RepoDocs = filename:join(RepoDir, "priv/docs/*"),
    CpDocCmd = "cp " ++ RepoDocs ++ " " ++ DocRoot,
    Resp = os:cmd(CpDocCmd),
    ?dbg({CpDocCmd, Resp}),

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
         {transfer_log, "logs/transfer_log.txt"},
         {erl_script_timeout, 60} %% http waits upto 60 secs
         %% "Refresh Vote Count" takes more that the default 15 secs
        ],
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
