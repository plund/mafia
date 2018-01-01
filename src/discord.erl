%%%-------------------------------------------------------------------
%%% @author Peter Lund <peter@liber>
%%% @copyright (C) 2017, Peter Lund
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2017 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(discord).

-behaviour(gen_server). %% websocket_client

%% API
-export([discord_client/0,
         read_discord_bot_registration_data/0,
         get_reg_val/1,
         decode_json/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%-compile(export_all).
-export([test/0,
         close/1,
         channel_arg/0,
         msgs_arg/0,
         msgs_arg2/0,
         channel_id/0
        ]).

-include("../include/mafia_atoms.hrl").

-define(SERVER, ?MODULE).

-record(state,
        {
          channel_id,
          last_read,
          host,
          socket,
          buffer = []
        }).

%%%===================================================================
%%% API
%%%===================================================================

test() ->
    ssl:start(),
    %% {ok, { StatusLine={_,_,_}, Headers = [], Resp :: string()  }} =
    httpc:request("https://discordapp.com/api/gateway"),
    %% request() :: {url(), headers()}
    Url = "https://discordapp.com/api" ++ msgs_arg(),
    Headers = [{"Host", api_host()},
               {"Authorization", "Bot " ++ token()},
               {"User-Agent", "DiscordBot MafiaBot/1.0"}],
    case httpc:request(get, {Url, Headers}, [], []) of
        {ok, {_StatusLine, _Headers, Body}} ->
            decode_json_body(Body)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
read_discord_bot_registration_data() ->
    {ok, L} = file:consult("discord_bot_registration_data.txt"),
    Store =
        fun({Key, "replace me"}) ->
                io:format("Warning: '~p' does not have any value\n", [Key]);
           ({Key, Value}) ->
                put(Key, Value)
        end,
    ok = lists:foreach(Store, L).

get_reg_val(Key) ->
    %% crash if undefined
    case get(Key) of
        Value when Value /= ?undefined ->
            Value
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
    S = #state{
      %% channel_id,
      %% last_read,
      %% host,
      %% socket,
      %% buffer = []
     },
    {ok, S}.

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
%%handle_info({ssl, Socket, Data}, State) ->
%%handle_info({ssl_closed, Socket}, State) ->
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

discord_client() ->
    ssl:start(),
    {ok, Socket} = connect(),
    GetReq = get_req(""),
    ssl:send(Socket, GetReq).

connect() ->
    ssl:connect(api_host(), 443, [], infinity).

close(Socket) ->
    ssl:close(Socket).

get_req(Args) ->
    "GET /api" ++ Args ++ " HTTP/1.1\r\n"
        "Host: " ++ api_host() ++  "\r\n"
        "Authorization: Bot " ++ token() ++ "\r\n"
        "User-Agent: DiscordBot MafiaBot/1.0\r\n"
        "\r\n".


%% GET /channels/{channel.id}
channel_arg() -> "/channels/" ++ channel_id().


%% GET/channels/{channel.id}/messages
msgs_arg() -> "/channels/" ++ channel_id() ++ "/messages".

msgs_arg2() -> "/channels/" ++ channel_id() ++
                  "/messages?after=305217028451729409".

channel_id() -> "303952298948821003".

api_host() -> "discordapp.com".

token() -> get_reg_val(discord_client_token).

%% -----------------------------------------------------------------------------
%% @doc Decodes a full json response (headers and all chunks in one string)

decode_json(Str) ->
    {_Head, Body} = split(Str, "\r\n\r\n"),
    %% io:format("Body ~p\n", [Body]),
    {ChSizeStr, Chunks} = split(Body, "\r\n"),
    ChSize = list_to_integer(ChSizeStr, 16),
    %% io:format("ChSize ~p\n", [ChSize]),
    %% io:format("Chunk0 ~p\n", [Chunks]),
    Ch = string:substr(Chunks, 1, ChSize),
    %% io:format("Chunk1 ~p\n", [Ch1]),
    %% {Ch, Rest} = split(Chunks, "\r\n0\r\n\r\n"),
    %% io:format("Rest ~p\n", [Rest]),
    %% io:format("Chunk ~p\n", [Ch]),
    decode_json_body(Ch).

decode_json_body(Body) ->
    {ok, TokensOpaque, _} = erl_scan:string(Body),
    %% io:format("Token ~p\n", [Tokens]),
    Tokens =
        lists:foldr(fun({Cat, Anno}, Acc) ->
                            [{Cat, erl_anno:to_term(Anno)} | Acc];
                       ({Cat, Anno, Symb}, Acc) ->
                            [{Cat, erl_anno:to_term(Anno), Symb} | Acc]
                    end,
                    [],
                    TokensOpaque),
    Ts2 = replace_curly(Tokens),
    %% io:format("curly ~p\n", [Ts2]),
    Ts3 = replace_colon(Ts2),
    %% io:format("colon ~p\n", [Ts3]),
    {ok, Ts4} = erl_parse:parse_term(Ts3 ++ [{dot,1}]),
    mk_atom_keys(Ts4).

split(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {Str, ""};
        P ->
            Len = length(Search),
            {string:left(Str, P - 1),
             lists:nthtail(P - 1 + Len, Str)}
    end.

replace_curly(L) ->
    F = fun({'{', 1}) -> {'[', 1};
           ({'}', 1}) -> {']', 1};
           (T) -> T
        end,
    [F(T) || T <- L].

replace_colon([T1, {':', 1} | TT]) ->
    {Right, Rest} = get_right(TT, 0, []),
    [{'{', 1}, T1, {',', 1}] ++ replace_colon(Right) ++ [{'}', 1}]
        ++ replace_colon(Rest);
replace_colon([H | TT]) -> [H | replace_colon(TT)];
replace_colon([]) -> [].

get_right([H = {'[', 1} | T], N, Acc) ->
    get_right(T, N + 1, [H | Acc]);
get_right([H = {']', 1} | T], N, Acc) when N == 1 ->
    {lists:reverse([H | Acc]), T};
get_right([H = {']', 1} | T], N, Acc) ->
    get_right(T, N - 1, [H | Acc]);
get_right([H | T], 0, []) ->
    {[H], T};
get_right(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
get_right([H | T], N, Acc) ->
    get_right(T, N, [H | Acc]).

mk_atom_keys([{Str, Val} | T]) when is_list(Str) ->
    [{list_to_atom(Str), mk_atom_keys(Val)} | mk_atom_keys(T)];
mk_atom_keys([H | T]) -> [mk_atom_keys(H) | mk_atom_keys(T)];
mk_atom_keys([]) -> [];
mk_atom_keys(Val) -> Val.
