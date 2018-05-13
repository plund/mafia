-module(web).

-include("mafia.hrl").

-export([front_page/3,
         game_status/3,
         search/3,
         msgs/3,
         msg/3,
         vote_tracker/3,
         stats/3,
         'forum.php'/3,
         dst_changes/3,
         game_settings/3,
         users/3,

         deliver/2,
         is_secure/1,
         host_info/1,
         msg_key2str/1,
         str2msg_key/1
        ]).

-define(page(PageFun),
PageFun(Sid, Env, In) ->
    catch_debug(
      Sid, PageFun,
      fun(do) -> (mod(PageFun)):PageFun(Sid, Env, In);
         (in) -> {PageFun, In}
      end)
).

mod(search) -> web_msgs;
mod(msgs) -> web_msgs;
mod(msg) -> web_msgs;
mod(_) -> web_impl.

%% forwards to web_impl module.
?page(front_page).
?page(game_status).
?page(search).
?page(msgs).
?page(msg).
?page(vote_tracker).
?page(stats).
?page('forum.php').
?page(dst_changes).
?page(users).
?page(game_settings).

catch_debug(Sid, Tag, F) ->
        TimeA = erlang:monotonic_time(millisecond),
        Req = F(in),
        ReqType = element(1, Req),
        case catch F(do) of
            {'EXIT', Term} ->
                ?dbg({catch_debug, {Tag, Term}}),
                deliver(Sid,
                        [?HTML_TAB_START(?a2l(Tag), ""),
                         "<tr><td>", "An error occured!", "</td></tr>",
                         ?HTML_TAB_END]);
            {NumBytes, Args} ->
                ?inc_cnt(total, reqs, 1),
                ?inc_cnt(total, ?bytes, NumBytes),
                ?inc_cnt(ReqType, reqs, 1),
                ?inc_cnt(ReqType, ?bytes, NumBytes),
                if Args /= ?none, Args /= [] ->
                        ?inc_cnt(ReqType, {reqs, Args}, 1),
                        ?inc_cnt(ReqType, {?bytes, Args}, NumBytes);
                   true -> ok
                end,
                TimeB = erlang:monotonic_time(millisecond),
                MilliSecs = TimeB - TimeA,
                ?dbg({Req, sent, [{bytes, NumBytes}, {millisecs, MilliSecs}]}),
                ok
        end.

deliver(Sid, Html) ->
    mod_esi:deliver(Sid, Html),
    size(l2b(Html)).

l2b(L) when is_list(L) -> unicode:characters_to_binary(L);
l2b(B) when is_binary(B) -> B.

-spec is_secure(Env) -> boolean() when
      Env :: proplists:proplist().
is_secure(Env) ->
    ?SECUREPORT == proplists:get_value(server_port, Env).

%% http_host is either "192.168.0.100:50667" or "mafia.peterlund.se"
-spec host_info(Env) -> {boolean(), Host, ScriptName} when
      Env :: proplists:proplist(),
      Host :: string(),
      ScriptName :: string().
host_info(Env) ->
    %% The following is present when forwarded from apache2 vhosts config
    %% {http_x_forwarded_host,"mafia.peterlund.se"} host used by client
    Host =
        case proplists:get_value(http_x_forwarded_host, Env) of
            ?undefined ->
                HttpHost = proplists:get_value(http_host, Env),
                case string:tokens(HttpHost, ":") of
                    [H, _P] -> H;
                    [H] -> H
                end;
            H -> H
        end,
    Tokens = string:tokens(Host, "."),
    ScriptName = proplists:get_value(script_name, Env),
    IsNum = fun(Str) ->
                    case catch ?l2i(Str) of
                        {'EXIT', _} -> ?false;
                        _ -> ?true
                    end
            end,
    {lists:all(IsNum, Tokens), Host, ScriptName}.

msg_key2str({MsgId, ?webDip}) -> ":" ++ ?i2l(MsgId);
msg_key2str({MsgId, ?vDip}) -> "v:" ++ ?i2l(MsgId);
msg_key2str({MsgId, ?wd2}) -> "w:" ++ ?i2l(MsgId).

str2msg_key(Str) ->
    case mafia_lib:split_on_first_char(Str, $:) of
        {"", IntStr} ->
            {?l2i(IntStr), ?webDip};
        {"v", IntStr} ->
            {?l2i(IntStr), ?vDip};
        {"w", IntStr} ->
            {?l2i(IntStr), ?wd2};
        _ ->
            ?dbg({error, not_msg_id, Str}),
            {error, not_msg_id}
    end.
