-module(web).

-include("mafia.hrl").

-export([search/3,
         game_status/3,
         msgs/3,
         msg/3,
         vote_tracker/3,
         stats/3,
         'forum.php'/3,
         users/3,
         deliver/2
        ]).

game_status(Sid, Env, In) ->
    catch_debug(
      Sid, game_status,
      fun(do) -> web_impl:game_status(Sid, Env, In);
         (in) -> {game_status, In}
      end).

search(Sid, Env, In) ->
    catch_debug(
      Sid, search,
      fun(do) -> web_impl:search(Sid, Env, In);
         (in) -> {search, In}
      end).

msgs(Sid, Env, In) ->
    catch_debug(
      Sid, msgs,
      fun(do) -> web_impl:msgs(Sid, Env, In);
         (in) -> {msgs, In}
      end).

msg(Sid, Env, In) ->
    catch_debug(
      Sid, msg,
      fun(do) -> web_impl:msg(Sid, Env, In);
         (in) -> {msg, In}
      end).

vote_tracker(Sid, Env, In) ->
    catch_debug(
      Sid, vote_tracker,
      fun(do) -> web_impl:vote_tracker(Sid, Env, In);
         (in) -> {vote_tracker, In}
      end).

stats(Sid, Env, In) ->
    catch_debug(
      Sid, stats,
      fun(do) -> web_impl:stats(Sid, Env, In);
         (in) -> {stat, In}
      end).

'forum.php'(Sid, Env, In) ->
    catch_debug(
      Sid, stats,
      fun(do) -> web_impl:forum_php(Sid, Env, In);
         (in) -> {stat, In}
      end).

users(Sid, Env, In) ->
    catch_debug(
      Sid, users,
      fun(do) -> web_impl:users(Sid, Env, In);
         (in) -> {users, In}
      end).

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

l2b(L) when is_list(L) -> ?l2b(L);
l2b(B) when is_binary(B) -> B.
