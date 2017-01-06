-module(web).

-include("mafia.hrl").

-export([msg_search_result/3,
         game_status/3,
         vote_tracker/3,
         stats/3,

         deliver/2
        ]).

game_status(Sid, Env, In) ->
    catch_debug(
      Sid, game_status,
      fun(do) -> web_impl:game_status(Sid, Env, In);
         (in) -> {game_status, In}
      end).

msg_search_result(Sid, Env, In) ->
    catch_debug(
      Sid, msg_search_result,
      fun(do) -> web_impl:msg_search_result(Sid, Env, In);
         (in) -> {msg_search_result, In}
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

catch_debug(Sid, Tag, F) ->
        TimeA = erlang:monotonic_time(millisecond),
        Req = F(in),
        ReqType = element(1, Req),
        ?inc_cnt(ReqType),
        case catch F(do) of
            {'EXIT', Term} ->
                ?dbg({catch_debug, {Tag, Term}}),
                deliver(Sid,
                        [?HTML_TAB_START(?a2l(Tag), ""),
                         "<tr><td>", "An error occured!", "</td></tr>",
                         ?HTML_TAB_END]);
            {NumBytes, Args} ->
                ?inc_cnt(ReqType, ?bytes, NumBytes),
                if Args /= ?none ->
                        ?inc_cnt(ReqType,
                                 {?bytes, Args},
                                 NumBytes);
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
