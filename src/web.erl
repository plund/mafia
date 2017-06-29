-module(web).

-include("mafia.hrl").

-export([search/3,
         game_status/3,
         msgs/3,
         msg/3,
         vote_tracker/3,
         stats/3,
         'forum.php'/3,
         dst_changes/3,
         game_settings/3,
         users/3,

         deliver/2
        ]).

-define(page(PageFun),
PageFun(Sid, Env, In) ->
    catch_debug(
      Sid, PageFun,
      fun(do) -> web_impl:PageFun(Sid, Env, In);
         (in) -> {PageFun, In}
      end)
).

%% forwards to web_impl module.
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

l2b(L) when is_list(L) -> ?l2b(L);
l2b(B) when is_binary(B) -> B.
