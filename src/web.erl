-module(web).

-include("mafia.hrl").

-export([msg_search_result/3,
         vote_tracker/3,
         stats/3,

         deliver/2
        ]).

msg_search_result(Sid, Env, In) ->
    ?dbg({msg_search_result, In}),
    catch_debug(
      Sid, msg_search_result,
      fun() -> mafia_web:msg_search_result(Sid, Env, In) end).

vote_tracker(Sid, Env, In) ->
    ?dbg({vote_tracker, In}),
    catch_debug(
      Sid, vote_tracker,
      fun() -> mafia_web:vote_tracker(Sid, Env, In) end).

stats(Sid, Env, In) ->
    ?dbg({stat, In}),
    catch_debug(
      Sid, stats,
      fun() -> mafia_web:stats(Sid, Env, In) end).

catch_debug(Sid, Tag, F) ->
    TimeA = erlang:monotonic_time(millisecond),
    case catch F() of
        {'EXIT', Term} ->
            Str = lists:flatten(io_lib:format("~p\n", [{Tag, Term}])),
            ?dbg_str(Str),
            deliver(Sid,
                    [?HTML_TAB_START(?a2l(Tag), ""),
                     "<tr><td>", "An error occured!", "</td></tr>",
                     ?HTML_TAB_END]);
        NumBytes ->
            TimeB = erlang:monotonic_time(millisecond),
            MilliSecs = TimeB - TimeA,
            ?dbg({sent, [{bytes, NumBytes}, {millisecs, MilliSecs}]}),
            ok
    end.

deliver(Sid, Html) ->
    mod_esi:deliver(Sid, Html),
    size(l2b(Html)).

l2b(L) when is_list(L) -> ?l2b(L);
l2b(B) when is_binary(B) -> B.
