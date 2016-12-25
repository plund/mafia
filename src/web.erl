-module(web).

-include("mafia.hrl").

-export([msg_search_result/3,
         vote_tracker/3,
         stats/3]).

msg_search_result(Sid, Env, In) ->
    ?dbg({msg_search_result, In}),
    mafia_web:msg_search_result(Sid, Env, In).

vote_tracker(Sid, Env, In) ->
    ?dbg({vote_tracker, In}),
    mafia_web:vote_tracker(Sid, Env, In).

stats(Sid, Env, In) ->
    ?dbg({stat, In}),
    mafia_web:stats(Sid, Env, In).
