-module(web).

-export([msg_search_result/3,
         vote_tracker/3,
         stats/3]).

msg_search_result(Sid, Env, In) ->
    mafia_web:msg_search_result(Sid, Env, In).

vote_tracker(Sid, _Env, In) ->
    mafia_web:vote_tracker(Sid, _Env, In).

stats(Sid, _Env, In) ->
    mafia_web:stats(Sid, _Env, In).
