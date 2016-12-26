-module(web).

-include("mafia.hrl").

-export([msg_search_result/3,
         game_status/3,
         vote_tracker/3,
         stats/3,

         deliver/2,
         show_msg/1
        ]).

game_status(Sid, Env, In) ->
    ?dbg({game_status, In}),
    catch_debug(
      Sid, game_status,
      fun() -> mafia_web:game_status(Sid, Env, In) end).

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

%% ?html mode only, use mafia_print:pp for ?text
show_msg(MsgId) when is_integer(MsgId) ->
    show_msg(mafia:rmess(MsgId));

show_msg([]) -> "<tr><td>No message found with this id</td></tr>";
show_msg([#message{user_name = MsgUserB,
                   page_num = PageNum,
                   time = Time,
                   message = MsgB}]) ->
    GameKey = ?getv(?game_key),
    MsgPhase = mafia_time:calculate_phase(GameKey, Time),
    DayStr = case MsgPhase of
                 {DNum, ?day} -> "Day-" ++ ?i2l(DNum);
                 {DNum, ?night} -> "Night-" ++ ?i2l(DNum);
                 ?game_ended -> "Game End "
             end,
    Hash = erlang:phash2(MsgUserB, 16#1000000),
    Color = Hash bor 16#C0C0C0,
    ColorStr = integer_to_list(Color, 16),
    {HH, MM} = mafia_time:hh_mm_to_deadline(GameKey, Time),
    ?l2b(["<tr bgcolor=\"#", ColorStr,
          "\"><td valign=\"top\"><b>", MsgUserB,
          "</b><br>",
          DayStr, " ", p(HH), ":", p(MM),
          "<br> page ", ?i2l(PageNum),
          "</td><td valign=\"top\">", MsgB,
          "</td></tr>\r\n"]).

l2b(L) when is_list(L) -> ?l2b(L);
l2b(B) when is_binary(B) -> B.

p(I) when I > 9 -> ?i2l(I);
p(I) -> string:right(?i2l(I), 2, $0).
