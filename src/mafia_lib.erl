-module(mafia_lib).

-export([dl2phase/1]).

-include("mafia.hrl").

dl2phase({?game_ended, _Time}) -> ?game_ended;
dl2phase({Num, Don, _Time}) -> {Num, Don}.

