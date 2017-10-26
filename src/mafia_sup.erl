%%%-------------------------------------------------------------------
%% @doc mafia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

%% Supervision tree (one_for_one)
%%
%% +----+
%% |1.MS|
%% +----+
%%   |
%%   +------+-----+----------+
%%   |      |     |          |
%% +---+  +---+  +---+     +---+
%% |Web|  |g21|  |g22| ... |g32|
%% +---+  +---+  +---+     +---+
%%
%% Let Web start and add all siblings G1 ...

-module(mafia_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start/1]).

%% Supervisor callbacks
-export([init/1]).

-include("mafia.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(GNum) ->
    supervisor:start_child(?SERVER, game_child_specs(GNum)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    io:format(?MODULE_STRING ++ ":init()\n"),
    mafia:setup_mnesia(),
    io:format(?MODULE_STRING ++ " mnesia setup\n"),
    {ok, {#{strategy => one_for_one,
            intensity => 20,
            period => 4},
          all_child_specs()
         }}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Child :: #{id, start, restart, shutdown, type, modules}
all_child_specs() ->
    [#{id => mafia_web,
       start => {mafia_web, start_link, []},
       restart => permanent
      }
     | all_game_child_specs()].

all_game_child_specs() ->
    [game_child_specs(GNum) || GNum <- mafia_lib:all_keys(mafia_game)].

game_child_specs(GNum) ->
    Id = ?l2a("game_" ++ ?i2l(GNum)),
    #{id => Id,
      start => {game, start_link, [Id, GNum]},
      restart => permanent
     }.
