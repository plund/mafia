%%%-------------------------------------------------------------------
%% @doc mafia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mafia_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {#{strategy => one_for_all,
            intensity => 1,
            period => 5},
          %%{one_for_all, 0, 1},
          [
           %% #{id => mnesia,
           %%   start => {application, start, [mnesia]}
           %%  },
           %% #{id => inets,
           %%   start => {inets, start, [permanent]}
           %%  },
           #{id => mafia_web,
             start => {mafia_web, start_link, []},
             restart => permanent %% (default)
            }
          ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
