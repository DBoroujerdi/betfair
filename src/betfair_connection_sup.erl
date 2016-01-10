-module(betfair_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    PoolerSup = {pooler_sup, {pooler_sup, start_link, []},
                 permanent, infinity, supervisor, [pooler_sup]},
    {ok, {{one_for_one, 5, 10}, [PoolerSup]}}.
