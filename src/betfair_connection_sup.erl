-module(betfair_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 1, 5}, procs()}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

procs() ->
    [proc({betfair_connection, Id}, betfair_connection, permanent, []) ||
        Id <- lists:seq(1, 10)].

proc(Id, Module, Restart, Args) ->
    betfair_sup:worker(Id, Module, Restart, Args).
