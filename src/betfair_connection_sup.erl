-module(betfair_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Opts]) ->
    NumConn = proplists:get_value(num_conns, Opts),
    {ok, {{one_for_one, 1, 5}, procs(NumConn)}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

procs(NumConnections) ->
    [proc({betfair_connection, Id}, betfair_connection, permanent, []) ||
        Id <- lists:seq(1, NumConnections)].

proc(Id, Module, Restart, Args) ->
    betfair_sup:worker(Id, Module, Restart, Args).
