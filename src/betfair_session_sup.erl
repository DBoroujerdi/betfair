-module(betfair_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Opts]) ->
    {ok, {{one_for_all, 1, 5}, [
                                betfair_sup:worker(betfair_session, permanent, [Opts]),
                                betfair_sup:supervisor(betfair_connection_sup, permanent, [Opts])
                               ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
