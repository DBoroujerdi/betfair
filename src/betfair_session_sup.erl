-module(betfair_session_sup).

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

init([]) ->
    {ok, {{one_for_all, 1, 5}, [
                                betfair_sup:worker(betfair_session, permanent, []),
                                betfair_sup:supervisor(betfair_connection_sup, permanent, [])
                               ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
