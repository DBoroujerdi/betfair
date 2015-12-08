-module(betfair_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([supervisor/1]).
-export([supervisor/2]).
-export([supervisor/3]).
-export([supervisor/4]).

-export([worker/1]).
-export([worker/2]).
-export([worker/3]).
-export([worker/4]).

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
                                supervisor(betfair_session_sup, permanent, [])
                               ]}}.


%%====================================================================
%% Internal functions
%%====================================================================

supervisor(Module) ->
    supervisor(Module, permanent).

supervisor(Module, Restart) ->
    supervisor(Module, Restart, []).

supervisor(Module, Restart, Args) ->
    supervisor(Module, Module, Restart, Args).

supervisor(Id, Module, Restart, Args) ->
    child_spec(Id, Module, Restart, infinity, supervisor, Args).

worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Args) ->
    worker(Module, Module, Restart, Args).

worker(Id, Module, Restart, Args) ->
    child_spec(Id, Module, Restart, 5000, worker, Args).

child_spec(Id, Module, Restart, Shutdown, Type, Args) ->
    {Id, {Module, start_link, Args}, Restart, Shutdown, Type, [Module]}.
