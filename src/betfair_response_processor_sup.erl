-module(betfair_response_processor_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([Opts]) ->
    NumProcs = proplists:get_value(num_procs, Opts),
    {ok, {{one_for_one, 1, 5}, procs(NumProcs)}}.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

procs(NumProcs) ->
    [proc({betfair_response_processor, Id},
          betfair_response_processor, permanent, []) ||
        Id <- lists:seq(1, NumProcs)].

proc(Id, Module, Restart, Args) ->
    betfair_sup:worker(Id, Module, Restart, Args).
