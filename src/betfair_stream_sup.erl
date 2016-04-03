-module(betfair_stream_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_stream/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_stream(Token) ->
    supervisor:start_child(?SERVER, [Token, self()]).


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
          [{betfair_stream_sup,
            {betfair_stream, start_link, []},
            temporary, 1000, worker, [betfair_stream_sup]}
          ]}}.
