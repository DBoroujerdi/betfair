-module(betfair_stream_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_stream/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_stream(binary(), binary()) -> {ok, pid()} | {error, term()}.
start_stream(MarketId, Token) ->
    case supervisor:start_child(?SERVER, [MarketId, Token, self()]) of
        {ok, _P} = Pid -> Pid;
        {error, {already_started, Pid}} ->
            ok = betfair_stream:add_market(Pid, MarketId),
            {ok, Pid};
        {error, _Reason} = Error -> Error
    end.


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
          [{betfair_stream_sup,
            {betfair_stream, start_link, []},
            temporary, 1000, worker, [betfair_stream_sup]}
          ]}}.
