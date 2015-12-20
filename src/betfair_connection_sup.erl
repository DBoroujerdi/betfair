-module(betfair_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([start_connection/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link(Opts, Session) when is_binary(Session) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts, Session]).

start_connection() ->
    supervisor:start_child(?SERVER, []).


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([Opts, Session]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [#{id => betfair_connection,
                    start => {betfair_connection, start_link, [Opts, Session]},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.
