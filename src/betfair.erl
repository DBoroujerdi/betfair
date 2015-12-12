-module(betfair).

-export([start/0]).
-export([make/0]).
-export([get_env/0]).

-define(SCOPE, l).

make() ->
    make:all([load]).

start() ->
    application:ensure_all_started(?MODULE).

get_env() ->
    application:get_all_env(betfair).
