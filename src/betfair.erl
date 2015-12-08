-module(betfair).

-export([start/0]).
-export([make/0]).
-export([get_credentials/0]).
-export([get_ssl_opts/0]).

-export_type([credentials/0]).

-define(SCOPE, l).

make() ->
    make:all([load]).

start() ->
    application:ensure_all_started(?MODULE).

-type credentials() :: #{username => string(),
                        password => string(),
                        app_key=> string()}.

-spec get_credentials() -> undefined | {ok, credentials()}.
get_credentials() ->
    {ok, Credentials} = application:get_env(betfair, credentials),
    {ok, maps:from_list(Credentials)}.

-spec get_ssl_opts() -> undefined | {ok, list(tuple())}.
get_ssl_opts() ->
    application:get_env(betfair, ssl).
