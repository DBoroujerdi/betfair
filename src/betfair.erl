-module(betfair).

-export([start/0,
         make/0,
         get_opts/0,
         prop/1,
         request/1,
         request/2,
         request/3]).

-define(SCOPE, l).
-define(TIMEOUT, 5000). %% TODO: pass in as option


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

make() ->
    make:all([load]).

start() ->
    application:ensure_all_started(?MODULE).

-spec get_opts() -> list(tuple()).
get_opts() ->
    application:get_all_env(?MODULE).

-spec prop(atom()) -> list().
prop(Name) ->
    proplists:get_value(Name, get_opts()).

-type method() :: atom().
-type market_filters() :: list(tuple()).

-type sync_response() :: {betfair_response, binary()} | {error, term()}.
-type async_response() :: ok | {error, term()}.
-type response() :: sync_response() | async_response().

-spec request(method()) -> response().
request(Method) ->
    request(Method, []).

-spec request(method(), market_filters()) -> response().
request(Method, MFilters) ->
    request(Method, MFilters, []).

-spec request(method(), market_filters(), list(tuple())) -> response().
request(Method, MFilters, Opts) ->
    case betfair_rpc:check_filters(MFilters) of
        ok    -> case betfair_rpc:is_valid_method(Method) of
                     true  -> do_request(Method, MFilters, Opts);
                     _     -> {invalid_method, Method}
                 end;
        Error -> {incorrect_filter, Error}
    end.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec do_request(atom(), list(tuple()), list(tuple())) ->
                        {error, term()} | binary().
do_request(Method, MFilters, [{sync, true}]) ->
    sync_request(Method, MFilters, self());
do_request(Method, MFilters, []) ->
    async_request(Method, MFilters).

-spec sync_request(atom(), list(tuple()), pid()) -> {error, any()} | binary().
sync_request(Method, MFilters, Caller) ->
    case pooler:take_member(connection_pool) of
        Pid when is_pid(Pid) ->
            Rpc = betfair_rpc:new(Method, MFilters),
            ok = betfair_connection:request(Pid, Rpc),
            _ = pooler:return_member(connection_pool, Pid, ok),
            receive
                {betfair_response, Caller, Response} ->
                    Response;
                Unknown ->
                    {unknown_response, Unknown}
            after ?TIMEOUT ->
                    {error, timeout}
            end;
        Error -> {error, Error}
    end.

-spec async_request(atom(), list(tuple())) -> {error, any()} | ok.
async_request(Method, MFilters) ->
    case pooler:take_member(connection_pool) of
        Pid when is_pid(Pid) ->
            Rpc = betfair_rpc:new(Method, MFilters),
            betfair_connection:request(Pid, Rpc),
            _ = pooler:return_member(connection_pool, Pid, ok);
        Error -> {error, Error}
    end.
