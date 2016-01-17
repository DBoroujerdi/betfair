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

-export_type([method/0]).
-export_type([params/0]).

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


-type sync_response() :: {betfair_response, binary()} | {error, term()}.
-type async_response() :: ok | {error, term()}.
-type response() :: sync_response() | async_response().

-type method() :: list_event_types.
-type params() :: list(tuple()).
-type options() :: list(tuple()).

-spec request(atom()) -> response().
request(Method) ->
    request(Method, []).

-spec request(method(), params()) -> response().
request(Method, Params) ->
    request(Method, Params, []).

-spec request(method(), params(), options()) -> response().
request(Method, Params, Opts) ->
    case betfair_rpc:check_params(Params) of
        ok    -> case betfair_rpc:is_valid_method(Method) of
                     true  -> do_request(Method, Params, Opts);
                     _     -> {invalid_method, Method}
                 end;
        Error -> {incorrect_filter, Error}
    end.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec do_request(atom(), list(tuple()), list(tuple())) ->
                        {error, term()} | binary().
do_request(Method, Params, [{sync, true}]) ->
    sync_request(Method, Params, self());
do_request(Method, Params, []) ->
    async_request(Method, Params).

-spec sync_request(atom(), list(tuple()), pid()) -> {error, any()} | binary().
sync_request(Method, Params, Caller) ->
    case pooler:take_member(connection_pool) of
        Pid when is_pid(Pid) ->
            Rpc = betfair_rpc:new(Method, Params),
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
async_request(Method, Params) ->
    case pooler:take_member(connection_pool) of
        Pid when is_pid(Pid) ->
            Rpc = betfair_rpc:new(Method, Params),
            betfair_connection:request(Pid, Rpc),
            _ = pooler:return_member(connection_pool, Pid, ok);
        Error -> {error, Error}
    end.
