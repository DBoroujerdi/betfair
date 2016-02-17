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
    case betfair_rpc:check(Method, Params) of
        ok  -> make_request(Method, Params, Opts);
        Error -> {incorrect_filter, Error}
    end.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

make_request(list_market_book, _Params, _Opts) ->
    %% rate limit using Jobs
    ok;
make_request(Method, Params, Opts) ->
    do_request(Method, Params, Opts).

-spec do_request(atom(), list(tuple()), list(tuple())) ->
                        {error, term()} | binary().
do_request(Method, Params, [{sync, true}]) ->
    ok = betfair_connection:request(betfair_rpc:new(Method, Params)),
    Self = self(),
    receive
        {betfair_response, Self, Response} ->
            Response;
        Unknown ->
            {unknown_response, Unknown}
    after ?TIMEOUT ->
            {error, timeout}
    end;
do_request(Method, Params, []) ->
    betfair_connection:request(betfair_rpc:new(Method, Params)).
