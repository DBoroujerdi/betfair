-module(betfair).

-export([start/0,
         make/0,
         get_opts/0,
         prop/1,
         new_session/0,
         new_stream/1,
         close_stream/1,
         subscribe/2,
         unsubscribe/2,
         request/2,
         request/3,
         keep_alive/1,
         get_env/1,
         get_env/2,
         get_env_bin/1,
         domain/1,
         credentials/0,
         ssl_options/0]).

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

-spec new_stream(binary()) -> {ok, pid()} | {error, term()}.
new_stream(Token) ->
    betfair_stream_sup:start_stream(Token).

close_stream(Stream) ->
    supervisor:terminate_child(betfair_stream_sup, Stream).

-spec subscribe(pid(), binary()) -> ok.
subscribe(Stream, Market) ->
    betfair_stream:add_market(Stream, Market).

-spec unsubscribe(pid(), binary()) -> ok | {error, term()}.
unsubscribe(Stream, Market) ->
    betfair_stream:remove_market(Stream, Market).

-spec new_session() -> {ok, binary()} | {error, term()}.
new_session() ->
    betfair_request:login().

-type sync_response() :: {betfair_response, binary()} | {error, term()}.
-type async_response() :: ok | {error, term()}.
-type response() :: sync_response() | async_response().

-type method() :: atom().
-type params() :: list(tuple()).
%% -type options() :: list(tuple()).

-spec request(atom(), binary()) -> response().
request(Method, Token) ->
    request(Method, Token, []).

-spec request(method(), binary(), params()) -> response().
request(Method, Token, Params) ->
    case betfair_rpc:check(Method, Params) of
        ok  ->
            Rpc = betfair_rpc:new(Method, Params),
            betfair_request:request(Method, Rpc, Token);
        Error -> {error, Error}
    end.

%% keep the session alive
keep_alive(_Session) ->
    %% TODO
    ok.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

ssl_options() ->
    get_env(ssl).

-spec credentials() -> [any()].
credentials() ->
    get_env(credentials).

-spec domain(atom()) -> string().
domain(identity) ->
    get_env(identity_endpoint);
domain(exchange) ->
    get_env(exchange_endpoint).

-spec get_env(atom()) -> list() | undefined.
get_env(Key) ->
    gproc:get_env(?SCOPE, ?MODULE, Key, [os_env, app_env]).

-spec get_env_bin(atom()) -> list() | undefined.
get_env_bin(Key) ->
    case get_env(Key) of
        Val when is_list(Val) ->
            list_to_binary(Val);
        Nope ->
            Nope
    end.

-spec get_env(atom(), any()) -> string() | undefined.
get_env(Key, Default) ->
    gproc:get_env(?SCOPE, ?MODULE, Key, [os_env, app_env, {default, Default}]).
