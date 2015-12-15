-module(betfair_session).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([get_token/0]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


%%--------------------------------------------------------------------
%% Types & Macros
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(SCOPE, l).

-type token() :: string().

-export_type([credentials/0]).
-export_type([token/0]).

-record(state, {credentials :: betfair:credentials(),
                connection :: pid(),
                token :: token(),
                keep_alive :: number()}).

-type credentials() :: #{username => string(),
                         password => string(),
                         app_key=> string()}.


%%%===================================================================
%%% API
%%%===================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

-spec get_token() -> string().
get_token() ->
    gen_server:call(?MODULE, token).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Opts]) ->
    Credentials = proplists:get_value(credentials, Opts),
    _ = lager:info("Credentials: ~p~n", [Credentials]),

    SslOpts = proplists:get_value(ssl, Opts),

    _ = lager:info("Connecting with ssl opts ~p~n", [SslOpts]),
    IdentityEndpoint = proplists:get_value(identity_endpoint, Opts),
    {ok, Connection} = gun:open(IdentityEndpoint, 443,
                                #{transport => ssl, transport_opts => SslOpts}),
    {ok, _} = gun:await_up(Connection),

    KeepAlive = get_keep_alive(Opts),
    _ = keep_alive(KeepAlive),

    {ok, #state{credentials = maps:from_list(Credentials),
                keep_alive = KeepAlive,
                connection = Connection}}.


handle_call(token, _From, #state{credentials=Credentials,
                                 connection=Connection} = State) ->
    #{app_key := Appkey} = Credentials,
    _ = lager:info("Logging in .."),
    ReqBody = betfair_http:url_encode(maps:without([app_key], Credentials)),
    ReqHeaders = [{<<"Content-Type">>, "application/x-www-form-urlencoded"},
                  {<<"X-Application">>, Appkey}],

    Stream = gun:post(Connection, "/api/certlogin", ReqHeaders, ReqBody),
    Reply = request(Connection, Stream),

    {reply, {ok, Reply}, update_state(Reply, State)};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(keep_alive, #state{token=Token,
                               connection=Connection,
                               keep_alive=Interval} = State) ->
    _ = lager:info("Keeping session ~p alive..", [Token]),

    Headers = [{<<"Accept">>, "application/json"},
               {<<"X-Application">>, <<"AppKey">>},
               {<<"X-Authentication">>, Token}],

    Stream = gun:get(Connection, "/api/keepAlive", Headers),
    {ok, Token} = request(Connection, Stream ),
    _ = keep_alive(Interval),
    {noreply, State};
handle_info(Info, State) ->
    _ = lager:warning("Unexpected message ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec request(pid(), reference()) -> {ok, token()} | any().
request(Connection, Stream) ->
    case gun:await(Connection, Stream) of
        {response, fin, _Status, _Headers} ->
            {error, no_data};
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(Connection, Stream),
            Response = jsx:decode(Body, [return_maps, {labels, atom}]),
            token(Response)
    end.

-spec update_state(Token, State) -> #state{} when
      Token :: {ok, token()} | any(),
      State :: #state{}.
update_state({ok, Token}, State) ->
    State#state{token = Token};
update_state(_, State) ->
    State.


-spec token(map()) -> {ok, token()} | {error, string()}.
token(#{sessionToken := Token, loginStatus := _Reason}) ->
    {ok, Token};
token(#{token := Token, status := <<"SUCCESS">>}) ->
    {ok, Token};
token(#{loginStatus := Reason}) ->
    {error, Reason};
token(#{error := Reason}) ->
    {error, Reason}.


-spec keep_alive(number()) -> reference().
keep_alive(Interval) ->
    erlang:send_after(Interval, self(), keep_alive).


-spec get_keep_alive(list(tuple())) -> number().
get_keep_alive(Opts) ->
    KeepAlive = proplists:get_value(keep_alive, Opts),
    keep_alive_millis(KeepAlive).

-spec keep_alive_millis(number()) -> number().
keep_alive_millis(KeepAliveHours) ->
    (1000 * 60 * 60) * KeepAliveHours.
