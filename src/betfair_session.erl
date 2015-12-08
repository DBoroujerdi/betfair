-module(betfair_session).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([login/0]).
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

-define(SERVER, ?MODULE).
-define(SCOPE, l).

-type token() :: string().

-export_type([token/0]).

-record(state, {credentials::betfair:credentials(), connection, token}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug, [trace, log]}]).

login() ->
    gen_server:call(?MODULE, login).

get_token() ->
    gen_server:call(?MODULE, token).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    lager:info("Starting new session"),
    {ok, Credentials} = betfair:get_credentials(),
    lager:info("Credentials: ~p~n", [Credentials]),
    {ok, SslOpts} = betfair:get_ssl_opts(),

    lager:info("Connecting with ssl opts ~p~n", [SslOpts]),
    {ok, _} = gun:open("identitysso.betfair.com", 443,
                       #{transport => ssl, transport_opts => SslOpts}),

    {ok, #state{credentials=Credentials}}.

handle_call(token, _From, #state{token=Token} = State) ->
    {reply, Token, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(login,
            #state{credentials=Credentials, connection=Connection} = State) ->
    #{app_key := Appkey} = Credentials,
    lager:info("Logging in"),
    ReqBody = betfair_http:url_encode(maps:without([app_key], Credentials)),
    ReqHeaders = [{<<"Content-Type">>, "application/x-www-form-urlencoded"},
                  {<<"X-Application">>, Appkey}],
    _ = gun:post(Connection, "/api/certlogin", ReqHeaders, ReqBody),
    {noreply, State};
handle_info({gun_data, _Connection, _StreamRef, nofin, Data}, State) ->
    lager:info("Received Data, ~p", [Data]),
    {noreply, State};
handle_info({gun_data, _Connection, _StreamRef, fin, Data}, State) ->
    lager:info("Received Data, ~p", [Data]),

    case maybe_handle(Data) of
        {ok, Token} ->
            gproc_ps:publish(?SCOPE, session_token, Token),
            {noreply, State#state{token = Token}};
        {error, Reason} ->
            lager:error("Error login response: ~p", [Reason]),
            {noreply, State}
    end;
handle_info({gun_up, Connection, _Transport}, State) ->
    lager:info("Connection established .."),
    self() ! login,
    {noreply, State#state{connection = Connection}};
handle_info({gun_response, _Connection, _StreamRef, nofin, _Status, _}, State) ->
    lager:info("Headers received .."),
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Unexpected message ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_handle(Response) when is_binary(Response) ->
    Decoded = jsx:decode(Response, [return_maps]),
    maybe_handle(Decoded);
maybe_handle(#{<<"sessionToken">> := Token}) ->
    {ok, Token};
maybe_handle(#{<<"loginStatus">> := Reason}) ->
    {error, Reason}.
