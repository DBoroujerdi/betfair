-module(betfair_session).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([login/0]).
-export([get_token/0]).
-export([update_token/1]).
-export([keep_alive/0]).

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

-export_type([credentials/0]).
-export_type([token/0]).

-record(state, {credentials::betfair:credentials(),
                connection, token, keep_alive}).

-type credentials() :: #{username => string(),
                         password => string(),
                         app_key=> string()}.


%%%===================================================================
%%% API
%%%===================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

login() ->
    gen_server:call(?MODULE, login).

get_token() ->
    gen_server:call(?MODULE, token).

update_token(Token) ->
    gen_server:cast(?MODULE, {token, Token}).

keep_alive() ->
    betfair_session ! keep_alive.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Opts]) ->
    lager:info("Starting new session"),
    Credentials = proplists:get_value(credentials, Opts),
    lager:info("Credentials: ~p~n", [Credentials]),

    SslOpts = proplists:get_value(ssl, Opts),

    lager:info("Connecting with ssl opts ~p~n", [SslOpts]),
    {ok, _} = gun:open("identitysso.betfair.com", 443,
                       #{transport => ssl, transport_opts => SslOpts}),

    %% {ok, #state{credentials=Credentials, keep_alive = 10000}}.
    {ok, #state{credentials=maps:from_list(Credentials), keep_alive = 1000 * 60 * 60 * 60}}.


handle_call(token, _From, #state{token=Token} = State) ->
    {reply, Token, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({token, Token}, State) ->
    lager:info("Updating token with ~p", [Token]),

    {noreply, State#state{token=Token}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({token, Token}, #state{token=Token} = State) when is_binary(Token) ->
    lager:info("Updating with token ~p", [Token]),
    gproc_ps:publish(?SCOPE, session_token, Token),
    {noreply, State#state{token=Token}};
handle_info({token, Token}, State) ->
    lager:info("Updating with first token with ~p", [Token]),
    gproc_ps:publish(?SCOPE, session_token, Token),
    {noreply, State#state{token=Token}};
handle_info(keep_alive, #state{token=Token, connection=Connection,
                               credentials=Credentials} = State) ->
    _ = lager:info("Keeping session ~p alive..", [Token]),

    #{app_key := Appkey} = Credentials,
    Headers = [{<<"Content-Type">>, "application/json"},
                  {<<"X-Application">>, Appkey},
                  {<<"X-Authentication">>, Token}],

    Stream = gun:get(Connection, "/api/keepAlive", Headers),
    _ = receive_response(self(), Stream, Connection, fun keep_alive_handler/1),
    _ = keep_alive(State#state.keep_alive),
    {noreply, State};
handle_info(login, #state{credentials=Credentials,
                          connection=Connection} = State) ->
    #{app_key := Appkey} = Credentials,
    lager:info("Logging in"),
    ReqBody = betfair_http:url_encode(maps:without([app_key], Credentials)),
    ReqHeaders = [{<<"Content-Type">>, "application/x-www-form-urlencoded"},
                  {<<"X-Application">>, Appkey}],
    Stream = gun:post(Connection, "/api/certlogin", ReqHeaders, ReqBody),
    _ = receive_response(self(), Stream, Connection, fun login_handler/1),
    _ = keep_alive(State#state.keep_alive),
    {noreply, State};
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

receive_response(Self, Stream, Connection, Handler) ->
    receive
        {gun_response, Connection, Stream, fin, _Status, _Headers} ->
            no_data;
        {gun_response, Connection, Stream, nofin, _Status, _Headers} ->
            %% TODO only receive if 200 status?
            receive_data(Self, Connection, Stream, Handler);
        {'DOWN', _, process, Connection, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after 1000 ->
            exit(timeout)
    end.

receive_data(Self, Connection, Stream, Handler) ->
    receive
        {gun_data, _Connection, _Stream, nofin, _Data} ->
            receive_data(Self, Connection, Stream, Handler);
        {gun_data, _Connection, Stream, fin, Data} ->
            lager:info("Received Data ~p", [Data]),
            Response = jsx:decode(Data, [return_maps, {labels, atom}]),
            Self ! Handler(Response);
        {'DOWN', _, process, _Connection, Reason} ->
            _ = lager:error("Oops!"),
            exit(Reason)
    after 1000 ->
            exit(timeout)
    end.


login_handler(#{sessionToken := Token, loginStatus := _Reason}) ->
    {token, Token};
login_handler(#{loginStatus := Reason}) ->
    exit(Reason).

keep_alive_handler(#{token := Token, status := <<"SUCCESS">>}) ->
    {token, Token};
keep_alive_handler(#{status := <<"SUCCESS">>, error := Reason}) ->
    exit(Reason).


keep_alive(KeepAliveInterval) ->
    erlang:send_after(KeepAliveInterval, self(), keep_alive).
