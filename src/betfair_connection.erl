-module(betfair_connection).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([request/1]).

-define(SCOPE, l).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(EMPTY_BIN, <<>>).

-record(state, {token, connection, app_key, callers, data}).


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec request(map()) -> ok.
request(RpcBody) ->
    gen_server:cast(?MODULE, {request, self(), RpcBody}).


%%------------------------------------------------------------------------------
%% gen-server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    Opts = betfair:get_opts(),
    SslOpts = proplists:get_value(ssl, Opts),
    Credentials = proplists:get_value(credentials, Opts),
    {ok, Session} = betfair_session_token_store:get_token(),

    _ = lager:info("Connecting to the betfair api with token ~p", [Session]),
    Endpoint = proplists:get_value(exchange_endpoint, Opts),
    {ok, Connection} = gun:open(Endpoint, 443,
                                #{transport => ssl, transport_opts => SslOpts}),

    {ok, _} = gun:await_up(Connection),

    {ok, #state{token=Session,
                connection=Connection,
                app_key=app_key(Credentials),
                callers=#{},
                data=?EMPTY_BIN}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({request, Caller, Rpc}, #state{token = Token,
                                           connection = Conn,
                                           app_key = Appkey,
                                           callers = Callers} = State) ->
    Json = jsx:encode(Rpc),
    ReqHeaders = [{<<"X-Authentication">>, Token},
                  {<<"X-Application">>, Appkey}],
    Method = proplists:get_value(method, Rpc),
    Url = "/exchange/betting/json-rpc/v1/" ++ binary_to_list(Method),

    _ = lager:info("Sending Json to url ~p: ~p", [Url, Json]),

    Stream = gun:post(Conn, Url, ReqHeaders, Json),
    {noreply, State#state{callers = Callers#{Stream => Caller}}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gun_response, _, _, nofin, 200, _}, State) ->
    {noreply, State};
handle_info({gun_response, _, _, fin, Status, Headers}, State) ->
    Reason = betfair_http:handle_status(Status, Headers),
    {stop, Reason, State};
handle_info({gun_data, _, _, nofin, Part}, #state{data = Acc} = State) ->
    {noreply, State#state{data = <<Acc/binary, Part/binary>>}};
handle_info({gun_data, _, Stream, fin, Part},
            #state{data = Acc, callers = Callers} = State) ->
    Data = <<Acc/binary, Part/binary>>,
    case maps:find(Stream, Callers) of
        {ok, Caller} -> Caller ! {betfair_response, Caller, Data},
                        {noreply, State#state{data = <<>>,
                                              callers = maps:remove(Stream, Callers)}};
        error        -> {stop, error, State}
    end;
handle_info(Info, State) ->
    _ = lager:warning("Unexpected response: ~p", [Info]),
    {noreply, State}.
terminate(_Reason, #state{connection=Connection}) ->
    gun:shutdown(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

app_key(Props) ->
    proplists:get_value(app_key, Props).
