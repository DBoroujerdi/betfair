-module(betfair_connection).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([request/2]).
-export([set_owner/2]).

-define(SCOPE, l).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {token, connection, app_key, owner}).

%% TODO: Actually do something with the connection and request

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link(Opts, SessionToken) ->
    gen_server:start_link(?MODULE, [Opts, SessionToken], []).

-spec request(pid(), betfair_ops:rpc()) -> ok.
request(Pid, RpcBody) ->
    gen_server:cast(Pid, {request, RpcBody}).

-spec set_owner(pid(), pid()) -> {ok, reference()}.
set_owner(Pid, OwnerPid) ->
    gen_server:call(Pid, {owner_pid, OwnerPid}).


%%------------------------------------------------------------------------------
%% gen-server callbacks
%%------------------------------------------------------------------------------

init([Opts, Session]) ->
    SslOpts = proplists:get_value(ssl, Opts),
    Credentials = proplists:get_value(credentials, Opts),

    _ = lager:info("Connecting to the betfair api with token ~p", [Session]),
    Endpoint = proplists:get_value(exchange_endpoint, Opts),
    {ok, Connection} = gun:open(Endpoint, 443,
                                #{transport => ssl, transport_opts => SslOpts}),

    {ok, _} = gun:await_up(Connection),

    {ok, #state{token=Session,
                connection=Connection,
                app_key=app_key(Credentials)}}.

handle_call({owner_pid, Pid}, _From, State) ->
    Mref = erlang:monitor(process, Pid),
    {reply, {ok, Mref}, State#state{owner=Pid}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({request, RpcBody}, #state{token = Token,
                                       connection = Conn,
                                       app_key = Appkey} = State) ->
    _ = lager:info("Making request with body: ~p", [RpcBody]),
    #{method := Method} = RpcBody,
    Json = jsx:encode(RpcBody),
    lager:info("Body: ~p", [Json]),

    ReqHeaders = [{<<"X-Authentication">>, Token},
                  {<<"X-Application">>, Appkey}],
    _ = gun:post(Conn, "/exchange/betting/json-rpc/v1/" ++ Method, ReqHeaders, Json),

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gun_data, _Conn, _Stream, fin, Data},
            #{owner := _Owner} = State) ->
    lager:info("Response: ~p", [Data]),

    %% TODO: Send data on to message processor pool

    {no_reply, State};
handle_info({'DOWN', _MRef, process, _ServerPid, _Reason}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    lager:warning("Unexpected response: ~p", [Info]),
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
