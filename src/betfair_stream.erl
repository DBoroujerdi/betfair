-module(betfair_stream).

-behaviour(gen_server).

%% API
-export([start_link/3,
         add_market/2]).

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

-record(state, {connection :: pid(),
                market     :: [binary()],
                data       :: binary(),
                token      :: binary()}).

-define(url_for(Method), "/exchange/betting/json-rpc/v1/"
        ++ atom_to_list(Method)).


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec start_link(Market, Token, Owner) -> term() when
      Market  :: string(),
      Token   :: binary(),
      Owner   :: pid().
start_link(Market, Token, Owner) ->
    gen_server:start_link(?MODULE, [Market, Token, Owner], []).

-spec add_market(pid(), binary()) -> ok.
add_market(Pid, Market) ->
    gen_server:cast(Pid, {add_market, Market}).


%%------------------------------------------------------------------------------
%% gen-server callbacks
%%------------------------------------------------------------------------------

init([Market, Token, Owner]) ->
    self() ! {connect, {betfair:domain(exchange), betfair:ssl_options()}},
    _ = monitor(Owner),
    {ok, #state{data   = ?EMPTY_BIN,
                market = Market,
                token  = Token}}.

handle_call(_, _, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    {stop, owner_died, State};
handle_info({connect, Opts}, State) ->
    case open(Opts) of
        {error, Reason} ->
            {stop, Reason, State};
        Connection ->
            _ = erlang:send_after(3000, self(), poll),
            {noreply, State#state{connection=Connection}}
    end;
handle_info(poll, #state{market=Market,
                         token=Token,
                         connection=Conn} = State) ->
    Rpc = betfair_rpc:new(list_market_book, [{market_ids, [Market]}]),
    ReqHeaders = [{<<"X-Authentication">>, Token},
                  {<<"X-Application">>, betfair:get_env(app_key)}],
    _ = gun:post(Conn, ?url_for(list_market_book), ReqHeaders, jsx:encode(Rpc)),
    {noreply, State};
handle_info({gun_response, _, _, nofin, 200, _}, State) ->
    {noreply, State};
handle_info({gun_response, _, _, fin, Status, Headers}, State) ->
    Reason = betfair_http:handle_status(Status, Headers),
    {stop, Reason, State};
handle_info({gun_data, _, _, nofin, Part}, #state{data = Acc} = State) ->
    {noreply, State#state{data = <<Acc/binary, Part/binary>>}};
handle_info({gun_data, _, _Stream, fin, Part},
            #state{data = Acc, market = Market} = State) ->
    Data = <<Acc/binary, Part/binary>>,
    _ = gproc_ps:publish(?SCOPE, {market_update, Market}, Data),
    _ = erlang:send_after(3000, self(), poll),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, #state{connection=Connection}) ->
    gun:shutdown(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec open({string(), [tuple()]}) -> {ok, pid()} | {error, term()}.
open({Endpoint, SslOpts}) ->
    case gun:open(Endpoint, 443, #{transport      => ssl,
                                   transport_opts => SslOpts}) of
        {ok, Connection} ->
            case gun:await_up(Connection) of
                {ok, _} ->
                    Connection;
                Error -> Error
            end;
        Error -> Error
    end.

-spec monitor(pid()) -> reference().
monitor(Pid) ->
    erlang:monitor(process, Pid).
