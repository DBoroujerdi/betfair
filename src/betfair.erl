-module(betfair).

-ifdef(TEST).
-compile([export_all]).
-endif.

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([list_countries/0]).

-export([start/0]).
-export([make/0]).
-export([get_env/0]).

-define(SCOPE, l).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {token, connection}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

list_countries() ->
    gen_server:cast(?MODULE, {list_countries, self()}).

make() ->
    make:all([load]).

start() ->
    application:ensure_all_started(?MODULE).

get_env() ->
    application:get_all_env(betfair).


%%--------------------------------------------------------------------
%% gen-server callbacks
%%--------------------------------------------------------------------

init([Opts]) ->
    {ok, SessionToken} = betfair_session:get_token(),

    SslOpts = proplists:get_value(ssl, Opts),

    lager:info("Connecting to the betfair api with token ~p", [SessionToken]),
    ExchangeEndpoint = proplists:get_value(exchange_endpoint, Opts),
    {ok, Connection} = gun:open(ExchangeEndpoint, 443,
                               #{transport => ssl, transport_opts => SslOpts}),

    {ok, _} = gun:await_up(Connection),

    {ok, #state{token=SessionToken, connection=Connection}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({list_countries, _CallerPid},
            #state{token = _Token, connection = _Connection} = State) ->

    %% TODO

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
