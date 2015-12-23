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

-record(state, {token, connection}).

%% TODO: Actually do something with the connection and request

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link(Opts, SessionToken) ->
    gen_server:start_link(?MODULE, [Opts, SessionToken], []).

request(Pid, Command) ->
    gen_server:cast(Pid, Command).

set_owner(Pid, OwnerPid) ->
    gen_server:call(Pid, {owner_pid, OwnerPid}).


%%------------------------------------------------------------------------------
%% gen-server callbacks
%%------------------------------------------------------------------------------

init([Opts, Session]) ->
    SslOpts = proplists:get_value(ssl, Opts),

    lager:info("Connecting to the betfair api with token ~p", [Session]),
    Endpoint = proplists:get_value(exchange_endpoint, Opts),
    {ok, Connection} = gun:open(Endpoint, 443,
                                #{transport => ssl, transport_opts => SslOpts}),

    {ok, _} = gun:await_up(Connection),

    {ok, #state{token=Session, connection=Connection}}.

handle_call({owner_pid, Pid}, _From, State) ->
    Mref = erlang:monitor(process, Pid),
    {reply, {ok, Mref}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({_Command, _Filter} = Request, State) ->

    _ = lager:info("Received request ~p", [Request]),

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, _ServerPid, _Reason}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
