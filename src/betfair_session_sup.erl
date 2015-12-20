-module(betfair_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_session/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type token() :: string().

-export_type([credentials/0]).
-export_type([token/0]).

-type credentials() :: #{username => string(),
                         password => string(),
                         app_key=> string()}.


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_session(Opts) ->
    Credentials = proplists:get_value(credentials, Opts),
    SSlOpts = proplists:get_value(ssl, Opts),
    Endpoint = proplists:get_value(identity_endpoint, Opts),
    {ok, SessionToken}  = receive_token(open_conn(Endpoint, SSlOpts),
                                  maps:from_list(Credentials)),

    ChildSpec = betfair_sup:supervisor(betfair_connection_sup,
                                       permanent, [Opts, SessionToken]),

    case supervisor:start_child(?MODULE,  ChildSpec) of
        {ok, _P} = Pid -> Pid;
        {error, {already_started, P}} -> {ok, P};
        {error, _Reason} = Error -> Error
    end.


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

open_conn(Endpoint, SSlOpts) ->
    {ok, Connection} = gun:open(Endpoint, 443,
                                #{transport => ssl, transport_opts => SSlOpts}),
    {ok, _} = gun:await_up(Connection),
    Connection.

receive_token(Connection, #{app_key := Appkey} = Credentials) ->
    ReqBody = betfair_http:url_encode(maps:without([app_key], Credentials)),
    ReqHeaders = [{<<"Content-Type">>, "application/x-www-form-urlencoded"},
                  {<<"X-Application">>, Appkey}],
    Stream = gun:post(Connection, "/api/certlogin", ReqHeaders, ReqBody),
    receive_data(Connection, Stream).


-spec receive_data(pid(), reference()) -> {ok, token()} | any().
receive_data(Connection, Stream) ->
    case gun:await(Connection, Stream) of
        {response, fin, _Status, _Headers} ->
            {error, no_data};
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(Connection, Stream),
            Response = jsx:decode(Body, [return_maps, {labels, atom}]),
            token(Response)
    end.

-spec token(map()) -> {ok, token()} | {error, string()}.
token(#{sessionToken := Token, loginStatus := _Reason}) ->
    {ok, Token};
token(#{token := Token, status := <<"SUCCESS">>}) ->
    {ok, Token};
token(#{loginStatus := Reason}) ->
    {error, Reason};
token(#{error := Reason}) ->
    {error, Reason}.
