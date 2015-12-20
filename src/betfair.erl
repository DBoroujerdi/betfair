-module(betfair).

-export([start/0]).
-export([make/0]).
-export([get_opts/0]).
-export([start_session/0]).
-export([start_session/1]).
-export([start_connection/0]).
-export([request/3]).
-export([request_sync/3]).

-define(SCOPE, l).


%% TODO: api inputs validation!
%% TODO: Sync requests


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

make() ->
    make:all([load]).

start() ->
    application:ensure_all_started(?MODULE).

get_opts() ->
    application:get_all_env(betfair).

start_session() ->
    start_session(get_opts()).
start_session(Opts) ->
    betfair_session_sup:start_session(check_opts(Opts)).

start_connection() ->
    betfair_connection_sup:start_connection().

request(Pid, Command, Filter) ->
    betfair_connection:request(Pid, {Command, Filter}).

request_sync(_Pid, _Command, _Filter) ->
    %% TODO:
    ok.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

check_opts(Opts) when is_list(Opts) ->
    check_opts(Opts, Opts).

check_opts(Opts, [{keep_alive, KeepAlive}|Rest]) when is_number(KeepAlive) ->
    check_opts(Opts, Rest);
check_opts(Opts, [{num_conns, NumCons}|Rest]) when is_number(NumCons) ->
    check_opts(Opts, Rest);
check_opts(Opts, [{ssl, SslOpts}|Rest]) when is_list(SslOpts) ->
    check_opts(Opts, Rest);
check_opts(Opts, [{credentials, Credentials}|Rest]) when is_list(Credentials)  ->
    check_opts(Opts, Rest);
check_opts(Opts, [{_Key, _Value}|Rest]) ->
    check_opts(Opts, Rest);
check_opts(Opts, []) ->
    Opts.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% TODO: test check_opts

-endif
