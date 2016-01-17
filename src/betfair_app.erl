-module(betfair_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).


%%------------------------------------------------------------------------------
%% Api Functions
%%------------------------------------------------------------------------------

start(_, _) ->
    _ = lager:info("Starting.."),
    Opts = betfair:get_opts(),
    ok = check_opts(Opts),
    ok = betfair_session_token_store:new(),
    ok = setup_session(Opts),

    PoolConfig = [{name, connection_pool},
                  {max_count, betfair:prop(max_connections)},
                  {init_count, betfair:prop(num_connections)},
                  {start_mfa, {betfair_connection, start_link, []}}],

    _ = lager:info("~p", [PoolConfig]),

    {ok, _} = pooler:new_pool(PoolConfig),

    betfair_sup:start_link(Opts).

stop(_) ->
    ok.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec check_opts(list(tuple())) -> ok.
check_opts([{keep_alive, KeepAlive}|Rest]) when is_number(KeepAlive) ->
    check_opts(Rest);
check_opts([{num_conns, NumCons}|Rest]) when is_number(NumCons) ->
    check_opts(Rest);
check_opts([{num_procs, NumCons}|Rest]) when is_number(NumCons) ->
    check_opts(Rest);
check_opts([{ssl, SslOpts}|Rest]) when is_list(SslOpts) ->
    check_opts(Rest);
check_opts([{credentials, Credentials}|Rest]) when is_list(Credentials) ->
    check_opts(Rest);
check_opts([{identity_endpoint, Opt}|Rest]) when is_list(Opt) ->
    check_opts(Rest);
check_opts([{num_connections, Opt}|Rest]) when is_number(Opt) ->
    check_opts(Rest);
check_opts([{max_connections, Opt}|Rest]) when is_number(Opt) ->
    check_opts(Rest);
check_opts([{exchange_endpoint, Opt}|Rest]) when is_list(Opt) ->
    check_opts(Rest);
check_opts([{included_applications, _Opt}|Rest]) ->
    check_opts(Rest);

check_opts([]) ->
    ok.

-spec setup_session(list(tuple())) -> ok.
setup_session(Opts) ->
    {ok, SessionToken} = betfair_session:new_session(Opts),
    betfair_session_token_store:update_token(SessionToken).
