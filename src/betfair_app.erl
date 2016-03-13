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

    betfair_sup:start_link().

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
check_opts([{app_key, _Opt}|Rest]) ->
    check_opts(Rest);

check_opts([]) ->
    ok.
