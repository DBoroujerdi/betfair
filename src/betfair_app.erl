-module(betfair_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%%====================================================================
%% API
%%====================================================================
start(_, _) ->
    lager:info("Starting.."),

    Opts = betfair:get_env(),

    case check_options(Opts) of
        ok ->
            betfair_sup:start_link(Opts);
        Error ->
            Error
    end.

stop(_) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

check_options([{num_conns, NumCons}|Rest]) when is_number(NumCons) ->
    check_options(Rest);
check_options([{ssl, SslOpts}|Rest]) when is_list(SslOpts) ->
    check_options(Rest);
check_options([{credentials, Credentials}|Rest]) when is_list(Credentials)  ->
    check_options(Rest);
check_options([{_Key, _Value}|Rest]) ->
    check_options(Rest);
check_options([]) ->
    ok.

%% TODO now actually check each option type
