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
    lager:info("Starting.."),

    Opts = betfair:get_opts(),

    case check_opts(Opts) of
        ok ->
            betfair_sup:start_link(Opts);
        Error ->
            Error
    end.

stop(_) ->
    ok.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

check_opts([{num_conns, NumCons}|Rest]) when is_number(NumCons) ->
    check_opts(Rest);
check_opts([{_Key, _Value}|Rest]) ->
    check_opts(Rest);
check_opts([]) ->
    ok.
