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
    betfair_sup:start_link(Opts).

stop(_) ->
    ok.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec check_opts(list(tuple())) -> ok.
check_opts([{num_conns, NumCons}|Rest]) when is_number(NumCons) ->
    check_opts(Rest);
check_opts([{_Key, _Value}|Rest]) ->
    check_opts(Rest);
check_opts([]) ->
    ok.
