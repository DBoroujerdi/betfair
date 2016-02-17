-module(betfair_rpc).

-export([new/1]).
-export([new/2]).
-export([check/2]).
-export([check_params/1]).

-define(APING_PREFIX, <<"SportsAPING/v1.0/">>).

-define(ALLOWED_PROJECTIONS, [<<"COMPETITION">>,
                              <<"EVENT">>,
                              <<"EVENT_TYPE">>,
                              <<"RUNNER_DESCRIPTION">>,
                              <<"RUNNER_METADATA">>,
                              <<"MARKET_START_TIME">>]).


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% -export_type([rpc/0]).

-define(BASE_RPC, [{jsonrpc, <<"2.0">>}, {id, <<"1">>}]).

%% -type rpc() :: [{jsonrpc, binary()},
%%                 {method, binary()},
%%                 {id, binary()},
%%                 {params, map()}].


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new(betfair:method()) -> map().
new(Method) ->
    new(Method, []).

-spec new(betfair:method(), betfair:params()) -> list(tuple).
new(Method, Params) ->
    ?BASE_RPC ++ [{method, method(Method)},
                  {params, params(Params)}].

-spec check(atom(), betfair:params()) -> ok | {error, atom(), any()}.
check(list_event_types, Params) ->
    check_params(contains(Params, []));
check(list_events, Params) ->
    check_params(contains(Params, [filter]));
check(list_market_catalogue, Params) ->
    check_params(contains(Params, [filter, max_results]));
check(Method, _) ->
    {error, invalid_method, Method}.

-spec check_params(list(tuple())) -> ok | {check_error, tuple()}.
check_params([]) -> ok;
check_params(Params) when is_list(Params) ->
    check_params(Params, fun check_param/1).


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec contains(list(tuple()), list(atom())) -> ok | {error, missing_params}.
contains(Params, Required) ->
    case check_members_of(Required, names(Params)) of
        ok -> Params;
        _ -> {error, missing_param}
    end.


-spec names(list(tuple())) -> list(atom()).
names(Params) -> proplists:get_keys(Params).

-spec check_params(list(tuple()), fun()) -> ok | {check_error, any()}.
check_params([], _) -> ok;
check_params([H|Tail], CheckFun) when is_tuple(H) ->
    case CheckFun(H) of
        ok    -> check_params(Tail, CheckFun);
        Error -> {check_error, Error}
    end;
check_params([H|_], _) ->
    {check_error, {format, H}}.


-spec check_param({atom(), any()}) -> ok | {invalid_value, Reason} when
      Reason :: term().
check_param({filter, Param}) when is_list(Param) ->
    check_params(Param, fun check_filter/1);
check_param({max_results, Param}) ->
    case Param of
        P when false =:= is_number(P) -> incorrect_type;
        P when (P < 0) or (P > 300)   -> out_of_bounds;
        _ -> ok
    end;
check_param({market_projection, Param}) ->
    case Param of
        P when (false =:= is_list(P)) -> incorrect_type;
        P -> check_members_of(P, ?ALLOWED_PROJECTIONS)
    end;
check_param(Param) -> {unknown_param, Param}.


-spec check_members_of(list(), list()) -> unknown_value | ok.
check_members_of([], _) -> ok;
check_members_of([H|T], List) ->
    case lists:member(H, List) of
        true  -> check_members_of(T, List);
        false -> unknown_value
    end.


-spec check_filter(tuple()) -> incorrect_type | ok.
check_filter({event_type_ids, Filter}) -> is_integer_list(Filter);
check_filter({event_ids, Filter}) -> is_integer_list(Filter);
check_filter(Tuple) -> {unknown_filter, Tuple}.

-spec is_integer_list(list(integer())) -> incorrect_type | ok.
is_integer_list(Val) ->
    case Val of
        V when false =:= is_list(V) -> incorrect_type;
        V -> check_integer_types(V)
    end.

-spec check_integer_types(List) -> ok | {invalid_value, Reason} when
      List :: list(integer()) | [],
      Reason :: term().
check_integer_types([]) -> ok;
check_integer_types([H|T]) when is_integer(H) -> check_integer_types(T);
check_integer_types(V) -> {invalid_value, V}.


-spec method(atom()) -> binary().
method(Method) ->
    concat(?APING_PREFIX, camel_case(atom_to_binary(Method, utf8))).

-spec concat(binary(), binary()) -> binary().
concat(Bin1, Bin2) when is_binary(Bin1), is_binary(Bin2) ->
    <<Bin1/binary, Bin2/binary>>.

-spec params(list(tuple())) -> list(tuple()).
params([]) ->
    [{filter, [{}]}];
params(Params) ->
    camel_case_keys(Params).


-spec camel_case_keys(betfair:params()) -> betfair:params().
camel_case_keys(Params) ->
    camel_case_keys(Params, []).

-spec camel_case_keys(betfair:params(), betfair:params()) -> betfair:params().
camel_case_keys([], Acc) ->
    lists:reverse(Acc);
camel_case_keys([{Key, Val}|Tail], Acc) when is_list(Val) ->
    camel_case_keys(Tail, [{camel_case(Key), camel_case_keys(Val)}|Acc]);
camel_case_keys([{K, V}|Tail], Acc) ->
    camel_case_keys(Tail, [{camel_case(K), V}|Acc]);
camel_case_keys([H|Tail], Acc) ->
    camel_case_keys(Tail, [H|Acc]).


-spec camel_case(Val) -> binary() | atom() | string() when
      Val :: binary() | string() | atom().
camel_case(A) when is_atom(A) ->
    binary_to_atom(camel_case(atom_to_binary(A, utf8)), utf8);
camel_case(B) when is_binary(B) ->
    camel_case(B, <<>>);
camel_case(C) when is_list(C) ->
    binary_to_list(camel_case(list_to_binary(C))).


-spec camel_case(binary(), binary()) -> binary().
camel_case(<<$_, C:1/binary, Rest/binary>>, Acc) ->
    C2 = unistring:to_upper(C),
    camel_case(Rest, <<Acc/binary, C2/binary>>);
camel_case(<<C:1/binary, Rest/binary>>, Acc) ->
    camel_case(Rest, <<Acc/binary, C/binary>>);
camel_case(<<>>, Acc) -> Acc.


%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, debug_info]).

camel_case_keys_test() ->
    Input = [{filter, [{event_type_ids, [1, 2, 3]},
                      {market_countries, [<<"GB">>]}]},
             {market_projection, [<<"COMPETITION">>]}],

    Expected = [{filter, [{eventTypeIds, [1, 2, 3]},
                          {marketCountries, [<<"GB">>]}]},
                {marketProjection, [<<"COMPETITION">>]}],

    ?assertEqual(Expected, camel_case_keys(Input)).


camel_case_test() ->
    ?assertEqual(<<"eventTypeIds">>, camel_case(<<"event_type_ids">>)),
    ?assertEqual(<<"marketProjection">>, camel_case(<<"market_projection">>)).

rpc_test() ->
    ActualCommand = ?MODULE:new(listEventTypes),
    Expected = [{jsonrpc, <<"2.0">>},
                {id, <<"1">>},
                {method, <<"SportsAPING/v1.0/listEventTypes">>},
                {params, [{filter, [{}]}]}],

    ?assertEqual(Expected, ActualCommand).


rpc_with_params_test() ->
    MarketParams = [{filter, [{event_type_ids, [1, 2, 3]},
                             {market_countries, [<<"GB">>]}]},
                    {market_projection, [<<"COMPETITION">>]}],
    ActualCommand = ?MODULE:new(listEvents, MarketParams),
    Expected = [{jsonrpc, <<"2.0">>},
                {id, <<"1">>},
                {method, <<"SportsAPING/v1.0/listEvents">>},
                {params, [{filter, [{eventTypeIds, [1, 2, 3]},
                                    {marketCountries, [<<"GB">>]}]},
                          {marketProjection, [<<"COMPETITION">>]}]}],

    ?assertEqual(Expected, ActualCommand).


check_params_test() ->
    TestParams = [{filter, [{event_type_ids, [1, 2, 3]}]},
                  {max_results, 200},
                  {market_projection, [<<"COMPETITION">>,
                                       <<"EVENT">>,
                                       <<"EVENT_TYPE">>,
                                       <<"RUNNER_DESCRIPTION">>,
                                       <<"RUNNER_METADATA">>,
                                       <<"MARKET_START_TIME">>]}],

    Result = ?MODULE:check_params(TestParams),
    ?assertEqual(ok, Result).

-endif.
