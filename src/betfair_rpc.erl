-module(betfair_rpc).

-export([new/1]).
-export([new/2]).
-export([check_filters/1]).
-export([is_valid_method/1]).

-define(METHODS, [listEventTypes]).
-define(APING_PREFIX, <<"SportsAPING/v1.0/">>).


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-export_type([rpc/0]).

-define(BASE_RPC, #{jsonrpc => <<"2.0">>, id => <<"1">>}).


-type rpc() :: #{jsonrpc => binary(),
                 method => binary(),
                 id => binary(),
                 params => map()}.

-type method() :: atom().
-type filters() :: list(tuple()).


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new(method()) -> map().
new(Method) ->
    new(Method, []).

-spec new(method(), filters()) -> rpc().
new(Method, Filters) ->
    ?BASE_RPC#{method => method(Method),
               params => #{filter => maps:from_list(Filters)}}.

-spec check_filters(list(tuple())) -> ok | {error, tuple()}.
check_filters([]) -> ok;
check_filters([H|Tail]) when is_tuple(H) ->
    case check_filter(H) of
        ok     -> check_filters(Tail);
        Reason -> {filter_error, H, Reason}
    end;
check_filters(_) ->
    filter_error.

-spec is_valid_method(atom()) -> boolean().
is_valid_method(Method) when is_atom(Method) ->
    lists:member(Method, ?METHODS);
is_valid_method(_) ->
    false.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec check_filter({atom(), any()}) -> ok | {invalid_value, Reason} when
      Reason :: term().
check_filter({eventTypeIds, Value}) when is_list(Value) ->
    check_integer_list(Value).

-spec check_integer_list(List) -> ok | {invalid_value, Reason} when
      List :: list(integer()) | [],
      Reason :: term().
check_integer_list([])   -> ok;
check_integer_list([H|T]) when is_integer(H) ->
    check_integer_list(T);
check_integer_list(V) -> {invalid_value, V}.

-spec method(atom()) -> binary().
method(Method) ->
    betfair_util:concat(?APING_PREFIX, betfair_util:any_to_binary(Method)).


%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


rpc_test() ->
    ActualCommand = ?MODULE:new(listEventTypes),
    Expected = #{jsonrpc => <<"2.0">>,
                 method => <<"SportsAPING/v1.0/listEventTypes">>,
                 id => <<"1">>,
                 params => #{filter => #{}}},

    ?assertEqual(Expected, ActualCommand).


rpc_with_filters_test() ->
    MarketFilters = [{eventTypeIds, [1, 2, 3]},
                     {marketCountries, [<<"GB">>]}],
    ActualCommand = ?MODULE:new(listEvents, MarketFilters),
    Expected = #{jsonrpc => <<"2.0">>,
                 method => <<"SportsAPING/v1.0/listEvents">>,
                 id => <<"1">>,
                 params => #{filter => #{eventTypeIds => [1, 2, 3],
                                         marketCountries => [<<"GB">>]}}},

    ?assertEqual(Expected, ActualCommand).

-endif.
