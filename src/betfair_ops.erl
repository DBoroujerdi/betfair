-module(betfair_ops).

-export([rpc/1]).
-export([rpc/2]).

-define(METHOD_PREFIX, <<"SportsAPING/v1.0/">>).

-export_type([rpc/0]).

-define(BASE_RPC, #{jsonrpc => <<"2.0">>, id => <<"1">>}).


-type rpc() :: #{jsonrpc => string(),
                         method => string(),
                         id => number(),
                         params => map()}.

-type method() :: atom().
-type filters() :: list(tuple()).

-spec rpc(method()) -> map().
rpc(Method) ->
    rpc(Method, []).

-spec rpc(method(), filters()) -> rpc().
rpc(Method, Filters) ->
    ?BASE_RPC#{method => method(Method),
               params => #{filter => maps:from_list(Filters)}}.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

method(Method) ->
    Bin1 = ?METHOD_PREFIX,
    Bin2 = atom_to_binary(Method, utf8),
    <<Bin1/binary, Bin2/binary>>.

%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rpc_test() ->
    ActualCommand = rpc(listEventTypes),
    Expected = #{ jsonrpc => <<"2.0">>,
                  method => <<"SportsAPING/v1.0/listEventTypes">>,
                  id => <<"1">>,
                  params => #{
                    filter => #{}
                   }},

    ?assertEqual(Expected, ActualCommand).

rpc_with_filters_test() ->
    MarketFilters = [{eventTypeIds, [1,2,3]},
                     {marketCountries, [<<"GB">>]}],
    ActualCommand = rpc(listEvents, MarketFilters),
    Expected = #{ jsonrpc => <<"2.0">>,
                  method => <<"SportsAPING/v1.0/listEvents">>,
                  id => <<"1">>,
                  params => #{
                    filter => #{eventTypeIds => [1,2,3],
                                marketCountries => [<<"GB">>]}
                   }},

    ?assertEqual(Expected, ActualCommand).

-endif.
