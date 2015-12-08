-module(betfair_ops).

-export([list_event_types/2]).
%% TODO: the rest of the commands


-define(BASE_RPC, #{jsonrpc => "2.0", id => "1"}).


-type rpc_command() :: #{jsonrpc => string(),
                         method => string(),
                         id => number(),
                         params => map()}.

-type command() :: string().
-type filter() :: map().


-spec list_event_types(string(), string()) -> rpc_command().

list_event_types(EventType, Country) ->
    Filter = #{eventTypeIds => [EventType],
               marketCountries => [Country]},

    rpc("SportsAPING/v1.0/listEvents", #{params => #{filter => Filter}}).




%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

-spec rpc(command(), filter()) -> rpc_command().

rpc(Method, Filter) ->
    ?BASE_RPC#{method => Method,
               params => #{filter => Filter}}.


%%%====================================================================
%%% Unit tests
%%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_event_types_test() ->
    ActualCommand = list_event_types("7", "GB"),
    Expected = #{ jsonrpc => "2.0",
                  method => "",
                  id => "1",
                  params => #{
                    filter => #{eventTypeIds => ["7"],
                                marketCountries => ["GB"]
                               }
                   }},

    ?assertEqual(Expected, ActualCommand).

-endif.
