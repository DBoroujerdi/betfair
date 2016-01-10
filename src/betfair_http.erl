-module(betfair_http).

-export([url_encode/1]).
-export([hdr/2]).
-export([handle_status/2]).


%%------------------------------------------------------------------------------
%% Api Functions
%%------------------------------------------------------------------------------

-spec url_encode(map()) -> binary().
url_encode(Params) ->
    url_encode(lists:map(fun to_binary/1, maps:to_list(Params)), <<>>).

url_encode([{K, V}|[]], Acc) ->
    <<Acc/binary, K/binary, "=", V/binary>>;
url_encode([{K, V}|Tail], Acc) ->
    Acc2 = <<Acc/binary, K/binary, "=", V/binary, "&">>,
    url_encode(Tail, Acc2).

-spec hdr(binary(), list()) -> {binary(), list()}.
hdr(Key, Val) when is_binary(Key) and is_list(Val) ->
    {Key, Val}.

-spec handle_status(integer(), any()) ->
                           {atom(), {status, integer}} |
                           {atom(), {status, integer}, {headers | any()}}.
handle_status(Status, Headers) when Status >= 500 ->
    {server_error, {status, Status}, {headers, Headers}};
handle_status(Status, _) when Status >= 400 ->
    {client_error, {status, Status}};
handle_status(Status, _) when Status =/= 200 ->
    {error_status, {status, Status}}.


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec to_binary(term()) -> binary().
to_binary({L, R}) ->
    {to_binary(L), to_binary(R)};
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
    V.


%%------------------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_binary_test() ->
    ?assertEqual(<<"atom">>, to_binary(atom)),
    ?assertEqual(<<"1234">>, to_binary(1234)),
    ?assertEqual(<<"string">>, to_binary("string")),
    ?assertEqual(<<"binary">>, to_binary(<<"binary">>)).

url_encode_test() ->
    ?assertEqual(<<"a=b&c=d&e=f">>, url_encode(#{a => b, c => d, e => f})).

-endif.
