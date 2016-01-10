-module(betfair_util).

-export([any_to_binary/1,
         concat/2]).

any_to_binary(Any) when is_atom(Any) ->
    atom_to_binary(Any, utf8);
any_to_binary(Any) when is_list(Any) ->
    list_to_binary(Any).

concat(Bin1, Bin2) when is_binary(Bin1), is_binary(Bin2) ->
    <<Bin1/binary, Bin2/binary>>.
