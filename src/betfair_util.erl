-module(betfair_util).

-export([any_to_binary/1]).

any_to_binary(Any) when is_atom(Any) ->
    atom_to_binary(Any, utf8);
any_to_binary(Any) when is_list(Any) ->
    list_to_binary(Any).
