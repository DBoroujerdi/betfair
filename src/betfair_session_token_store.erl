-module(betfair_session_token_store).

-export([new/0]).
-export([update_token/1]).
-export([get_token/0]).


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new() -> ok.
new() ->
    session_token = ets:new(session_token, [set, named_table]),
    ok.

-spec update_token(betfair_session:token()) -> ok.
update_token(Token) ->
    true = ets:insert(session_token, {session_token, Token}),
    ok.

-spec get_token() -> {ok, betfair_session:token()} | undefined.
get_token() ->
    return(ets:lookup(session_token, session_token)).


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec return(EtsItem) -> betfair_session:token() | undefined when
      EtsItem :: [] | [{session_token, betfair_session:token()}].
return([{session_token, Token}]) ->
    {ok, Token};
return([]) ->
    undefined.
