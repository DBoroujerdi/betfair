-module(betfair_request).

-export([login/0,
         request/3]).

-define(HTTPS, "https://").
-define(LOGIN_PATH, "/api/certlogin").
-define(RPC_PATH, "/exchange/betting/json-rpc/v1/").

-define(HEADER_X_AUTHENTICATION, "X-Authentication").
-define(HEADER_ACCEPT, "Accept").
-define(HEADER_X_APPLICATION, "X-Application").

-define(CONTENT_TYPE_JSON, "application/json").
-define(CONTENT_TYPE_URL_FORM_ENCODED, "application/x-www-form-urlencoded").

-define(OPTIONS, [{body_format, binary}]).


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec login() -> binary() | {error, term()}.
login() ->
    Url = url(?HTTPS, betfair:domain(identity), ?LOGIN_PATH),
    Headers = [{?HEADER_X_APPLICATION, betfair:get_env(app_key)},
              {?HEADER_ACCEPT, ?CONTENT_TYPE_JSON}],

    case execute({Url, Headers, ?CONTENT_TYPE_URL_FORM_ENCODED,
                  betfair_http:url_encode(betfair:credentials())}) of
        {ok, Result} ->
            token(Result);
        Error ->
            Error
    end.


-spec request(atom(), map(), binary()) -> binary() | {error, term()}.
request(Method, Body, Token) ->
    Path = ?RPC_PATH ++ atom_to_list(Method),
    Url = url(?HTTPS, betfair:domain(exchange), Path),
    Headers = [{?HEADER_X_AUTHENTICATION, binary_to_list(Token)},
               {?HEADER_X_APPLICATION, betfair:get_env(app_key)}],

    execute({Url, Headers, ?CONTENT_TYPE_JSON, jsx:encode(Body)}).


%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------

-spec token(map()) -> {ok, binary()} | {error, binary()}.
token(#{sessionToken := Token, loginStatus := <<"SUCCESS">>}) ->
    {ok, Token};
token(#{loginStatus := Reason}) ->
    {error, Reason}.

-spec execute(httpc:request()) ->  {ok, map()} | {error, term()}.
execute(Params) ->
    Result = httpc:request(post, Params, http_options(), ?OPTIONS),
    case handle(Result) of
        {ok, Bin} ->
            parse(Bin);
        Error ->
            Error
    end.

http_options() -> [{ssl, betfair:ssl_options()}].

-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(Response) ->
    json_decode(Response).

-spec json_decode(binary()) -> {ok, map()} | {error, term()}.
json_decode(Body) ->
    try jsx:decode(Body, [{labels, atom}, return_maps]) of
        Decoded -> {ok, Decoded}
    catch
        _:Error -> {error, {jsx_decode_error, Error, Body}}
    end.

-spec handle(Response) -> {ok, binary()} | {error, term()} when
      Response :: {ok, binary()} | term().
handle({ok, Response}) ->
    body(Response);
handle(Response) ->
    {error, Response}.

body({_, _, Body}) ->
    {ok, Body};
body(Response) ->
    {error, {no_body, Response}}.

-spec url(string(), string(), string()) -> string().
url(Protocol, Host, Path) ->
    Protocol ++ Host ++ Path.
