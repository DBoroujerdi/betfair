bety
=====

An Erlang/OTP Betfair Client

Build
-----

``` $ make ```

Config
------

Example..

```
            {credentials, [
                           {username, "username"},
                           {password, "password"}
                          ]
            },
            {app_key, "app_key"},
            {ssl, [
                   {certfile, "/path/to/client-2048.crt"},
                   {keyfile, "/path/to/client-2048.key"},
                   {password, "password"}
                  ]},
            {max_connections, 10},
            {num_connections, 2},
            {num_procs, 10},
            {keep_alive, 1},
            {identity_endpoint, "identitysso.betfair.com"},
            {exchange_endpoint, "api.betfair.com"}
```

Run
---

``` $ ./start-dev.sh ```


Api
---

Start a session.
```
{ok, Token} = betfair:new_session().
```

Get a list of event types.
```
{ok, Result} = betfair:request(list_event_types, Token, []).
```

With that you can then request events of a chosens type(s).
```
{ok, Result} = betfair:request(list_events, Token, [{filter, [{event_type_ids, [7]}]}]).
```

Then the market catalogue of that event.
```
{ok, Result} = betfair:request(list_market_catalogue, Token, [{filter, [{event_ids, [27652586]}]}, {max_results, 200}, {market_projection, [<<"COMPETITION">>, <<"EVENT">>, <<"EVENT_TYPE">>, <<"RUNNER_DESCRIPTION">>, <<"RUNNER_METADATA">>, <<"MARKET_START_TIME">>]}]).
```

Finally, subscribe to regular price updates for a chosen market.
```
{ok, Pid} = betfair:subscribe(Market, Token),
flush().
```
