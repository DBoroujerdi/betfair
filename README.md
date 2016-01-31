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

Sync request
``` betfair:request(list_event_types, []). ```

Async request
``` betfair:request(list_event_types, [], [{sync, true}]). ```


Example Requests
----------------

All horse racing events
``` betfair:request(list_events, [{filter, [{event_type_ids, [7]}]}]). ```
