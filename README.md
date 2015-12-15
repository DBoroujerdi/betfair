betfair
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
            {num_procs, 10},
            {keep_alive, 3600},
            {identity_endpoint, "identitysso.betfair.com"},
            {exchange_endpoint, "api.betfair.com"}
```

Run
---

``` $ ./start-dev.sh ```


Api
---

TODO..