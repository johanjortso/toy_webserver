# toy_webserver
An extremely simple web server for educational purposes. It exists in four different versions, with increasing complexity.

1. A TCP server that echoes what is sent to it, and then closes the connection
2. A TCP server that can serve multiple clients, but only one at a time
3. A fully parallel TCP server that still only echoes what it's sent
4. A basic HTTP server that understands GET

## Common test

```console
$ ct_run -dir . -logdir ct
```

Or from an Erlang shell
```console
$ ct:run_test([{suite, "./server_SUITE"}, {logdir, "./ct"}]).
$ ct:run_test([{suite, "./server_SUITE"}, {logdir, "./ct"}, {group, server_tests}, {case, [tc_server_parallell_client_requests]} ]).
```

## Debugging TCP connections

```console
netstat -ano | Select-String "8080"
```
