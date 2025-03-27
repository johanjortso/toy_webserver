# toy_webserver
An extremely simple web server for educational purposes

## Current application behaviour (not quite yet HTTP)

- [x] One-off TCP connection. Replies to any TCP request ping with a pong, then closes connection.

## Common test

```console
$ ct_run -dir . -logdir ct
```
