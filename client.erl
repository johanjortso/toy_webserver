-module(client).
-compile([export_all, nowarn_export_all]).

start(Port) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, [list, {packet, 0}]),
    ok = io:format("Client: Connected to: ~p~n", [Socket]),
    ok = gen_tcp:send(Socket, "Hello"),
    ReplyFromServer =
        receive
            Reply ->
                ok = io:format("Client: Reply: ~p~n", [Reply]),
                Reply
        after timer:seconds(30) ->
            ok = io:format("Client: No data received.~n"),
            {}
        end,
    ok = io:format("Client: Closing connection.~n"),
    ok = gen_tcp:close(Socket),
    ReplyFromServer.
