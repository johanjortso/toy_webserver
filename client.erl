-module(client).
-compile(export_all).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 7777, [list, {packet, 0}]),
    io:format("Client: connected to: ~p~n", [Socket]),
    gen_tcp:send(Socket, "hello"),
    ReplyFromServer =
        receive
            Reply ->
                io:format("Client: reply: ~p~n", [Reply]),
                Reply
        after timer:seconds(30) ->
            io:format("Client: no data received.~n"),
            {}
        end,
    io:format("Client: closing connection.~n"),
    gen_tcp:close(Socket),
    ReplyFromServer.
