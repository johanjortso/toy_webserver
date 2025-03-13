-module(client).
-compile(export_all).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 7777, [list, {packet, 0}]),
    io:format("Client: connected to: ~p~n", [Socket]),
    gen_tcp:send(Socket, "hello"),
    receive
        Reply ->
            io:format("Client: reply: ~p~n", [Reply])
    after timer:seconds(30) ->
        io:format("Client: no data received.~n")
    end.
