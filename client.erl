-module(client).
-compile(export_all).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 7777, [list, {packet, 0}]),
    io:format("connected to: ~p~n", [Socket]),
    gen_tcp:send(Socket, "hello"),
    receive
        Reply ->
            io:format("Reply: ~p~n", [Reply])
    end.
