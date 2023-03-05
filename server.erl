-module(server).
-compile(export_all).

start(Port) ->
    {ok, ListenSocket} =
        gen_tcp:listen(Port, [list, {packet, 0},
                                    {reuseaddr, true},
                                    {active, true}]),
    io:format("1~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("2~n"),
    ok = gen_tcp:close(ListenSocket),
    io:format("Server listening on port ~p~n", [Port]),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, StringMsg} ->
            io:format("Server received message: ~p~n", [StringMsg]),
            Reply = "Echo " ++ StringMsg,
            io:format("Server replying: ~p~n", [Reply]),
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed - shutting down...~n")
    end.
