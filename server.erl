-module(server).
-compile([export_all, nowarn_export_all]).

start(Port) ->
    {ok, ListenSocket} =
        gen_tcp:listen(Port, [list, {packet, 0},
                                    {active, true},
                                    {reuseaddr, true}]),
    ok = io:format("Server: Opened listening socket: ~p~n", [ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ok = io:format("Server: Accepted connection on socket: ~p~n", [Socket]),
    ok = gen_tcp:close(ListenSocket),
    io:format("Server: Listening on port ~p~n", [Port]),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, StringMsg} ->
            io:format("Server: Received message: ~p~n", [StringMsg]),
            Reply = "Echo " ++ StringMsg,
            io:format("Server: Replying: ~p~n", [Reply]),
            ok = gen_tcp:send(Socket, Reply),
            ok = gen_tcp:close(Socket);
        {tcp_closed, Socket} ->
            io:format("Server: Socket closed - shutting down...~n")
    end.
