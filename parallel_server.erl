-module(parallel_server).
-compile([export_all, nowarn_export_all]).

start(Port) ->
    {ok, ListenSocket} =
        gen_tcp:listen(Port, [list, {packet, 0},
                                    {active, true},
                                    {reuseaddr, true}]),
    {ok, {ListenIp, ListenPort}} = inet:sockname(ListenSocket),
    ok = io:format("Server: Listening on IP ~p port ~p~n", [ListenIp, ListenPort]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, {LocalIp, LocalPort}} = inet:sockname(Socket),
    {ok, {PeerIp, PeerPort}} = inet:peername(Socket),
    ok = io:format("Server: Accepted connection from IP ~p port ~p on IP ~p port ~p~n",
                   [PeerIp, PeerPort, LocalIp, LocalPort]),
    ok = gen_tcp:close(ListenSocket),
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
