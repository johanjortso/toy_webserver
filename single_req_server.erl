-module(single_req_server).

-compile([export_all, nowarn_export_all]).

start(Port) ->
    {ok, ListenSocket} =
        gen_tcp:listen(Port, [list, {packet, 0}, {active, true}, {reuseaddr, true}]),
    {ok, {ListenIp, ListenPort}} = inet:sockname(ListenSocket),
    Pid = self(),
    ok = io:format("Server~p: Listening on IP ~p port ~p~n", [Pid, ListenIp, ListenPort]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, {LocalIp, LocalPort}} = inet:sockname(Socket),
    {ok, {PeerIp, PeerPort}} = inet:peername(Socket),
    io:format("Server~p: Accepted connection:~n  send IP ~p port ~p~n"
              "  recv IP ~p port ~p - Spawning handler...~n",
              [Pid, PeerIp, PeerPort, LocalIp, LocalPort]),
    ok = gen_tcp:close(ListenSocket),
    loop(Socket).

loop(Socket) ->
    Pid = self(),
    receive
        {tcp, Socket, StringMsg} ->
            io:format("Server~p: Received message: ~p~n", [Pid, StringMsg]),
            Reply = "Echo " ++ StringMsg,
            io:format("Server~p: Replying: ~p~n", [Pid, Reply]),
            ok = gen_tcp:send(Socket, Reply);
        {tcp_closed, Socket} ->
            io:format("Server~p: Socket closed - shutting down...~n", [Pid])
    end,
    ok = gen_tcp:close(Socket).
