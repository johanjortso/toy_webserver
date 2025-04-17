-module(multi_req_server).
-compile([export_all, nowarn_export_all]).

start(Port) ->
    register(toy_webserver, self()),
    {ok, ListenSocket} =
        gen_tcp:listen(Port,
                       [list,
                        {packet, 0},
                        {active, true},
                        {reuseaddr, true}]),
    connect_loop(ListenSocket).

connect_loop(ListenSocket) ->
    %% Check if we've received message to shut down server, otherwise listen for connections.
    receive
        stop ->
            io:format("Server: shutting down...~n~n"),
            ok = gen_tcp:close(ListenSocket),
            exit(normal)
    after
        0 ->
            {ok, {ListenIp, ListenPort}} = inet:sockname(ListenSocket),
            ok = io:format("Server: Listening on IP ~p port ~p~n", [ListenIp, ListenPort])
    end,
    case gen_tcp:accept(ListenSocket, timer:seconds(30)) of
        %% A client connected, handle request.
        {ok, Socket} ->
            {ok, {LocalIp, LocalPort}} = inet:sockname(Socket),
            {ok, {PeerIp, PeerPort}} = inet:peername(Socket),
            io:format("Server: Accepted connection from IP ~p port ~p on IP ~p port ~p~n",
                      [PeerIp, PeerPort, LocalIp, LocalPort]),
            handle_connection(Socket);
        %% No client connected for a while, time out so we can check for stop message in next loop iteration.
        {error, timeout} ->
            io:format("Server: no client request to handle...~n~n")
    end,
    connect_loop(ListenSocket).

handle_connection(Socket) ->
    receive
       {tcp, Socket, StringMsg} ->
            io:format("Server: Received message: ~p~n", [StringMsg]),
            io:format("Server: working on request...~n"),
            %% Fake doing some work that takes time.
            timer:sleep(timer:seconds(3)),
            Reply = "Echo " ++ StringMsg,
            io:format("Server: Replying: ~p~n~n", [Reply]),
            ok = gen_tcp:send(Socket, Reply);
        {tcp_closed, Socket} ->
            io:format("Server: Client closed socket, aborting...~n~n")
    after
        timer:seconds(30) ->
            io:format("Server: No data received from client~n~n")
    end,
    ok = gen_tcp:close(Socket).
