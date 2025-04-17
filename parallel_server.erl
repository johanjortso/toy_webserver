-module(parallel_server).
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
    Pid = self(),
    %% Check if we've received message to shut down server, otherwise listen for connections.
    receive
        stop ->
            io:format("Server~p: shutting down...~n~n", [Pid]),
            ok = gen_tcp:close(ListenSocket),
            exit(normal)
    after
        0 ->
            {ok, {ListenIp, ListenPort}} = inet:sockname(ListenSocket),
            ok = io:format("Server~p: Listening on IP ~p port ~p~n", [Pid, ListenIp, ListenPort])
    end,
    case gen_tcp:accept(ListenSocket, timer:seconds(30)) of
        %% A client connected, handle request.
        {ok, Socket} ->
            {ok, {LocalIp, LocalPort}} = inet:sockname(Socket),
            {ok, {PeerIp, PeerPort}} = inet:peername(Socket),
            io:format("Server~p: Accepted connection from IP ~p port ~p on IP ~p port ~p~n",
                      [Pid, PeerIp, PeerPort, LocalIp, LocalPort]),
            spawn(?MODULE, handle_connection, [Socket]);
        %% No client connected for a while, time out so we can check for stop message in next loop iteration.
        {error, timeout} ->
            io:format("Server~p: no client request to handle...~n~n", [Pid])
    end,
    connect_loop(ListenSocket).

handle_connection(Socket) ->
    Pid = self(),
    receive
       {tcp, Socket, StringMsg} ->
            io:format("Server~p: Received message: ~p~n", [Pid, StringMsg]),
            io:format("Server~p: working on request...~n", [Pid]),
            %% Fake doing some work that takes time.
            timer:sleep(timer:seconds(3)),
            Reply = "Echo " ++ StringMsg,
            io:format("Server~p: Replying: ~p~n~n", [Pid, Reply]),
            ok = gen_tcp:send(Socket, Reply);
        {tcp_closed, Socket} ->
            io:format("Server~p: Client closed socket, aborting...~n~n", [Pid])
    after
        timer:seconds(30) ->
            io:format("Server~p: No data received from client~n~n", [Pid])
    end,
    ok = gen_tcp:close(Socket).
