-module(http_server).
-compile([export_all, nowarn_export_all]).

start(Port) ->
    Pid = self(),
    register(toy_webserver, Pid),
    {ok, ListenSocket} =
        gen_tcp:listen(Port,
                       [list,
                        {packet, 0},
                        {active, true},
                        {reuseaddr, true}]),
    connect_loop(ListenSocket),
    receive
        stop ->
            ok = gen_tcp:close(ListenSocket),
            io:format("Server~p: stopped...~n~n", [Pid])
    end.

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
            spawn_link(?MODULE, connect_loop, [ListenSocket]),
            handle_connection(Socket);
        %% No client connected for a while, time out so we can check for stop message in next loop iteration.
        {error, timeout} ->
            io:format("Server~p: no client request to handle...~n~n", [Pid]),
            spawn_link(?MODULE, connect_loop, [ListenSocket]);
        {error, closed} ->
            io:format("Server~p: Listening socket closed, shutting down...~n~n", [Pid])
    end.

handle_connection(Socket) ->
    Pid = self(),
    receive
       {tcp, Socket, StringMsg} ->
            io:format("Server~p: Received message: ~p~n", [Pid, StringMsg]),
            io:format("Server~p: working on request...~n", [Pid]),
            Reply = http:handle_request(StringMsg),
            io:format("Server~p: Replying...~n~n", [Pid]),
            %% io:format("Server~p: Replying: ~p~n~n", [Pid, Reply]),
            ok = gen_tcp:send(Socket, Reply);
        {tcp_closed, Socket} ->
            io:format("Server~p: Client closed socket, aborting...~n~n", [Pid])
    after
        timer:seconds(30) ->
            io:format("Server~p: No data received from client~n~n", [Pid])
    end,
    ok = gen_tcp:close(Socket).
