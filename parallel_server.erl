-module(parallel_server).
-export([start/1, established_state/2, listen_state/2]).

start(Port) ->
    register(parallel_server, self()),
    {ok, ListenSocket} =
        gen_tcp:listen(Port,
                       [list,
                        {packet, 0},
                        {active, true},
                        {reuseaddr, true}]),
    listen_state(ListenSocket, 1).

listen_state(Socket, HandlerCount) ->
    %% Check if we've received message to shut down server, otherwise listen for connections.
    Pid = self(),
    receive
        stop ->
            io:format("ListeningServer~p: Received 'stop' - shutting down...~n~n", [Pid]),
            ok = gen_tcp:close(Socket),
            exit(normal)
    after
        0 ->
            {ok, {ListenIp, ListenPort}} = inet:sockname(Socket),
            ok = io:format("ListeningServer~p: Listening on IP ~p port ~p~n",
                           [Pid, ListenIp, ListenPort])
    end,
    case gen_tcp:accept(Socket, timer:seconds(30)) of
        %% A client connected, handle request.
        {ok, EstablishedSocket} ->
            {ok, {LocalIp, LocalPort}} = inet:sockname(EstablishedSocket),
            {ok, {PeerIp, PeerPort}} = inet:peername(EstablishedSocket),
            %% FIXME I'm sure there is a tidier way...
            HandlerName = lists:flatten("Server" ++ io_lib:format("~3..0w", [HandlerCount])),
            io:format("ListeningServer~p: Accepted connection:~n  send IP ~p port ~p~n\t"
                      "  recv IP ~p port ~p - Spawning handler...~n",
                      [Pid, PeerIp, PeerPort, LocalIp, LocalPort]),
            HandlerPid = spawn_link(?MODULE, established_state, [EstablishedSocket, HandlerName]),
            %% Whichever process accepts TCP connection owns the socket, and also gets
            %% any data client sends, so need to handover socket to handler process.
            gen_tcp:controlling_process(EstablishedSocket, HandlerPid);
        %% No client connected for a while, time out so we can check for stop
        %% message in next loop iteration.
        {error, timeout} ->
            io:format("ListeningServer~p: No client request to handle...~n~n", [Pid])
    end,
    ?MODULE:listen_state(Socket, HandlerCount + 1).

established_state(Socket, ServerName) ->
    Pid = self(),
    receive
       {tcp, Socket, StringMsg} ->
            io:format("~p ~p: Received message: ~p~n", [Pid, ServerName, StringMsg]),
            io:format("~p ~p: Working on request...~n", [Pid, ServerName]),
            %% Fake doing some work that takes time.
            timer:sleep(timer:seconds(3)),
            Reply = "Echo " ++ StringMsg,
            io:format("~p ~p: Replying: ~p~n~n", [Pid, ServerName, Reply]),
            ok = gen_tcp:send(Socket, Reply);
        {tcp_closed, Socket} ->
            io:format("~p ~p: Client closed socket, shutting down...~n~n", [Pid, ServerName])
    after
        timer:seconds(30) ->
            io:format("~p ~p: No data received from client~n~n", [Pid, ServerName])
    end,
    ok = gen_tcp:close(Socket).
