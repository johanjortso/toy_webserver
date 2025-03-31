-module(server_SUITE).
-compile([export_all, nowarn_export_all]).

all() -> [{group, GroupName} || {GroupName, _Opt, List} <- groups(), length(List) > 0].

groups() ->
    ServerTests = [
        tc_server_onetime_request,
        tc_server_multiple_client_requests,
        tc_server_parallell_client_requests
    ],
    [{server_tests, [], ServerTests}].

init_per_suite(Config) ->
    Config ++ [{host, "localhost"},
               {port, 7777}].

end_per_suite(_Config) ->
    ok.

tc_server_onetime_request(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    spawn_link(single_req_server, start, [Port]),
    {tcp, _Socket, "Echo Hello"} = client:start(Host, Port),
    ok.

tc_server_multiple_client_requests(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    spawn_link(multi_req_server, start, [Port]),
    {tcp, _Socket1, "Echo Hello"} = client:start(Host, Port),
    {tcp, _Socket2, "Echo Hello"} = client:start(Host, Port),
    toy_webserver ! stop,
    {tcp, _, "Echo Hello"} = client:start(Host, Port),
    ok.

tc_server_parallell_client_requests(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    spawn_link(parallel_server, start, [Port]),
    TcPid = self(),
    NoOfRequests = 50,
    [ spawn_link(fun() ->
                    timer:sleep(N * 10),
                    Reply = client:start(Host, Port),
                    TcPid ! Reply
            end)
      || N <- lists:seq(1, NoOfRequests) ],
    receive_replies(NoOfRequests),
    ct:pal("Got replies from server for all client requests."),
    toy_webserver ! stop,
    ok.

%% Helper functions
receive_replies(_N = 0) ->
        ok;
receive_replies(N) when is_integer(N), N > 0 ->
    receive
        {tcp, _Socket, "Echo Hello"} ->
            ct:pal("~p replies left~n", [N])
    after
        timer:seconds(5) ->
            ct:fail("Did not get reply from request ~p~n", [N])
    end,
    receive_replies(N - 1).
