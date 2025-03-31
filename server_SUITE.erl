-module(server_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

all() -> [{group, GroupName} || {GroupName, _Opt, List} <- groups(), length(List) > 0].

groups() ->
    ServerTests = [
        tc_server_onetime_request,
        tc_server_multiple_client_requests,
        tc_server_parallell_client_requests
    ],
    Http = [tc_http_split_headers,
               tc_http_make_header_text,
               tc_rfc_2616_date],
    [{server_tests, [], ServerTests},
     {http, [parallel], Http}].

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

tc_http_split_headers(_Config) ->
    Request =
        "GET /path/to/file.html HTTP/1.1\r\nFrom: someuser@example.com\r\nUse"
        "r-Agent: Toy Client\r\n\r\n",
    {Resource, Headers} = http:parse_request(Request),
    ct:pal("Resource ~p~n", [Resource]),
    ct:pal("Headers ~p~n", [Headers]),
    ?assertEqual({"GET", "/path/to/file.html", "HTTP/1.1"}, Resource),
    ?assertEqual(#{
                    "From" => "someuser@example.com",
                    "User-Agent" => "Toy Client"
                },
                Headers),
    ok.
    
tc_http_make_header_text(_Config) ->
    HeaderText =
        http:make_header_text(200,
                              #{
                                "From" => "someuser@example.com",
                                "User-Agent" => "Toy Client"
                               }),
    Expected = "HTTP/1.0 200 OK\r\nFrom: someuser@example.com\r\nUser-Agent: Toy Client\r\n\r\n",
    ?assertEqual(Expected, HeaderText),
    ok.

    
tc_rfc_2616_date(_Config) ->
    Expected = "Fri, 31 Dec 1999 23:59:58 GMT",
    RfcDate = http:date_rfc_2616({{1999, 12, 31}, {23, 59, 58}}),
    ?assertEqual(Expected, RfcDate),
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
