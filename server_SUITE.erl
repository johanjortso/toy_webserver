-module(server_SUITE).
-compile([export_all, nowarn_export_all]).

all() -> [{group, GroupName} || {GroupName, _Opt, List} <- groups(), length(List) > 0].

groups() ->
    Grp1 = [
        tc_server_onetime_request,
        tc_server_multiple_client_requests
    ],
    [{grp1, [], Grp1}].

init_per_suite(Config) ->
    Config ++ [{host, "localhost"},
               {port, 7777}].

end_per_suite(_Config) ->
    ok.

tc_server_onetime_request(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    spawn_link(server, start, [Port]),
    {tcp, _Socket, "Echo Hello"} = client:start(Host, Port),
    ok.

tc_server_multiple_client_requests(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    spawn_link(server, start, [Port]),
    {tcp, Socket, "Echo Hello"} = client:start(Host, Port),
    {tcp, Socket, "Echo Hello"} = client:start(Host, Port),
    ok.
