-module(server_SUITE).
-compile([export_all, nowarn_export_all]).

all() -> [{group, GroupName} || {GroupName, _Opt, List} <- groups(), length(List) > 0].

groups() ->
    Grp1 = [
        tc_server_onetime_request,
        tc_server_multiple_client_requests
    ],
    [{grp1, [], Grp1}].

tc_server_onetime_request(_Config) ->
    Host = "localhost",
    Port = 7777,
    spawn_link(server, start, [Port]),
    {tcp, _Socket, "Echo Hello"} = client:start(Host, Port),
    ok.

tc_server_multiple_client_requests(_Config) ->
    Host = "localhost",
    Port = 7777,
    spawn_link(server, start, [Port]),
    {tcp, Socket, "Echo Hello"} = client:start(Host, Port),
    {tcp, Socket, "Echo Hello"} = client:start(Host, Port),
    ok.
