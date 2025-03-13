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
    spawn_link(server, start, [7777]),
    {tcp, _Socket, "Echo hello"} = client:start(),
    ok.

tc_server_multiple_client_requests(_Config) ->
    spawn_link(server, start, [7777]),
    {tcp, Socket, "Echo hello"} = client:start(),
    {tcp, Socket, "Echo hello"} = client:start(),
    ok.
