-module(http).

-export([handle_request/1,
         parse_request/1,
         make_header_text/2,
         date_rfc_2616/1]).

handle_request(_Request) ->
    %% TODO: parse request
    %% TODO: 404/500 if not found
    %% TODO: MIME type after file ending
    Resource = "/",
    io:format("http: Resource: ~p~n", [Resource]),
    {ok, Content} = get_content(Resource),
    Mime = mime:get(Resource),
    io:format("http: Mime: ~p~n", [Mime]),
    ResponseHeaders = #{"Date" => date_rfc_2616(),
                        "Content-Type" => "text/html; charset=utf-8"
                       },
    Header = make_header_text(200, ResponseHeaders),
    %% io:format("http: Header0: ~p~n", [Header]),
    %% Header0 =
    %%     """
    %%     HTTP/1.0 200 OK
    %%     Date: Fri, 31 Dec 1999 23:59:59 GMT
    %%     Content-Type: text/html
    %% 
    %% 
    %%     """,
    %% io:format("http: Header: ~p~n", [Header0]),
    lists:concat([Header, binary_to_list(Content)]).

parse_request(Request) ->
    Tokens = string:split(string:trim(Request), "\r\n", all),
    [Resource | HeaderText] = lists:map(fun string:trim/1, Tokens),
    {parse_resource(Resource), parse_headers(HeaderText)}.

parse_resource(Resource) ->
    [Method, File, Protocol = "HTTP/1.1"] = string:split(Resource, " ", all),
    {Method, File, Protocol}.

parse_headers(HeaderText) ->
    HeaderPropList =
        lists:map(fun(H) ->
                          [Key, Val] = string:split(H, ": "),
                          {Key, Val}
                  end,
                  HeaderText),
    maps:from_list(HeaderPropList).

make_header_text(ResponseCode, Headers) ->
    ResponseCodeText = make_response_code(ResponseCode),
    HeaderText = [ lists:concat([Key, ": ", Val, "\r\n"])
                   || {Key, Val} <- maps:to_list(Headers) ],
    lists:flatten([ResponseCodeText | HeaderText]) ++ "\r\n".

make_response_code(200) -> "HTTP/1.0 200 OK\r\n";
make_response_code(404) -> "HTTP/1.0 400 NOT FOUND\r\n";
make_response_code(500) -> "HTTP/1.0 500 INTERNAL SERVER ERROR\r\n".

date_rfc_2616() ->
    date_rfc_2616(calendar:universal_time()).

date_rfc_2616({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    %% {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    DayOfWeek = calendar:day_of_the_week(Year, Month, Day),
    DateFormat = io_lib:format("~s, ~p ~s ~p ~.2.0w:~.2.0w:~.2.0w GMT",
                               [day_of_week(DayOfWeek), Day, month(Month),
                                Year, Hour, Min, Sec]),
    lists:flatten(DateFormat).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Okt";
month(11) -> "Nov";
month(12) -> "Dec".

day_of_week(1) -> "Mon";
day_of_week(2) -> "Tue";
day_of_week(3) -> "Wed";
day_of_week(4) -> "Thu";
day_of_week(5) -> "Fri";
day_of_week(6) -> "Sat";
day_of_week(7) -> "Sun".

date_iso_8601() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00",
                  [Year, Month, Day, Hour, Min, Sec]).

get_content("/") ->
    {ok, Data} = file:read_file("content/index.html");

get_content([$/ | FileName]) ->
    {ok, Data} = file:read_file("content/" ++ FileName);

get_content(_) ->
    {error, 404}.