-module(proxy).
-export([start/0, handle_request/1, listener_process/0, wait_connect/1]).

% Simple HTTP proxy server (spawns a new process for each connection).
% Reloads if module is updated.

start() ->
    inets:start(), % start inet module for HTTP requests.
    % Spawn listener
    spawn(?MODULE, listener_process, []).

listener_process() ->
    { ok, ListenSocket } = gen_tcp:listen(8080, [binary, {active, false}]),
    wait_connect(ListenSocket).

wait_connect(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    _Pid = spawn(?MODULE, handle_request, [Socket]),
    ?MODULE:wait_connect(ListenSocket).

handle_request(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    io:format("~s~n", [Packet]),
    [FirstLine | Request] = binary:split(Packet, <<"~n">>),
    [Verb, Url | _Version] = binary:split(FirstLine, <<" ">>, [global]),
    handle_verb(Verb, binary:bin_to_list(Url), Request, Socket).

handle_verb(<<"GET">>, Url, _Request, Socket) ->
    io:format("GET for ~s~n", [Url]),
    {ok, {{_Version, 200, _ReasonPhrase}, Headers, Body}} =  httpc:request(Url),
    gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\n"),
    send_headers(Socket, Headers),
    gen_tcp:send(Socket, "\r\n"),
    gen_tcp:send(Socket, Body),
    gen_tcp:close(Socket).

send_headers(_Socket, []) ->
    ok;
send_headers(Socket, [{Header, Value} | Tail]) ->
    gen_tcp:send(Socket, Header),
    gen_tcp:send(Socket, ": "),
    gen_tcp:send(Socket, Value),
    gen_tcp:send(Socket, "\r\n"),
    send_headers(Socket, Tail).
