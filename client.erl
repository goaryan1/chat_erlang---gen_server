-module(client).

-export([start/0, listen_loop/1]).
-record(client_status, {name, serverSocket, startPid}).

start() ->
    io:format("Connecting to server...~n"),
    {ok, Socket} = gen_tcp:connect('localhost', 9990, [binary, {active, true}]),
    gen_tcp:recv(Socket, 0),
    receive
        {tcp, Socket, BinaryData} ->
            Data = erlang:binary_to_term(BinaryData),
            io:format("Data from server: ~p~n", [Data]),
            ClientStatus = #client_status{serverSocket = Socket, startPid = self()},
            listen_loop(ClientStatus),
            ok;
        {tcp_closed, Socket} ->
            io:format("Connection closed~n")
    end.

listen_loop(ClientStatus) ->



    listen_loop(ClientStatus).
