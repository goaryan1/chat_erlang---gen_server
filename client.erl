-module(client).
 
-export([start/0, listen_loop/1, send_message/0]).
-record(client_status, {name, serverSocket, startPid, serverNode}).
% -define(SERVER, server).
 
start() ->
    io:format("?MODULE = ~p~n", [?MODULE]),
    io:format("Connecting to server...~n"),
    {ok, Socket} = gen_tcp:connect('localhost', 9990, [binary, {active, true}]),
    gen_tcp:recv(Socket, 0),
    receive
        {tcp, Socket, BinaryData} ->
            Data = erlang:binary_to_term(BinaryData),
            io:format("Data from server: ~p~n", [Data]),
            {_,ServerNode,_} = Data,
            ClientStatus = #client_status{serverSocket = Socket, startPid = self(), serverNode = ServerNode},
            SpawnedPid = spawn(fun() -> listen_loop(ClientStatus) end),
            put(spawnedPid, SpawnedPid),
            % listen_loop(ClientStatus),
            ok;
        {tcp_closed, Socket} ->
            io:format("Connection closed~n")
    end.
 
listen_loop(ClientStatus) ->
    Socket = ClientStatus#client_status.serverSocket,
    StartPid = ClientStatus#client_status.startPid,
    % ServerNode = ClientStatus#client_status.serverNode,
    receive
        {tcp, Socket, BinaryData} ->
            Data = binary_to_term(BinaryData),
            case Data of
                _ ->
                    io:format("Undefined message received~n")
            end;
        {tcp_closed, Socket} ->
            io:format("Connection closed~n"),
            ok;
        {StartPid, Data} ->
            io:format("~p~n",[Data]),
            case Data of
                {message, Message} ->
                    Request = {message, Message},
                    io:format("message request sent~n"),
                    % ServerNode = ,
                    Response = gen_server:call({server, server@ggn001687}, Request),
                    io:format("~p~n", [Response]),
                    ok;
                    % gen_tcp:send(Socket, BinaryData);
                _ ->
                    io:format("Undefined internal message received~n")
            end
    end,
 
    listen_loop(ClientStatus).
 
send_message() ->
    Message = string:trim(io:get_line("Enter message: ")),
    StartPid = self(),
    SpawnedPid = get(spawnedPid),
    io:format("Sending Message to local ~n"),
    SpawnedPid ! {StartPid, {message, Message}},
    ok.
 
 
