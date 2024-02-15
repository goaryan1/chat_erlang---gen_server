% handle_call(Data, _From, State) ->
%     case Data of
-module(server).

-behaviour(gen_server).

-record(client, {clientSocket, clientName}).
-record(server_status, {listenSocket}).
%% API
-export([start_link/0, accept_clients/1]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
-define(SERVER, ?MODULE).
 
%%%===================================================================
%%% API
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Initialising Server ~n"),
    {ok, ListenSocket} = gen_tcp:listen(9990, [binary, {active, true}]),
    database_init(),
    ServerStatus = #server_status{listenSocket = ListenSocket},
    spawn(server, accept_clients, [ListenSocket]),
    {ok, ServerStatus}.

database_init() ->
    mnesia:start(),
    mnesia:create_table(client, [{attributes, record_info(fields, client)}]),
    mnesia:create_table(server_status, [{attributes, record_info(fields, server_status)}]).



accept_clients(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    gen_tcp:send(ClientSocket, term_to_binary({success, node(), "Succesfully joined !!"})),
    mnesia:transaction(fun() -> mnesia:write(#client{clientSocket = ClientSocket, clientName = "User"}) end),
    io:format("Client Accepted~n"),
    accept_clients(ListenSocket).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
% handle_call(_Request, _From, ServerStatus) ->
%     Reply = ok,
%     {reply, Reply, ServerStatus};

handle_call(Request, _From, ServerStatus) ->
    {message, Message} = Request,
    io:format("~p~n", [Message]),
    Reply = "Message received at server",
    {reply, Reply, ServerStatus}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%%===================================================================
%%% Internal functions
%%%=======================
