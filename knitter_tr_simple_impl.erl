-module(knitter_tr_simple_impl).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([server/1, listenIncoming/2, server_mesg_conv/1, attend/2]).
%-export([server_mesg_spool/3]).

-include("knitter_tr_simple_impl.hrl").



start(Agent) ->
    case knitter_ans:get_info(Agent) of
	{ans_ok, Info} ->
	    spawn_link(?MODULE, server, [Info]);
	{ans_error, _} ->
	    exit("start/1: unable to start protocol")
    end.


server(State) when record(State, server_state) ->
    receive
%-------------------- ADD A NEW CONNECTION TO LIST
	{newConection, Socket, ConnectionPID} ->
	    {ok, AddressPort} = inet:peername (Socket),
	    link(ConnectionPID),
	    server(State#server_state{connections = [{AddressPort, ConnectionPID} | State#server_state.connections]});
%-------------------- SEND A MESSAGE TO PEER AGENT
	{sendMessage, Mesg} ->
	    {ok, Receiver} = knitter_mesg:get_param(Mesg, receiver),
	    case lists:keysearch(Receiver, 3, State#server_state.connections) of
		{value, {_, ConnectionPID, Receiver}} ->
		    New_mesg_conv = send_message(ConnectionPID, State#server_state.conversor, Mesg),
		    server(State#server_state{conversor = New_mesg_conv});
		false ->
		    {ans_ok, Info} = knitter_ans:get_info(Receiver),
		    {value, {address, Address}} = lists:keysearch(address, 1, Info),
		    {value, {port, Port}} = lists:keysearch(port, 1, Info),
		    {Conn, Conns} = case lists:keysearch({Address, Port}, 1, State#server_state.connections) of
					{value, {_, Connection}} ->
					    New_connections = lists:keyreplace(Connection, 2, State#server_state.connections, {{Address, Port}, Connection, Receiver}),
					    {Connection, New_connections};
					false ->
					    Connection = make_connection(self(), Address, Port),
					    New_connections = [{{Address, Port}, Connection, Receiver} | State#server_state.connections],
					    {Connection, New_connections}
				    end,
		    New_mesg_conv = send_message(Conn, State#server_state.conversor, Mesg),
		    server(State#server_state{conversor = New_mesg_conv, connections = Conns})
	    end;
%-------------------- WE HAVE RECEIVED A MESSAGE FROM AN AGENT
	{Connection, receiveMessage, Mesg} ->
	    State#server_state.conversor ! {self(), toKQML, Mesg},
	    receive
		{kqml, KQMLMesg} ->
		    knitter:receive_message(KQMLMesg),
		    server(State);
		{'EXIT', Mesg_conv, _} ->
		    %%Hubo un error al convertir el mensaje. Hay que notificarlo
		    New_mesg_conv = spawn_link(?MODULE, server_mesg_conv, [self()]),
		    server(State#server_state{conversor = New_mesg_conv})
	    end;
%-------------------- A PROCESS HAS DIED
	{'EXIT', From, Reason} ->
	    New_connections = case lists:keysearch(From, 2, State#server_state.connections) of
				  {value, {_, Connection}} ->
				      lists:keydelete(From, 2, State#server_state.connections);
				  false ->
				      State#server_state.connections
			      end,
	    server(State#server_state{connections = New_connections});
%-------------------- IGNORE ANY OTHER MESSAGES
	_ ->
	    server(State)
    end;

server(Options) ->
    {value, {port, Port}} = lists:keysearch(port, 1, Options),
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    ListenPID = spawn_link(?MODULE, listenIncoming, [self(), ListenSocket]),
    Mesg_convPID = spawn_link(?MODULE, server_mesg_conv, [self()]),
    process_flag(trap_exit, true),
    server(#server_state{listen = ListenPID, conversor = Mesg_convPID}).


listenIncoming(Server, LSocket) ->
    {ok, NewSocket} = gen_tcp:accept(LSocket),
    ControlPID = spawn(?MODULE, attend, [Server, NewSocket]),
    Server ! {newConection, NewSocket, ControlPID},
    ok = gen_tcp:controlling_process(NewSocket, ControlPID),
    listenIncoming(Server, LSocket).


attend(Server, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Server ! {self(), receiveMessage, Data},
	    attend(Server, Socket);
	{Server, sendMessage, Mesg} ->
	    ok = gen_tcp:send(Socket, Mesg),
	    attend(Server, Socket);
	{tcp_closed, Socket} ->
	    exit(normal);
	{tcp_error, Socket, Reason} ->
	    gen_tcp:close(Socket),
	    exit(Reason);
	_ ->
	    attend(Server, Socket)
    end.


server_mesg_conv(Server) ->
    receive
	{Server, toString, Mesg} ->
	    Server ! {string, knitter_mesg_conv:kqml_to_ascii(Mesg)},
	    server_mesg_conv(Server);
	{Server, toKQML, Mesg} ->
	    Server ! {kqml, knitter_mesg_conv:ascii_to_kqml(Mesg)},
	    server_mesg_conv(Server);
	_ ->
	    server_mesg_conv(Server)
    end.


send_message(Connection, Mesg_conv, Mesg) ->
    Mesg_conv ! {self(), toString, Mesg},
    receive
	{string, StrMesg} ->
	    Connection ! {self(), sendMessage, StrMesg},
	    Mesg_conv;
	{'EXIT', Mesg_conv, _} ->
	    %%Hubo un error al convertir el mensaje. Hay que notificarlo
	    spawn_link(?MODULE, server_mesg_conv, [self()])
    end.


make_connection(Server, Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, []),
    ControlPID = spawn_link(?MODULE, attend, [Server, Socket]),
    ok = gen_tcp:controlling_process(Socket, ControlPID),
    ControlPID.










%================================================================================
%================================================================================
%================================================================================
%================================================================================
%================================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%top(Queue) ->
%    case queue:to_list(Queue) of
%	[] ->
%	    empty;
%	[H | _] ->
%	    {value, H}
%    end.
%
%
%start_mesg_spool(Server, Socket) ->
%    spawn(knitter_tr_simple, server_mesg_spool_init, [Server, Socket]).
%
%
%server_mesg_spool_init(Server, Socket) ->
%    process_flag(trap_exit, true),
%    ControlPID = spawn_link(knitter_tr_simple, attend, [Server, Socket]),
%    ok = gen_tcp:controlling_process(Socket, ControlPID),
%    server_mesg_spool(Server, queue:new(), ControlPID).
%
%    
%server_mesg_spool(Server, Buffer, ConnectionPID) ->
%    receive
%	{Server, sendMessage, Message} ->
%	    New_buffer = queue:in(Message, Buffer),
%	    server_mesg_spool(Server, New_buffer, ConnectionPID);
%	{ConnectionPID, getMessage} ->
%	    case top(Buffer) of
%		{value, H} ->
%		    ConnectionPID ! {message, H},
%		    server_mesg_spool(Server, Buffer, ConnectionPID);
%		empty ->
%		    server_mesg_spool_wait(Server, Buffer, ConnectionPID)
%	    end;
%	{ConnectionPID, drop_and_get} ->
%	    {_, New_buffer} = queue:out(Buffer),
%	    case top(Buffer) of
%		{value, H} ->
%		    ConnectionPID ! {message, H},
%		    server_mesg_spool(Server, New_buffer, ConnectionPID);
%		empty ->
%		    server_mesg_spool_wait(Server, New_buffer, ConnectionPID)
%	    end;
%	{'EXIT', ConnectionPID, _} ->
%	    New_control = spawn_link(knitter_tr_simple, attend, [Serv
%	    ok;%HAY QUE CORREGIR ESTO
%	_ ->
%	    server_mesg_spool(Server, Buffer, ConnectionPID)
%    end.
%
%
%server_mesg_spool_wait(Server, Buffer, ConnectionPID) ->
%    receive
%	{Server, sendMessage, Message} ->
%	    New_buffer = queue:in(Message, Buffer),
%	    ConnectionPID ! {message, Message},
%	    server_mesg_spool(Server, Buffer, ConnectionPID)
%    end.
