-module(knitter_tr_simple).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/2]).
-export([server/2, listenIncoming/2, server_mesg_conv/1, attend/2, server_mesg_spool/3]).



start(Control, Agent) ->
    case knitter_ans:get_info(Agent) of
	{ans_ok, Info} ->
	    spawn(knitter_tr_simple, server, [Control, Info]);
	{ans_error, _} ->
	    exit("start/1: unable to start protocol")
    end.


server(Control, Options) ->
    {value, {port, Port}} = lists:keysearch(port, 1, Options),
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    spawn_link(knitter_tr_simple, listenIncoming, [self(), ListenSocket]),
    Mesg_convPID = spawn_link(knitter_tr_simple, server_mesg_conv, [self()]),
    process_flag(trap_exit, true),
    server(Control, Mesg_convPID, []).


send_message(Connection, Mesg_conv, Mesg) ->
    Mesg_conv ! {self(), toString, Mesg},
    receive
	{string, StrMesg} ->
	    Connection ! {self(), sendMessage, StrMesg},
	    Mesg_conv;
	{'EXIT', Mesg_conv, _} ->
	    %%Hubo un error al convertir el mensaje. Hay que notificarlo
	    spawn_link(knitter_tr_simple, server_mesg_conv, [self()])
    end.


server(Control, Mesg_conv, Connections) ->
    receive
%-------------------- ADD A NEW CONNECTION TO LIST
	{newConection, Socket, ConnectionPID} ->
	    {ok, AddressPort} = inet:peername (Socket),
	    link(ConnectionPID),
	    server(Control, Mesg_conv, [{AddressPort, ConnectionPID} | Connections]);
%-------------------- SEND A MESSAGE TO PEER AGENT
	{Control, sendMessage, Mesg} ->
	    {ok, Receiver} = knitter_mesg:get_param(Mesg, receiver),
	    case lists:keysearch(Receiver, 3, Connections) of
		{value, {_, ConnectionPID, Receiver}} ->
		    New_mesg_conv = send_message(ConnectionPID, Mesg_conv, Mesg),
		    server(Control, New_mesg_conv, Connections);
		false ->
		    {ans_ok, Info} = knitter_ans:get_info(Receiver),
		    {value, {address, Address}} = lists:keysearch(address, 1, Info),
		    {value, {port, Port}} = lists:keysearch(port, 1, Info),
		    case lists:keysearch({Address, Port}, 1, Connections) of
			{value, {_, Connection}} ->
			    New_connections = lists:keyreplace(Connection, 2, Connections, {{Address, Port}, Connection, Receiver}),
			    Connection;
			false ->
			    Connection = make_connection(self(), Address, Port),
			    New_connections = [{{Address, Port}, Connection, Receiver} | Connections],
			    Connection
		    end,
		    New_mesg_conv = send_message(Connection, Mesg_conv, Mesg),
		    server(Control, New_mesg_conv, New_connections)
	    end;
%-------------------- WE HAVE RECEIVED A MESSAGE FROM AN AGENT
	{Connection, receiveMessage, Mesg} ->
	    Mesg_conv ! {self(), toKQML, Mesg},
	    receive
		{kqml, KQMLMesg} ->
		    Control ! {self(), receiveMessage, KQMLMesg},
		    New_mesg_conv = Mesg_conv;
		{'EXIT', Mesg_conv, _} ->
		    %%Hubo un error al convertir el mensaje. Hay que notificarlo
		    New_mesg_conv = spawn_link(knitter_tr_simple, server_mesg_conv, [self()])
	    end,
	    server(Control, New_mesg_conv, Connections);
%-------------------- A PROCESS HAS DIED
	{'EXIT', From, Reason} ->
	    server(Control, Mesg_conv, Connections);
%-------------------- IGNORE ANY OTHER MESSAGES
	_ ->
	    server(Control, Mesg_conv, Connections)
    end.


make_connection(Server, Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, []),
    ControlPID = spawn_link(knitter_tr_simple, attend, [Server, Socket]),
    ok = gen_tcp:controlling_process(Socket, ControlPID),
    ControlPID.


listenIncoming(Server, LSocket) ->
    {ok, NewSocket} = gen_tcp:accept(LSocket),
    ControlPID = spawn(knitter_tr_simple, attend, [Server, NewSocket]),
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
	_ ->
	    attend(Server, Socket)
    end.


server_mesg_conv(Server) ->
    receive
	{Server, toString, Mesg} ->
	    Server ! {string, knitter_mesg_conv:kqml_to_string(Mesg)},
	    server_mesg_conv(Server);
	{Server, toKQML, Mesg} ->
	    Server ! {kqml, knitter_mesg_conv:string_to_kqml(Mesg)},
	    server_mesg_conv(Server);
	_ ->
	    server_mesg_conv(Server)
    end.


top(Queue) ->
    case queue:to_list(Queue) of
	[] ->
	    empty;
	[H | _] ->
	    {value, H}
    end.


start_mesg_spool(Server, Socket) ->
    spawn(knitter_tr_simple, server_mesg_spool_init, [Server, Socket]).


server_mesg_spool_init(Server, Socket) ->
    process_flag(trap_exit, true),
    ControlPID = spawn_link(knitter_tr_simple, attend, [Server, Socket]),
    ok = gen_tcp:controlling_process(Socket, ControlPID),
    server_mesg_spool(Server, queue:new(), ControlPID).

    
server_mesg_spool(Server, Buffer, ConnectionPID) ->
    receive
	{Server, sendMessage, Message} ->
	    New_buffer = queue:in(Message, Buffer),
	    server_mesg_spool(Server, New_buffer, ConnectionPID);
	{ConnectionPID, getMessage} ->
	    case top(Buffer) of
		{value, H} ->
		    ConnectionPID ! {message, H},
		    server_mesg_spool(Server, Buffer, ConnectionPID);
		empty ->
		    server_mesg_spool_wait(Server, Buffer, ConnectionPID)
	    end;
	{ConnectionPID, drop_and_get} ->
	    {_, New_buffer} = queue:out(Buffer),
	    case top(Buffer) of
		{value, H} ->
		    ConnectionPID ! {message, H},
		    server_mesg_spool(Server, New_buffer, ConnectionPID);
		empty ->
		    server_mesg_spool_wait(Server, New_buffer, ConnectionPID)
	    end;
	{'EXIT', ConnectionPID, _} ->
	    ok;%HAY QUE CORREGIR ESTO
	_ ->
	    server_mesg_spool(Server, Buffer, ConnectionPID)
    end.


server_mesg_spool_wait(Server, Buffer, ConnectionPID) ->
    receive
	{Server, sendMessage, Message} ->
	    New_buffer = queue:in(Message, Buffer),
	    ConnectionPID ! {message, Message},
	    server_mesg_spool(Server, Buffer, ConnectionPID)
    end.
