-module(knitter_tr_simple).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/2]).
-export([server/2, listenIncoming/2, processMesg/1, attend/2]).



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
    spawn(knitter_tr_simple, listenIncoming, [self(), ListenSocket]),
    ProcessPID = spawn(knitter_tr_simple, processMesg, [self()]),
    server(Control, ProcessPID, []).


send_message(Connection, Process, Mesg) ->
    Process ! {self(), toString, Mesg},
    receive
	{string, StrMesg} ->
	    Connection ! {self(), sendMessage, StrMesg}
    end.


server(Control, Process, Connections) ->
    receive
%-------------------- ADD A NEW CONNECTION TO LIST
	{newConection, Socket, ConnectionPID} ->
	    {ok, AddressPort} = inet:peername (Socket),
	    server(Control, Process, [{AddressPort, ConnectionPID} | Connections]);
%-------------------- SEND A MESSAGE TO PEER AGENT
	{Control, sendMessage, Mesg} ->
	    {ok, Receiver} = knitter_mesg:get_param(Mesg, receiver),
	    case lists:keysearch(Receiver, 3, Connections) of
		{value, {_, ConnectionPID, Receiver}} ->
		    send_message(ConnectionPID, Process, Mesg),
		    server(Control, Process, Connections);
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
		    send_message(Connection, Process, Mesg),
		    server(Control, Process, New_connections)
	    end;
%-------------------- WE HAVE RECEIVED A MESSAGE FROM AN AGENT
	{Connection, receiveMessage, Mesg} ->
	    Process ! {self(), toKQML, Mesg},
	    receive
		{kqml, KQMLMesg} ->
		    Control ! {self(), receiveMessage, KQMLMesg}
	    end,
	    server(Control, Process, Connections);
%-------------------- IGNORE ANY OTHER MESSAGES
	_ ->
	    server(Control, Process, Connections)
    end.


make_connection(Server, Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, []),
    ControlPID = spawn(knitter_tr_simple, attend, [Server, Socket]),
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


processMesg(Server) ->
    receive
	{Server, toString, Mesg} ->
	    Server ! {string, knitter_mesg_conv:kqml_to_string(Mesg)},
	    processMesg(Server);
	{Server, toKQML, Mesg} ->
	    Server ! {kqml, knitter_mesg_conv:string_to_kqml(Mesg)},
	    processMesg(Server);
	_ ->
	    processMesg(Server)
    end.
