-module(knitter_tr_simple).
-vc('$Id$').
-author('$Author$').
-vsn('$Revision$').

-export([start/2]).
-export([supervisor/2, listenIncoming/2, processMesg/1, attend/2]).



start(Control, Agent) ->
    case knitter_ans:get_info(Agent) of
	{ans_ok, Info} ->
	    spawn(knitter_tr_simple, supervisor, [Control, Info]);
	{ans_error, _} ->
	    exit("start/1: unable to start protocol")
    end.


supervisor(Control, Options) ->
    {value, {port, Port}} = lists:keysearch(port, 1, Options),
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    spawn(knitter_tr_simple, listenIncoming, [self(), ListenSocket]),
    ProcessPID = spawn(knitter_tr_simple, processMesg, [self()]),
    supervisor(Control, ProcessPID, []).


send_message(Connection, Process, Mesg) ->
    Process ! {self(), toString, Mesg},
    receive
	{string, StrMesg} ->
	    Connection ! {self(), sendMessage, StrMesg}
    end.


supervisor(Control, Process, Connections) ->
    receive
%-------------------- ADD A NEW CONNECTION TO LIST
	{newConection, Socket, ConnectionPID} ->
	    {ok, AddressPort} = inet:peername (Socket),
	    supervisor(Control, Process, [{AddressPort, ConnectionPID} | Connections]);
%-------------------- SEND A MESSAGE TO PEER AGENT
	{Control, sendMessage, Mesg} ->
	    {ok, Receiver} = knitter_util:get_param(Mesg, receiver),
	    case lists:keysearch(Receiver, 3, Connections) of
		{value, {_, ConnectionPID, Receiver}} ->
		    send_message(ConnectionPID, Process, Mesg),
		    supervisor(Control, Process, Connections);
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
		    supervisor(Control, Process, New_connections)
	    end;
%-------------------- WE HAVE RECEIVED A MESSAGE FROM AN AGENT
	{Connection, receiveMessage, Mesg} ->
	    Process ! {self(), toKQML, Mesg},
	    receive
		{kqml, KQMLMesg} ->
		    Control ! {self(), receiveMessage, KQMLMesg}
	    end,
	    supervisor(Control, Process, Connections);
%-------------------- IGNORE ANY OTHER MESSAGES
	_ ->
	    supervisor(Control, Process, Connections)
    end.


make_connection(Supervisor, Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, []),
    ControlPID = spawn(knitter_tr_simple, attend, [Supervisor, Socket]),
    ok = gen_tcp:controlling_process(Socket, ControlPID),
    ControlPID.


listenIncoming(Supervisor, LSocket) ->
    {ok, NewSocket} = gen_tcp:accept(LSocket),
    ControlPID = spawn(knitter_tr_simple, attend, [Supervisor, NewSocket]),
    Supervisor ! {newConection, NewSocket, ControlPID},
    ok = gen_tcp:controlling_process(NewSocket, ControlPID),
    listenIncoming(Supervisor, LSocket).


attend(Supervisor, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Supervisor ! {self(), receiveMessage, Data},
	    attend(Supervisor, Socket);
	{Supervisor, sendMessage, Mesg} ->
	    ok = gen_tcp:send(Socket, Mesg),
	    attend(Supervisor, Socket);
	_ ->
	    attend(Supervisor, Socket)
    end.


processMesg(Supervisor) ->
    receive
	{Supervisor, toString, Mesg} ->
	    Supervisor ! {string, knitter_mesg:toString(Mesg)},
	    processMesg(Supervisor);
	{Supervisor, toKQML, Mesg} ->
	    Supervisor ! {kqml, knitter_mesg:toKQML(Mesg)},
	    processMesg(Supervisor);
	_ ->
	    processMesg(Supervisor)
    end.
