-module(knitter_tr_simple).
-author('$Author$').
-vsn('$Revision$').

-export([start/1]).
-export([supervisor/2, listenIncoming/2, processMesg/1, attend/2]).



start(Control) ->
    case file:consult('knitter_tr_simple.cfg') of
	{ok, Options} ->
	    spawn(knitter_tr_simple, supervisor, [Control, Options]);
	{error, _} ->
	    exit("start/1: unable to open/read configuration file")
    end.


supervisor(Control, Options) ->
    {value, {port, Port}} = lists:keysearch(port, 1, Options),
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    spawn(knitter_tr_simple, listenIncoming, [self(), ListenSocket]),
    ProcessPID = spawn(knitter_tr_simple, processMesg, [self()]),
    supervisor(Control, ProcessPID, []).


supervisor(Control, Process, Connections) ->
    receive
	{newConection, Socket, ConnectionPID} ->
	    {ok, AddressPort} = inet:peername (Socket),
	    supervisor(Control, Process, [{AddressPort, ConnectionPID} | Connections]);
	{Control, sendMessage, Mesg} ->
	    {_, Connection} = hd(Connections),         %HAY QUE BUSCARLA BIEN
	    Process ! {self(), toString, Mesg},
	    receive
		{string, StrMesg} ->
		    Connection ! {self(), sendMessage, StrMesg}
	    end,
	    supervisor(Control, Process, Connections);
	{Connection, receiveMessage, Mesg} ->
	    Process ! {self(), toKQML, Mesg},
	    receive
		{kqml, KQMLMesg} ->
		    Control ! {receiveMessage, KQMLMesg}
	    end,
	    supervisor(Control, Process, Connections);
	_ ->
	    supervisor(Control, Process, Connections)
    end.

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
