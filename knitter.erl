-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([start/1, server/3]).



start (AgentName) ->
    case file:consult('knitter.cfg') of
	{ok, Ports} ->
	    ListPorts = lists:map(fun({Protocol, Module}) -> {Protocol, apply(Module, start, [])} end, Ports),
	    spawn(knitter, server, [AgentName, dict:new(), dict:from_list(ListPorts)]);
	{error, _} ->
	    exit ("start/1: unable to open configuration file")
    end.


server (AgentName, Conversations, Ports) ->
    receive
	{From, createConversation, Conv, WithAgent} ->
	    case catch apply(Conv, start, [AgentName, WithAgent]) of
		PidConv when pid(PidConv) ->
		    From ! {ok, PidConv},
		    server(AgentName, dict:append({WithAgent, null}, PidConv, Conversations), Ports);
		_ ->
		    From ! {error, "Unable to create conversation"},
		    server(AgentName, Conversations, Ports)
	    end;
	_ ->
	    server(AgentName, Conversations, Ports)
    end.
