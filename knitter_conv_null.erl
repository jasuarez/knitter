-module(knitter_conv_null).
-author('$Author$').
-vsn('$Revision$').



-export([start/3]).
-export([server/3]).


start(Control, Local_agent, Remote_agent) ->
    spawn(knitter_conv_null, server, [Control, Local_agent, Remote_agent]).


server(Control, Local_agent, Remote_agent) ->
    receive
%-------------------- SEND A KQML MESSAGE TO REMOTE AGENT
	{From, sendMessage, Message} ->
	    Control ! {self(), sendMessage, Message},
	    server(Control, Local_agent, Remote_agent);
%-------------------- IGNORE OTHER ERLANG MESSAGES	    
	_ ->
	    server(Control, Local_agent, Remote_agent)
    end.
