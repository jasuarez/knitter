-module(knitter_conv_null).
-vc('$Id$').
-author('$Author$').
-vsn('$Revision$').

-export([start/3]).
-export([server/3]).



start(Control, Local_agent, Remote_agent) ->
    spawn(knitter_conv_null, server, [Control, queue:new(), q_new()]).


server(Control, QMesg, QWaiting) ->
    receive
%-------------------- SEND A KQML MESSAGE TO REMOTE AGENT
	{From, sendMessage, Message} ->
	    Control ! {self(), sendMessage, Message},
	    server(Control, QMesg, QWaiting);
%-------------------- GET NEXT KQML MESSAGE FROM REMOTE AGENT
	{From, getMessage, Id, Timeout} ->
	    case queue:out(QMesg) of
		{{value, Mesg}, New_QMesg} ->
		    From ! {ok, Id, Mesg},
		    server(Control, New_QMesg, QWaiting);
		{empty, New_QMesg} ->
		    case Timeout of
			0 ->
			    From ! {Id, no_message},
			    server(Control, New_QMesg, QWaiting);
			Time when Time < 0 ->
			    WaitingPID = spawn(knitter_conv_null, start_waiting, [self(), From, Id, infinity]),
			    New_QWaiting = q_in(WaitingPID, QWaiting),
			    server(Control, New_QMesg, New_QWaiting);
			Time ->
			    WaitingPID = spawn(knitter_conv_null, start_waiting, [self(), From, Id, Timeout]),
			    New_QWaiting = q_in(WaitingPID, QWaiting),
			    server(Control, New_QMesg, New_QWaiting)
		    end
		end;
%-------------------- RECEIVE A KQML MESSAGE FROM REMOTE AGENT
	{Control, receiveMessage, Message} ->
	    case q_out(QWaiting) of
		{{value, WaitingPID}, New_QWaiting} ->
		    WaitingPID ! {self(), message, Message},
		    server(Control, QMesg, New_QWaiting);
		{empty, New_QWaiting} ->
		    New_QMesg = queue:in(Message, QMesg),
		    server(Control, New_QMesg, QWaiting)
	    end;
%-------------------- A WAITING PROCESS GOT TIRED OF BE A WAITING PROCESS
	{Waiting, killme} ->
	    case q_out(Waiting, QWaiting) of
		{{value, Waiting}, New_QWaiting} ->
		    Waiting ! {self(), killed},
		    server(Control, QMesg, New_QWaiting);
		{empty, New_QWaiting} ->
		    server(Control, QMesg, New_QWaiting)
	    end;
%-------------------- IGNORE OTHER ERLANG MESSAGES	    
	_ ->
	    server(Control, QMesg, QWaiting)
    end.


q_new() ->
    [].


q_in(Item, Queue) ->
    Queue ++ [Item].


q_out([]) ->
    {empty, []};
q_out([H | T]) ->
    {{value, H}, T}.


q_out(Item, Queue) ->
    q_out(Item, Queue, []).


q_out(Item, [], Accum) ->
    {empty, lists:reverse(Accum)};
q_out(Item, [Item | T], Accum) ->
    {{value, Item}, lists:reverse(Accum) ++ T};
q_out(Item, [H | T], Accum) ->
    q_out(Item, T, Accum).


start_waiting(Master, Client, Id, Timeout) ->
    receive
	{Master, message, Message} ->
	    Client ! {Id, message, Message}
    after Timeout ->
	    Master ! {self(), killme},
	    receive
		{Master, killed} ->
		    Client ! {Id, no_message};
		{Master, message, Message} ->
		    Client ! {Id, message, Message}
	    end
    end.
