-module(knitter_conv_null_impl).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/2]).
-export([server/1]).

-include("knitter_conv_null_impl.hrl").



start(Local_agent, Remote_agent) ->
    spawn(?MODULE, server, [#server_state{}]).


server(State) ->
    receive
%%%-------------------- SEND A KQML MESSAGE TO REMOTE AGENT
	{From, sendMessage, Message} ->
	    knitter:send_message(Message),
	    server(State);
%%%-------------------- GET NEXT KQML MESSAGE FROM REMOTE AGENT
	{From, getMessage, Id, Timeout} ->
	    case queue:out(State#server_state.messages) of
		{{value, Mesg}, New_QMesg} ->
		    From ! {message, Id, Mesg},
		    server(State#server_state{messages = New_QMesg});
		{empty, New_QMesg} ->
		    case Timeout of
			0 ->
			    From ! {no_message, Id, timeout},
			    server(State#server_state{messages = New_QMesg});
			Time when Time < 0 ->
			    WaitingPID = spawn(?MODULE, start_waiting, [self(), From, Id, infinity]),
			    New_QWaiting = q_in(WaitingPID, State#server_state.processes),
			    server(State#server_state{messages = New_QMesg, processes = New_QWaiting});
			Time ->
			    WaitingPID = spawn(?MODULE, start_waiting, [self(), From, Id, Timeout]),
			    New_QWaiting = q_in(WaitingPID, State#server_state.processes),
			    server(State#server_state{messages = New_QMesg, processes = New_QWaiting})
		    end
	    end;
%%%-------------------- RECEIVE A KQML MESSAGE FROM REMOTE AGENT
	{receiveMessage, Message} ->
	    case q_out(State#server_state.processes) of
		{{value, WaitingPID}, New_QWaiting} ->
		    WaitingPID ! {self(), message, Message},
		    server(State#server_state{processes = New_QWaiting});
		{empty, New_QWaiting} ->
		    New_QMesg = queue:in(Message, State#server_state.messages),
		    server(State#server_state{messages = New_QMesg, processes = New_QWaiting})
	    end;
%%%-------------------- A WAITING PROCESS GOT TIRED OF BE A WAITING PROCESS
	{Waiting, killme} ->
	    case q_out(Waiting, State#server_state.processes) of
		{{value, Waiting}, New_QWaiting} ->
		    Waiting ! {self(), killed},
		    server(State#server_state{processes = New_QWaiting});
		{empty, New_QWaiting} ->
		    server(State#server_state{processes = New_QWaiting})
	    end;
%%%-------------------- STOP THE CONVERSATION
	stop ->
	    Stop_waiters = fun (WaiterPID) ->
				   WaiterPID ! stop
			   end,
	    lists:foreach(Stop_waiters, State#server_state.processes),
	    exit(normal);
%%%-------------------- IGNORE OTHER ERLANG MESSAGES	    
	_ ->
	    server(State)
    end.


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
	    Client ! {message, Id, Message};
	stop ->
	    Client ! {no_message, Id, system_end},
	    exit(normal)
    after Timeout ->
	    Master ! {self(), killme},
	    receive
		{Master, killed} ->
		    Client ! {no_message, Id, timeout};
		{Master, message, Message} ->
		    Client ! {message, Id, Message};
		stop  ->
		    Client ! {no_message, Id, system_end},
		    exit(normal)
	    end
    end.
