-module(knitter).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').


-export([start/1, listen/1, listen/2, start_conversation/2]).
-export([send_message/1]).
-export([receive_message/1]).



start(Agent_name) ->
    knitter_impl:start(Agent_name).


listen(Conversation_module) ->
    listen(Conversation_module, -1).


listen(Conversation_module, Time) ->
    knitter ! {self(), wait_conv, Conversation_module, Time},
    receive
	{new_conversation, Conversation} ->
	    {new_conversation, Conversation};
	{no_conversation, Reason} ->
	    {no_conversation, Reason}
    end.


start_conversation(Conversation_module, Agent) ->
    knitter ! {self(), createConversation, Conversation_module, Agent},
    receive
	{new_conversation, Conversation} ->
	    {new_conversation, Conversation}
    end.


send_message(Message) ->
    knitter ! {self(), sendMessage, Message},
    ok.


receive_message(Message) ->
	knitter ! {self(), receiveMessage, Message},
	ok.
