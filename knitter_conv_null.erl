-module(knitter_conv_null).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/2, stop/1, send_message/2, get_message/2, receive_message/2]).



start(Local_agent, Remote_Agent) ->
    knitter_conv_null_impl:start(Local_agent, Remote_Agent).


stop(Conversation) ->
    Conversation ! stop,
    ok.


send_message(Conversation, Message) ->
    Conversation ! {self(), sendMessage, Message},
    ok.


get_message(Conversation, Timeout) ->
    Id = make_ref(),
    Conversation ! {self(), getMessage, Id, Timeout},
    receive
	{message, Id, Message} ->
	    {message, Message};
	{no_message, Id, Reason} ->
	    {no_message, Reason}
    end.


receive_message(Conversation, Message) ->
    Conversation ! {receiveMessage, Message}.
