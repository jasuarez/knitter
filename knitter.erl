-module(knitter).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').


-export([start/1, listen/1, listen/2, start_conversation/2]).
-export([send_message/1, receive_message/1]).
-export([error_ans_other/2]).
-export([error_tr_message_parse/4, error_tr_agent_info/4, error_tr_connect/5, error_tr_other/3]).
-export([error_unexpected_message/4, error_start_protocol/2, error_incomming_connection/4, error_other/1]).



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


error_ans_other(ANS_Mod, Reason) ->
    knitter ! {error, ans_other, [ANS_Mod, Reason]}.


error_tr_message_parse(TR_Mod, TR_PID, Message, Reason) ->
    knitter ! {error, tr_message_parse, [TR_Mod, TR_PID, Message, Reason]}.


error_tr_agent_info(TR_Mod, TR_PID, AgentName, Reason) ->
    knitter ! {error, tr_agent_info, [TR_Mod, TR_PID, AgentName, Reason]}.


error_tr_connect(TR_Mod, TR_PID, Address, Port, Reason) ->
    knitter ! {error, tr_connect, [TR_Mod, TR_PID, Address, Port, Reason]}.


error_tr_other(TR_Mod, TR_PID, Reason) ->
    knitter ! {error, tr_other, [TR_Mod, TR_PID, Reason]}.


error_unexpected_message(TR_Mod, TR_PID, Message, Reason) ->
    knitter ! {error, unexpected_message, [TR_Mod, TR_PID, Message, Reason]}.


error_start_protocol(TR_Mod, Reason) ->
    knitter ! {error, unexpected_message, [TR_Mod, Reason]}.


error_incomming_connection(TR_Mod, TR_PID, Message, Reason) ->
    knitter ! {error, incomming_connection, [TR_Mod, TR_PID, Message, Reason]}.


error_other(Reason) ->
    knitter ! {error, other, [Reason]}.


set_error_callback(Reason, Module, Function, User_params) ->
    knitter ! {set_error_callback, Reason, Module, Function, User_params}.
