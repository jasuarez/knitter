-module(knitter_error).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').


-export([ans_other/2]).
-export([tr_message_parse/4, tr_agent_info/4, tr_connect/5, tr_other/3]).
-export([unexpected_message/4, start_conversation/2, start_protocol/2, incomming_conversation/4, other/1]).



ans_other(ANS_Mod, Reason) ->
    knitter:stop().


tr_message_parse(TR_Mod, TR_PID, Message, Reason) ->
    knitter:stop().


tr_agent_info(TR_Mod, TR_PID, AgentName, Reason) ->
    knitter:stop().


tr_connect(TR_Mod, TR_PID, Address, Port, Reason) ->
    knitter:stop().


tr_other(TR_Mod, TR_PID, Reason) ->
    knitter:stop().


unexpected_message(TR_Mod, TR_PID, Message, Reason) ->
    Reply_mesg = [{performative, "sorry"}, {receiver, knitter_mesg:get_param(Message, sender)}, {sender, knitter_mesg:get_param(Message, receiver)}, {'in-reply-to', knitter_mesg:get_param(Message, 'reply-with')}],
    TR_Mod:send(TR_PID, Reply_mesg).


start_conversation(Conv_Mod, Reason) ->
    knitter:stop().


start_protocol(TR_Mod, Reason) ->
    knitter:stop().


incomming_conversation(TR_Mod, TR_PID, Message, Reason) ->
    Reply_mesg = [{performative, "sorry"}, {receiver, knitter_mesg:get_param(Message, sender)}, {sender, knitter_mesg:get_param(Message, receiver)}, {'in-reply-to', knitter_mesg:get_param(Message, 'reply-with')}],
    TR_Mod:send(TR_PID, Reply_mesg).


other(Reason) ->
    knitter:stop().
