-module(knitter_error).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').


-export([ans_other/2]).
-export([tr_message_parse/4, tr_agent_info/4, tr_connect/5, tr_other/3]).
-export([unexpected_message/4, start_protocol/2, other/1]).



ans_other(ANS_Mod, Reason) ->
    finish().


tr_message_parse(TR_Mod, TR_PID, Message, Reason) ->
    finish().


tr_agent_info(TR_Mod, TR_PID, AgentName, Reason) ->
    finish().


tr_connect(TR_Mod, TR_PID, Address, Port, Reason) ->
    finish().


tr_other(TR_Mod, TR_PID, Reason) ->
    finish().


unexpected_message(TR_Mod, TR_PID, Message, Reason) ->
    Reply_mesg = [{performative, "sorry"}, {receiver, knitter_mesg:get_param(Message, sender)}, {sender, knitter_mesg:get_param(Message, receiver)}, {'in-reply-to', knitter_mesg:get_param(Message, 'reply-with')}],
    TR_Mod:send(TR_PID, Reply_mesg).


start_protocol(TR_Mod, Reason) ->
    finish().


incomming_conection(TR_Mod, TR_PID, Message, Reason) ->
    Reply_mesg = [{performative, "sorry"}, {receiver, knitter_mesg:get_param(Message, sender)}, {sender, knitter_mesg:get_param(Message, receiver)}, {'in-reply-to', knitter_mesg:get_param(Message, 'reply-with')}],
    TR_Mod:send(TR_PID, Reply_mesg).


other(Reason) ->
    finish().


finish() ->
    exit(whereis(knitter), kill).
