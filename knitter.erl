-module(knitter).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([server/6, wait_no_incomming_conv/1]).

-import(knitter_util, [keyssearch/3]).
-import(knitter_mesg, [get_param/2]).



start(Agent_name) ->
    Config = load_config(),
    start_ans(Config),
    Info = get_agent_info(Agent_name),
    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
    List_protocols = get_protocols(Config),
    Control = spawn(knitter, server, [Agent_name, null, List_protocols, [], [], []]),
    Incomming_conv = spawn(knitter, wait_no_incomming_conv, [Control]),
    Control ! {setIncommingConvPID, Incomming_conv},
    ProtoPID = start_protocol(Control, List_protocols, Protocol, Agent_name),
    Control ! {listenConnection, Protocol, ProtoPID},
    Control.


load_config() ->
    case file:consult('knitter.cfg') of
	{ok, Config} ->
	    Config;
	{error, _} ->
	    exit("unable to open/read configuration file")
    end.


start_ans(Config) ->
    case lists:keysearch(ans, 1, Config) of
	{value, {ans, Module_ANS}} ->
	    knitter_ans:start(Module_ANS);
	false ->
	    exit("no ANS module in configuration file")
    end.


get_agent_info(Agent) ->
    case knitter_ans:get_info(Agent) of
	{ans_ok, Info} ->
	    Info;
	{ans_error, Reason} ->
	    exit(Reason)
    end.


get_protocols(Config) ->
    List_protocols = keyssearch(protocol, 1, Config),
    case List_protocols of
	{values, Protocols} ->
	    lists:map(fun(Protocol) -> element(2, Protocol) end, Protocols);
	false ->
	    []
    end.


start_protocol(Control, Protocols, Name, Agent) ->
    case lists:keysearch(Name, 1, Protocols) of
	{value, {Name, Module}} ->
	    Module:start(Control, Agent);
	false ->
	    exit("unable to find protocol module")
    end.


server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages) ->
    receive
%-------------------- SET THE PROCESS WAITING FOR INCOMMING CONVERSATIONS
	{setIncommingConvPID, New_incomming_conv} ->
	    server(Agent_name, New_incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages);
%-------------------- ADD A LISTENING CONNECTION
	{listenConnection, Protocol, ProtoPID} ->
	    server(Agent_name, Incomming_conv, All_protocols, [{Protocol, ProtoPID} | Active_protocols], [{Agent_name, Protocol} | Conversations], Expected_messages);
%-------------------- WE WANT LISTEN FOR INCOMMING CONNECTIONS
	{Client, wait_conv, Conv_module, Time} ->
	    Incomming_conv ! {self(), wait_conv, Client, Conv_module, Time},
	    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages);
%-------------------- CREATE A CONVERSATION
	{From, createConversation, Conv, With_agent} ->
	    ConvPID = start_conversation(Conv, Agent_name, With_agent),
	    case lists:keysearch(With_agent, 1, Conversations) of
		{value, Some_conv} ->
		    From ! {ok, ConvPID},
		    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, [{With_agent, element(2, Some_conv)} | Conversations], Expected_messages);
		false ->
		    Protocol = get_protocol(With_agent),
		    case lists:keymember(Protocol, 1, Active_protocols) of
			true ->
			    From ! {ok, ConvPID},
			    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, [{With_agent, Protocol, ConvPID} | Conversations], Expected_messages);
			false ->
			    ProtoPID = start_protocol (self(), All_protocols, Protocol, Agent_name),
			    From ! {ok, ConvPID},
			    server(Agent_name, Incomming_conv, All_protocols, [{Protocol, ProtoPID} | Active_protocols], [{With_agent, Protocol, ConvPID} | Conversations], Expected_messages)
		    end
	    end;
%-------------------- SEND A KQML MESSAGE TO PEER
	{From, sendMessage, Message} ->
	    {value, {Receiver, Protocol, From}} = lists:keysearch(From, 3, Conversations),
	    {value, {Protocol, ProtoPID}} = lists:keysearch(Protocol, 1, Active_protocols),
	    ProtoPID ! {self(), sendMessage, Message},
	    case get_param(Message, 'reply-with') of
		{ok, Reply_id} ->
		    case lists:keymember(From, 1, Expected_messages) of
			true ->
			    New_expected = lists:keyreplace(From, 1, Expected_messages, {From, {Receiver, Reply_id}});
			false ->
			    New_expected = [{From, {Receiver, Reply_id}} | Expected_messages]
		    end;
		undef ->
		    New_expected = Expected_messages
	    end,
	    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, New_expected);
%-------------------- RECEIVE A KQML MESSAGE FROM REMOTE AGENT
	{ProtoPID, receiveMessage, Message} ->
	    {ok, Sender} = get_param(Message, sender),
	    case get_param(Message, 'in-reply-to') of
		{ok, In_reply_to} ->
		    case lists:keysearch({Sender, In_reply_to}, 2, Expected_messages) of
			{value, {ConvPID, _}} ->
			    ConvPID ! {self(), receiveMessage, Message},
			    New_expected = lists:keydelete(ConvPID, 1, Expected_messages),
			    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, New_expected);
			false ->
			    %Una respuesta no puede ser comienzo de una conversación. Enviar un sorry.
			    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages)
		    end;
		undef ->
		    Incomming_conv ! {self(), make_conv, Agent_name, Sender},
		    receive
			no_conversation ->
			    %No se espera por ninguna conversación. Enviar un sorry.
			    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages);
			{new_conversation, ConvPID} ->
			    ConvPID ! {self(), receiveMessage, Message},
			    {value, Protocol} = lists:keysearch(ProtoPID, 2, Active_protocols),
			    New_conversations = [{Sender, Protocol, ConvPID} | Conversations],
			    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, New_conversations, Expected_messages)
		    end
	    end;
%-------------------- ANY OTHER ERLANG MESSAGE IS DISCARTED
	_ ->
	    server(Agent_name, Incomming_conv, All_protocols, Active_protocols, Conversations, Expected_messages)
    end.


get_protocol(Agent) ->
    {ans_ok, Info} = knitter_ans:get_info(Agent),
    case lists:keysearch(protocol, 1, Info) of
	{value, {protocol, Protocol}} ->
	    Protocol;
	false ->
	    exit("agent has no protocol")
    end.


start_conversation(Conv_module, Local_agent, Remote_agent) ->
    case catch Conv_module:start(self(), Local_agent, Remote_agent) of
	ConvPID when pid(ConvPID) ->
	    ConvPID;
	_ ->
	    exit("unable to start conversation")
    end.


wait_no_incomming_conv(Control) ->
    receive
	{Control, make_conv, Local_agent, Remote_agent} ->
	    Control ! no_conversation,
	    wait_no_incomming_conv(Control);
	{Control, wait_conv, Client, Conv_module, Timeout} ->
	    Time = case Timeout of
		       N when N < 0 ->
			   infinity;
		       N ->
			   N
		   end,
	    wait_incomming_conv(Control, Client, Conv_module, Time);
	_ ->
	    wait_no_incomming_conv(Control)
    end.


wait_incomming_conv(Control, Client, Conv_module, Timeout) ->
    receive
	{Control, make_conv, Local_agent, Remote_agent} ->
	    ConvPID = start_conversation(Conv_module, Local_agent, Remote_agent),
	    Client ! {Control, new_conversation, ConvPID},
	    Control ! {new_conversation, ConvPID},
	    wait_no_incomming_conv(Control);
	{Control, wait_conv, New_client, New_conv_module, New_timeout} ->
	    Client ! {Control, no_conversation, time_out},
	    New_time = case New_timeout of
		       N when N < 0 ->
			   infinity;
		       N ->
			   N
		   end,
	    wait_incomming_conv(Control, New_client, New_conv_module, New_time)
    after Timeout ->
	    Client ! {Control, no_conversation, time_out},
	    wait_no_incomming_conv(Control)
    end.
