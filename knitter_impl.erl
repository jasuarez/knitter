-module(knitter_impl).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([server/1, wait_no_incomming_conv/0]).

-import(knitter_util, [keyssearch/3]).
-import(knitter_mesg, [get_param/2]).

-include("knitter_impl.hrl").



start(Agent_name) ->
    Config = load_config(),
    start_ans(Config),
    Info = get_agent_info(Agent_name),
    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
    List_protocols = get_protocols(Config),
    Control = spawn(?MODULE, server, #server_state{name = Agent_name, protocols = List_protocols}),
    register(knitter, Control),
    Incomming_conv = spawn_link(?MODULE, wait_no_incomming_conv, []),
    knitter ! {setIncommingConvPID, Incomming_conv},
    ProtoPID = start_protocol(List_protocols, Protocol, Agent_name),
    knitter ! {listenConnection, Protocol, ProtoPID},
    process_flag(trap_exit, true),
    Control.


start_ans(Config) ->
    case lists:keysearch(ans, 1, Config) of
	{value, {ans, Module_ANS}} ->
	    knitter_ans:start(Module_ANS);
	false ->
	    exit("no ANS module in configuration file")
    end.


start_protocol(Protocols, Name, Agent) ->
    case lists:keysearch(Name, 1, Protocols) of
	{value, {Name, Module}} ->
	    Module:start(Agent);
	false ->
	    exit("unable to find protocol module")
    end.


start_conversation(Conv_module, Local_agent, Remote_agent) ->
    case catch Conv_module:start(Local_agent, Remote_agent) of
	ConvPID when pid(ConvPID) ->
	    ConvPID;
	_ ->
	    exit("unable to start conversation")
    end.


load_config() ->
    case file:consult('knitter.cfg') of
	{ok, Config} ->
	    Config;
	{error, _} ->
	    exit("unable to open/read configuration file")
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


server(State) when record(State, server_state) ->
    receive
%-------------------- SET THE PROCESS WAITING FOR INCOMMING CONVERSATIONS
	{setIncommingConvPID, New_incomming_conv} ->
	    server(State#server_state{waiter_conv = New_incomming_conv});
%-------------------- ADD A LISTENING CONNECTION
	{listenConnection, Protocol, ProtoPID} ->
	    server(State#server_state{active_protocols = [{Protocol, ProtoPID} | State#server_state.active_protocols], conversations = [{State#server_state.name, Protocol} | State#server_state.conversations]});
%-------------------- WE WANT LISTEN FOR INCOMMING CONNECTIONS
	{Client, wait_conv, Conv_module, Time} ->
	    State#server_state.waiter_conv ! {wait_conv, Client, Conv_module, Time},
	    server(State);
%-------------------- CREATE A CONVERSATION
	{From, createConversation, Conv, With_agent} ->
	    ConvPID = start_conversation(Conv, State#server_state.name, With_agent),
	    case lists:keysearch(With_agent, 1, State#server_state.conversations) of
		{value, Some_conv} ->
		    From ! {new_conversation, ConvPID},
		    server(State#server_state{conversations = [{With_agent, element(2, Some_conv)} | State#server_state.conversations]});
		false ->
		    Protocol = get_protocol(With_agent),
		    case lists:keymember(Protocol, 1, State#server_state.active_protocols) of
			true ->
			    From ! {new_conversation, ConvPID},
			    server(State#server_state{conversations = [{With_agent, Protocol, ConvPID} | State#server_state.conversations]});
			false ->
			    ProtoPID = start_protocol (State#server_state.protocols, Protocol, State#server_state.name),
			    From ! {new_conversation, ConvPID},
			    server(State#server_state{active_protocols = [{Protocol, ProtoPID} | State#server_state.active_protocols], conversations = [{With_agent, Protocol, ConvPID} | State#server_state.conversations]})
		    end
	    end;
%-------------------- SEND A KQML MESSAGE TO PEER
	{From, sendMessage, Message} ->
	    {value, {Receiver, Protocol, From}} = lists:keysearch(From, 3, State#server_state.conversations),
	    {value, {Protocol, ProtoPID}} = lists:keysearch(Protocol, 1, State#server_state.active_protocols),
	    ProtoPID ! {sendMessage, Message},
	    case get_param(Message, 'reply-with') of
		{ok, Reply_id} ->
		    case lists:keymember(From, 1, State#server_state.expected_messages) of
			true ->
			    New_expected = lists:keyreplace(From, 1, State#server_state.expected_messages, {From, {Receiver, Reply_id}});
			false ->
			    New_expected = [{From, {Receiver, Reply_id}} | State#server_state.expected_messages]
		    end;
		undef ->
		    New_expected = State#server_state.expected_messages
	    end,
	    server(State#server_state{expected_messages = New_expected});
%-------------------- RECEIVE A KQML MESSAGE FROM REMOTE AGENT
	{ProtoPID, receiveMessage, Message} ->
	    {ok, Sender} = get_param(Message, sender),
	    case get_param(Message, 'in-reply-to') of
		{ok, In_reply_to} ->
		    case lists:keysearch({Sender, In_reply_to}, 2, State#server_state.expected_messages) of
			{value, {ConvPID, _}} ->
			    ConvPID ! {receiveMessage, Message},
			    New_expected = lists:keydelete(ConvPID, 1, State#server_state.expected_messages),
			    server(State#server_state{expected_messages = New_expected});
			false ->
			    %Una respuesta no puede ser comienzo de una conversación. Enviar un sorry.
			    server(State)
		    end;
		undef ->
		    State#server_state.waiter_conv ! {make_conv, State#server_state.name, Sender},
		    receive
			no_conversation ->
			    %No se espera por ninguna conversación. Enviar un sorry.
			    server(State);
			{new_conversation, ConvPID} ->
			    ConvPID ! {receiveMessage, Message},
			    {value, Protocol} = lists:keysearch(ProtoPID, 2, State#server_state.active_protocols),
			    New_conversations = [{Sender, Protocol, ConvPID} | State#server_state.conversations],
			    server(State#server_state{conversations = New_conversations})
		    end
	    end;
%-------------------- ANY OTHER ERLANG MESSAGE IS DISCARTED
	_ ->
	    server(State)
    end.


get_protocol(Agent) ->
    {ans_ok, Info} = knitter_ans:get_info(Agent),
    case lists:keysearch(protocol, 1, Info) of
	{value, {protocol, Protocol}} ->
	    Protocol;
	false ->
	    exit("agent has no protocol")
    end.


wait_no_incomming_conv() ->
    receive
	{make_conv, Local_agent, Remote_agent} ->
	    knitter ! no_conversation,
	    wait_no_incomming_conv();
	{wait_conv, Client, Conv_module, Timeout} ->
	    Time = case Timeout of
		       N when N < 0 ->
			   infinity;
		       N ->
			   N
		   end,
	    wait_incomming_conv(Client, Conv_module, Time);
	_ ->
	    wait_no_incomming_conv()
    end.


wait_incomming_conv(Client, Conv_module, Timeout) ->
    receive
	{make_conv, Local_agent, Remote_agent} ->
	    ConvPID = start_conversation(Conv_module, Local_agent, Remote_agent),
	    Client ! {new_conversation, ConvPID},
	    knitter ! {new_conversation, ConvPID},
	    wait_no_incomming_conv();
	{wait_conv, New_client, New_conv_module, New_timeout} ->
	    Client ! {no_conversation, other_waiting},
	    New_time = case New_timeout of
		       N when N < 0 ->
			   infinity;
		       N ->
			   N
		   end,
	    wait_incomming_conv(New_client, New_conv_module, New_time)
    after Timeout ->
	    Client ! {no_conversation, timeout},
	    wait_no_incomming_conv()
    end.
