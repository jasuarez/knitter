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
    Control = spawn(?MODULE, server, [#server_state{name = Agent_name, protocols = List_protocols}]),
    register(knitter, Control),
    Incomming_conv = spawn_link(?MODULE, wait_no_incomming_conv, []),
    knitter ! {setIncommingConvPID, Incomming_conv},
    ProtoPID = start_protocol(List_protocols, Protocol, Agent_name),
    knitter ! {listenConnection, Protocol, ProtoPID},
    process_flag(trap_exit, true),
    ok.


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
	    unable_to_start
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


get_protocol(Agent) ->
    {ans_ok, Info} = knitter_ans:get_info(Agent),
    case lists:keysearch(protocol, 1, Info) of
	{value, {protocol, Protocol}} ->
	    Protocol;
	false ->
	    exit("agent has no protocol")
    end.


server(State) ->
    receive
%%%-------------------- GENERATE AN ERROR
	{error, Type, Parameters} ->
	    %%spawn(?MODULE, execute_error_callback, [State#server_state.callbacks, Type, Parameters]),
	    %%spawn(?MODULE, fun() -> execute_error_callback(State#server_state.callbacks, Type, Parameters) end, []),
	    execute_error_callback(State#server_state.callbacks, Type, Parameters),
	    server(State);
%%%-------------------- CHANGE ERROR CALLBACK
	{set_error_callback, Reason, Module, Function, User_params} ->
	    server(State#server_state{callbacks = set_error_callback(State#server_state.callbacks, Reason, Module, Function, User_params)});
%%%-------------------- SET THE PROCESS WAITING FOR INCOMMING CONVERSATIONS
	{setIncommingConvPID, New_incomming_conv} ->
	    server(State#server_state{waiter_conv = New_incomming_conv});
%%%-------------------- ADD A LISTENING CONNECTION
	{listenConnection, Protocol, ProtoPID} ->
	    server(State#server_state{active_protocols = [{Protocol, ProtoPID} | State#server_state.active_protocols], conversations = [{State#server_state.name, Protocol} | State#server_state.conversations]});
%%%-------------------- WE WANT LISTEN FOR INCOMMING CONNECTIONS
	{Client, wait_conv, Conv_module, Time} ->
	    State#server_state.waiter_conv ! {wait_conv, Client, Conv_module, Time},
	    server(State);
%%%-------------------- CREATE A CONVERSATION
	{From, createConversation, Conv, With_agent} ->
	    case start_conversation(Conv, State#server_state.name, With_agent) of
		unable_to_start ->
		    knitter:error_start_conversation(Conv, "unable to start conversation"),
		    server(State);
		ConvPID ->	  
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
		    end
	    end;
%%%-------------------- SEND A KQML MESSAGE TO PEER
	{From, sendMessage, Message} ->
	    {value, {Receiver, Protocol, From}} = lists:keysearch(From, 3, State#server_state.conversations),
	    {value, {Protocol, ProtoPID}} = lists:keysearch(Protocol, 1, State#server_state.active_protocols),
	    ProtoPID ! {sendMessage, Message},
	    New_expected = case get_param(Message, 'reply-with') of
			       {ok, Reply_id} ->
				   case lists:keymember(From, 1, State#server_state.expected_messages) of
				       true ->
					   lists:keyreplace(From, 1, State#server_state.expected_messages, {From, {Receiver, Reply_id}});
				       false ->
					   [{From, {Receiver, Reply_id}} | State#server_state.expected_messages]
				   end;
			       undef ->
				   State#server_state.expected_messages
			   end,
	    server(State#server_state{expected_messages = New_expected});
%%%-------------------- RECEIVE A KQML MESSAGE FROM REMOTE AGENT
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
			    {value, Proto_name} = lists:keysearch(ProtoPID, 2, State#server_state.active_protocols),
			    {value, Proto_mod} = lists:keysearch(Proto_name, 1, State#server_state.protocols),
			    knitter:error_unexpected_message(Proto_mod, ProtoPID, Message, "a reply cannot start a conversation"),
			    server(State)
		    end;
		undef ->
		    State#server_state.waiter_conv ! {make_conv, State#server_state.name, Sender},
		    receive
			no_conversation ->
			    {value, Proto_name} = lists:keysearch(ProtoPID, 2, State#server_state.active_protocols),
			    {value, Proto_mod} = lists:keysearch(Proto_name, 1, State#server_state.protocols),
			    knitter:error_incomming_conversation(Proto_name, Proto_mod, Message, "anybody waits for a new conversation"),
			    server(State);
			{new_conversation, ConvPID} ->
			    ConvPID ! {receiveMessage, Message},
			    {value, Protocol} = lists:keysearch(ProtoPID, 2, State#server_state.active_protocols),
			    New_conversations = [{Sender, Protocol, ConvPID} | State#server_state.conversations],
			    server(State#server_state{conversations = New_conversations})
		    end
	    end;
%%%-------------------- STOP THE KNITTER SYSTEM
	stop ->
	    knitter_ans:stop(),
	    State#server_state.waiter_conv ! stop,
	    Stop_conv = fun (Element) ->
				case Element of
				    {_, _, ConvPID} ->
					ConvPID ! stop;
				    _ ->
					ok
				end
			end,
	    lists:foreach(Stop_conv, State#server_state.conversations),
	    Stop_prot = fun ({_, ProtoPID}) ->
				ProtoPID ! stop
			end,
	    lists:foreach(Stop_prot, State#server_state.active_protocols),
	    exit(normal);
%%%-------------------- ANY OTHER ERLANG MESSAGE IS DISCARTED
	_ ->
	    server(State)
    end.


execute_error_callback(Callbacks, ans_other, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.ans_other),
    Function = element(2, Callbacks#error_callbacks.ans_other),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.ans_other),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, tr_message_parse, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.tr_message_parse),
    Function = element(2, Callbacks#error_callbacks.tr_message_parse),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.tr_message_parse),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, tr_agent_info, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.tr_agent_info),
    Function = element(2, Callbacks#error_callbacks.tr_agent_info),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.tr_agent_info),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, tr_unable_connect, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.tr_unable_connect),
    Function = element(2, Callbacks#error_callbacks.tr_unable_connect),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.tr_unable_connect),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, tr_other, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.tr_other),
    Function = element(2, Callbacks#error_callbacks.tr_other),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.tr_other),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, unexpected_message, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.unexpected_message),
    Function = element(2, Callbacks#error_callbacks.unexpected_message),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.unexpected_message),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, start_conversation, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.start_conversation),
    Function = element(2, Callbacks#error_callbacks.start_conversation),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.start_conversation),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, start_protocol, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.start_protocol),
    Function = element(2, Callbacks#error_callbacks.start_protocol),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.start_protocol),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, incomming_conversation, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.incomming_conversation),
    Function = element(2, Callbacks#error_callbacks.incomming_conversation),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.incomming_conversation),
    apply(Module, Function, Params);

execute_error_callback(Callbacks, other, Parameters) ->
    Module = element(1, Callbacks#error_callbacks.other),
    Function = element(2, Callbacks#error_callbacks.other),
    Params = Parameters ++ element(3, Callbacks#error_callbacks.other),
    apply(Module, Function, Params).


set_error_callback(Callbacks, ans_other, Module, Function, Params) ->
    Callbacks#error_callbacks{ans_other = {Module, Function, Params}};

set_error_callback(Callbacks, tr_message_parse, Module, Function, Params) ->
    Callbacks#error_callbacks{tr_message_parse = {Module, Function, Params}};

set_error_callback(Callbacks, tr_agent_info, Module, Function, Params) ->
    Callbacks#error_callbacks{tr_agent_info = {Module, Function, Params}};

set_error_callback(Callbacks, tr_unable_connect, Module, Function, Params) ->
    Callbacks#error_callbacks{tr_unable_connect = {Module, Function, Params}};

set_error_callback(Callbacks, tr_other, Module, Function, Params) ->
    Callbacks#error_callbacks{tr_other = {Module, Function, Params}};

set_error_callback(Callbacks, unexpected_message, Module, Function, Params) ->
    Callbacks#error_callbacks{unexpected_message = {Module, Function, Params}};

set_error_callback(Callbacks, start_conversation, Module, Function, Params) ->
    Callbacks#error_callbacks{start_conversation = {Module, Function, Params}};

set_error_callback(Callbacks, start_protocol, Module, Function, Params) ->
    Callbacks#error_callbacks{start_protocol = {Module, Function, Params}};

set_error_callback(Callbacks, incomming_conversation, Module, Function, Params) ->
    Callbacks#error_callbacks{incomming_conversation = {Module, Function, Params}};

set_error_callback(Callbacks, other, Module, Function, Params) ->
    Callbacks#error_callbacks{other = {Module, Function, Params}}.


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
	stop ->
	    exit(normal);
	_ ->
	    wait_no_incomming_conv()
    end.


wait_incomming_conv(Client, Conv_module, Timeout) ->
    receive
	{make_conv, Local_agent, Remote_agent} ->
	    case start_conversation(Conv_module, Local_agent, Remote_agent) of
		unable_to_start ->
		    knitter:error_start_conversation(Conv_module, "unable to start conversation");
		ConvPID ->
		    Client ! {new_conversation, ConvPID},
		    knitter ! {new_conversation, ConvPID}
	    end,
	    wait_no_incomming_conv();
	{wait_conv, New_client, New_conv_module, New_timeout} ->
	    Client ! {no_conversation, other_waiting},
	    New_time = case New_timeout of
			   N when N < 0 ->
			       infinity;
			   N ->
			       N
		       end,
	    wait_incomming_conv(New_client, New_conv_module, New_time);
	stop ->
	    Client ! {no_conversation, system_end},
	    exit(normal)
    after Timeout ->
	    Client ! {no_conversation, timeout},
	    wait_no_incomming_conv()
    end.
