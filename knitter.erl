-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([start/1]).
-export([server/6]).

-import(knitter_util, [keyssearch/3, get_param/2]).



start(Agent_name) ->
    Config = load_config(),
    AnsPID = start_ans(Config),
    Info = get_agent_info(AnsPID, Agent_name),
    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
    List_protocols = get_protocols(Config),
    ProtoPID = start_protocol(List_protocols, Protocol),
    spawn(knitter, server, [Agent_name, AnsPID, List_protocols, [{Protocol, ProtoPID}], [{Agent_name, Protocol}], []]).


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
	    apply(Module_ANS, start, []);
	false ->
	    exit("no ANS module in configuration file")
    end.


get_agent_info(Ans, Agent) ->
    Ans ! {self(), agentInfo, Agent},
    receive
	{ok, Info} ->
	    Info;
	{error, Reason} ->
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


start_protocol(Protocols, Name) ->
    case lists:keysearch(Name, 1, Protocols) of
	{value, {Name, Module}} ->
	    apply(Module, start, []);
	false ->
	    exit("unable to find protocol module")
    end.


server(Agent_name, Ans, All_protocols, Active_protocols, Conversations, Expected_messages) ->
    receive
%-------------------- CREATE A CONVERSATION
	{From, createConversation, Conv, With_agent} ->
	    ConvPID = start_conversation(Conv, Agent_name, With_agent),
	    case lists:keysearch(With_agent, 1, Conversations) of
		{value, Some_conv} ->
		    From ! {ok, ConvPID},
		    server(Agent_name, Ans, All_protocols, Active_protocols, [{With_agent, element(2, Some_conv)} | Conversations], Expected_messages);
		false ->
		    Protocol = get_protocol(Ans, With_agent),
		    case lists:keymember(Protocol, 1, Active_protocols) of
			true ->
			    From ! {ok, ConvPID},
			    server(Agent_name, Ans, All_protocols, Active_protocols, [{With_agent, Protocol, ConvPID} | Conversations], Expected_messages);
			false ->
			    ProtoPID = start_protocol (All_protocols, Protocol),
			    From ! {ok, ConvPID},
			    server(Agent_name, Ans, All_protocols, [{Protocol, ProtoPID} | Active_protocols], [{With_agent, Protocol, ConvPID} | Conversations], Expected_messages)
		    end
	    end;
%-------------------- SEND A KQML MESSAGE TO PEER
	{From, sendMessage, Message} ->
	    {value, {Receiver, Protocol, From}} = lists:keysearch(From, 3, Conversations),
	    {value, {Protocol, ProtoPID}} = lists:keysearch(Protocol, 1, Active_protocols),
	    ProtoPID ! {self(), sendMessage, Message},
	    case get_param(Message, 'reply_with') of
		{ok, Reply_id} ->
		    case lists:keymember(From, 1, Expected_messages) of
			true ->
			    New_expected = lists:keyreplace(From, 1, Expected_messages, {From, Reply_id});
			false ->
			    New_expected = [{From, Reply_id} | Expected_messages]
		    end;
		undef ->
		    New_expected = Expected_messages
	    end,
	    server(Agent_name, Ans, All_protocols, Active_protocols, Conversations, New_expected);
%-------------------- ANY OTHER ERLANG MESSAGE IS DISCARTED
	_ ->
	    server(Agent_name, Ans, All_protocols, Active_protocols, Conversations, Expected_messages)
    end.


get_protocol(Ans, Agent) ->
    Info = get_agent_info(Ans, Agent),
    case lists:keysearch(protocol, 1, Info) of
	{value, {protocol, Protocol}} ->
	    Protocol;
	false ->
	    exit("agent has no protocol")
    end.


start_conversation(Conv_module, Local_agent, Remote_agent) ->
    case catch apply(Conv_module, start, [self(), Local_agent, Remote_agent]) of
	ConvPID when pid(ConvPID) ->
	    ConvPID;
	_ ->
	    exit("unable to start conversation")
    end.
