-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([start/1]).
-export([server/5]).

-import(knitter_util, [keyssearch/3]).



start(Agent_name) ->
    Config = load_config(),
    AnsPID = start_ans(Config),
    Info = get_agent_info(AnsPID, Agent_name),
    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
    List_protocols = get_protocols(Config),
    ProtoPID = start_protocol(List_protocols, Protocol),
    spawn(knitter, server, [Agent_name, AnsPID, List_protocols, [{Protocol, ProtoPID}], [{Agent_name, Protocol}]]).


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


server(Agent_name, Ans, All_protocols, Active_protocols, Conversations) ->
    receive
	_ ->
	    server(Agent_name, Ans, All_protocols, Active_protocols, Conversations)
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%start(AgentName) ->
%    case file:consult('knitter.cfg') of
%	{ok, Config} ->
%	    case lists:keysearch(ans, 1, Config) of
%		{value, {ans, Module_ANS}} ->
%		    AnsPID = spawn(Module_ANS, start, []),
%		    AnsPID ! {self(), getAgentInfo, AgentName},
%		    receive
%			{ok, Info} ->
%			    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
%			    case keyssearch(protocol, 1, Config) of
%				{value, Protocols} ->
%				    case lists:keysearch(Protocol, 1, Protocols) of
%					{value, {Protocol, Module_Protocol}} ->
%					    spawn(knitter, server, [AgentName, AnsPID, [{AgentName, Protocol}], [{Protocol, spawn(Module_Protocol, start, [])}], []]);
%					false ->
%					    exit("start/1: unable to find a protocol module")
%				    end;
%				false ->
%				    exit("start/1: there isn't any protocol defined")
%			    end;
%			{error, Reason} ->
%			    exit("start/1: " ++ Reason)
%		    end;
%		false->
%		    exit("start/1: no ANS module in configuration file")
%	    end;
%	{error, _} ->
%	    exit("start/1: unable to open/read configuration file")
%    end.
%
%
%server(AgentName, Ans, Conversations, Protocols, ExpectedMessages) ->
%    receive
%	{From, createConversation, Conv, WithAgent} ->
%	    case catch apply(Conv, start, [self(), AgentName, WithAgent]) of
%		PidConv when pid(PidConv) ->
%		    case lists:keysearch(WithAgent, 1, Conversations) of
%			{value, AlreadyConv} ->
%			    From ! {ok, PidConv},
%			    server(AgentName, Ans, [AlreadyConv | Conversations], [Prot | Protocols], ExpectedMessages);
%			false ->
%			    AnsPID ! {self(), getAgentInfo, WithAgent},
%			    receive
%				{ok, Info} ->
%				    {value, {protocol, Protocol}} = lists:keysearch(protocol, 1, Info),
%				    case lists:keymember(Protocol, 1, Protocols) of
%					{value, _} ->
%					    From ! {ok, PidConv},
%					    server(AgentName, Ans, [{WithAgent, Protocol} | Conversations], Protocols, ExpectedMessages);
%					false ->
%					    From ! {ok, PidConv},
%					    server(AgentName, Ans, [{WithAgent, Protocol} | Conversations], [{Protocol, spawn()} | Protocols], ExpectedMessages)
%	
%		    ok
%	    end
%	_ ->
%	    server(AgentName, Ans, Conversations, Protocols, ExpectedMessages)
%    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%start (AgentName) ->
%    case file:consult('knitter.cfg') of
%	{ok, Ports} ->
%	    ListPorts = lists:map(fun({Protocol, Module}) -> {Protocol, apply(Module, start, [])} end, Ports),
%	    spawn(knitter, server, [AgentName, dict:new(), dict:from_list(ListPorts)]);
%	{error, _} ->
%	    exit ("start/1: unable to open configuration file")
%    end.
%
%
%server (AgentName, Conversations, Ports) ->
%    receive
%	{From, createConversation, Conv, WithAgent} ->
%	    case catch apply(Conv, start, [AgentName, WithAgent]) of
%		PidConv when pid(PidConv) ->
%		    From ! {ok, PidConv},
%		    server(AgentName, dict:append({WithAgent, null}, PidConv, Conversations), Ports);
%		_ ->
%		    From ! {error, "Unable to create conversation"},
%		    server(AgentName, Conversations, Ports)
%	    end;
%	_ ->
%	    server(AgentName, Conversations, Ports)
%    end.
