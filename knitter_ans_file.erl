-module(knitter_ans_simple).
-author('$Author$').
-vsn('$Revision$').

-export([start/0]).
-export([server/1]).



start() ->
    case file:consult('knitter_ans_simple.cfg') of
	{ok, Agents} ->
	    case checkData(Agents) of
		ok ->
		    spawn(knitter_ans_simple, server, [Agents]);
		error ->
		    exit("start/0: invalid configuration file")
	    end;
	{error, _} ->
	    exit("start/0: unable to open/read configuration file")
    end.


server(Agents) ->
    receive
	{From, agentInfo, Agent} ->
	    case getAgentInfo(Agents, Agent) of
		[] ->
		    From ! {error, "Unknown agent"};
		Info ->
		    From ! {ok, Info}
	    end,
	    server(Agents);
	_ ->
	    server(Agents)
    end.


getAgentInfo([H | T], Agent) ->
    case lists:keysearch(agent, 1, H) of
	{value, {agent, Agent}} ->
	    H;
	_ ->
	    getAgentInfo(T, Agent)
    end;

getAgentInfo([], _) ->
    [].


checkData([H | T]) ->
    case {lists:keymember(agent, 1, H), lists:keymember(protocol, 1, H)} of
	{true, true} ->
	    checkData(T);
	_ ->
	    error
    end;


checkData([]) ->
    ok.
