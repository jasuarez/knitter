-module(knitter_ans_file).
-author('$Author$').
-vsn('$Revision$').

-export([start/0]).
-export([server/1]).



start() ->
    case file:consult('knitter_ans_file.cfg') of
	{ok, Agents} ->
	    case check_data(Agents) of
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
	{From, get_info, Agent} ->
	    case get_agent_info(Agents, Agent) of
		[] ->
		    From ! {ans_error, "Unknown agent"};
		Info ->
		    From ! {ans_ok, Info}
	    end,
	    server(Agents);
	_ ->
	    server(Agents)
    end.


get_agent_info([H | T], Agent) ->
    case lists:keysearch(agent, 1, H) of
	{value, {agent, Agent}} ->
	    H;
	_ ->
	    get_agent_info(T, Agent)
    end;

get_agent_info([], _) ->
    [].


check_data([H | T]) ->
    case {lists:keymember(agent, 1, H), lists:keymember(protocol, 1, H)} of
	{true, true} ->
	    checkData(T);
	_ ->
	    error
    end;


check_data([]) ->
    ok.
