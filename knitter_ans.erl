-module(knitter_ans).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([get_info/1]).



start(Ans) ->
    AnsPID = Ans:start(),
    register ('knitter_ans', AnsPID),
    AnsPID.


get_info(Agent) ->
    knitter_ans ! {self(), get_info, Agent},
    receive
	{ans_ok, Reply} ->
	    {ans_ok, Reply};
	{ans_error, Reason} ->
	    {ans_error, Reason}
    end.

