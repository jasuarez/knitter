-module(knitter_util).
-author('$Author$').
-vsn('$Revision$').

-export([start/0, server/1]).
-export([getParam/2, delParam/2, setParam/3]).


start () ->
    spawn(knitter_util, server, [1]).


server (NumID) ->
    receive
	{From, newID} ->
	    From ! {newID, NumID},
	    server(NumID + 1);
	_ ->
	    server(NumID)
    end.


getParam([{Param, Value} | _], Param) ->
    {ok, Value};
getParam([_ | T], Param) ->
    getParam(T, Param);
getParam([], _) ->
    {error, "Undefined parameter"}.


delParam(KQMLMesg, Param) ->
    delParam(KQMLMesg, Param, []).


delParam([{Param, _} | T], Param, L) ->
    {ok, T ++ L};
delParam([H | T], Param, L) ->
    delParam(T, Param, [H | L]);
delParam([], _, L) ->
    {undef, L}.


setParam(KQMLMesg, Param, Value) ->
    {_, K} = delParam(KQMLMesg, Param),
    {ok, [{Param, Value} | K]}.
