-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([ioToKQML/1]).
-export([getParam/2, delParam/2, setParam/3]).

-import(knitter_scanner, [scan/1]).
-import(knitter_parser, [parse/1]).


ioToKQML(FileInput) ->
    case catch parse(scan(FileInput)) of
	{ok, KQMLMesg} ->
	    {ok, KQMLMesg};
	
	{error, _} ->
	    {error, "Parse failed"};
	_ ->
	    {error, "Unknown error"}
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
