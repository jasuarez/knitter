-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([ioToKQML/1]).

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
