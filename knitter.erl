-module(knitter).
-author('$Author$').
-vsn('$Revision$').

-export([ioToKQML/1]).
-export([start/1, server/1]).

-import(knitter_scanner, [scan/1]).
-import(knitter_parser, [parse/1]).



start (MyName) ->
    spawn(knitter, server, [MyName]).


server (MyName) ->
    receive
	{FromAgent, createConversation, Conv, WithAgent} ->
	    case catch apply(Conv, start, [MyName, WithAgent]) of
		PidConv when pid(PidConv) ->
		    FromAgent ! {ok, PidConv};
		_ ->
		    FromAgent ! {error, "Unable to create conversation"}
	    end;
	_ ->
	    server(MyName)
    end.


ioToKQML(FileInput) ->
    case catch parse(scan(FileInput)) of
	{ok, KQMLMesg} ->
	    {ok, KQMLMesg};
	
	{error, _} ->
	    {error, "Parse failed"};
	_ ->
	    {error, "Unknown error"}
    end.
