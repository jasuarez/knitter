-module(knitter_mesg).
-vc('$Id$').
-author('$Author$').
-vsn('$Revision$').

-export([toString/1, toKQML/1]).



toString(Mesg) ->
    {value, {_, Value}} = lists:keysearch(performative, 1, Mesg),
    toString(lists:keydelete(performative, 1, Mesg), "(" ++ Value) ++ ")".


toString([{Perf, Value} | L], StrMesg) ->
    toString(L, StrMesg ++ " :" ++ atom_to_list(Perf) ++ " " ++ Value);
toString([], StrMesg) ->
    StrMesg.


toKQML(Mesg) ->
    Tokens = knitter_scanner:scan(Mesg),
    {ok, Result} = knitter_parser:parse(Tokens),
    Result.
