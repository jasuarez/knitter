-module(knitter_mesg_conv).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([kqml_to_ascii/1, ascii_to_kqml/1]).



kqml_to_ascii(Mesg) ->
    {value, {_, Value}} = lists:keysearch(performative, 1, Mesg),
    kqml_to_ascii(lists:keydelete(performative, 1, Mesg), "(" ++ Value) ++ ")".


kqml_to_ascii([{Perf, Value} | L], StrMesg) ->
    kqml_to_ascii(L, StrMesg ++ " :" ++ atom_to_list(Perf) ++ " " ++ Value);

kqml_to_ascii([], StrMesg) ->
    StrMesg.


ascii_to_kqml(Mesg) ->
    Tokens = knitter_scanner:scan(Mesg),
    {ok, Result} = knitter_parser:parse(Tokens),
    Result.
