-module(knitter_mesg_conv).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([kqml_to_string/1, string_to_kqml/1]).



kqml_to_string(Mesg) ->
    {value, {_, Value}} = lists:keysearch(performative, 1, Mesg),
    kqml_to_string(lists:keydelete(performative, 1, Mesg), "(" ++ Value) ++ ")".


kqml_to_string([{Perf, Value} | L], StrMesg) ->
    kqml_to_string(L, StrMesg ++ " :" ++ atom_to_list(Perf) ++ " " ++ Value);
kqml_to_string([], StrMesg) ->
    StrMesg.


string_to_kqml(Mesg) ->
    Tokens = knitter_scanner:scan(Mesg),
    {ok, Result} = knitter_parser:parse(Tokens),
    Result.
