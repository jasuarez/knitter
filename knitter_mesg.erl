-module(knitter_mesg).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([new/0, get_param/2, set_param/3]).



new() ->
    [].


get_param(KQML_mesg, Param) ->
    case lists:keysearch(Param, 1, KQML_mesg) of
	{value, {Param, Value}} ->
		{ok, Value};
	 false ->
		undef
	end.


set_param(KQML_mesg, Param, New_value) ->
    lists:keyreplace(Param, 1, KQML_mesg, {Param, New_value}).
