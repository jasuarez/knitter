-module(knitter_mesg).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([new/0, get_param/2, set_param/3, set_param/2, del_param/2]).



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
    case lists:keymember(Param, 1, KQML_mesg) of
	true ->
	    lists:keyreplace(Param, 1, KQML_mesg, {Param, New_value});
	false ->
	    [{Param, New_value} | KQML_mesg]
    end.


set_param(KQML_mesg, []) ->
    KQML_mesg;
set_param(KQML_mesg, [{Param, New_value} | T]) ->
    set_param(set_param(KQML_mesg, Param, New_value), T).


del_param(KQML_mesg, Params) when list(Params) ->
    lists:foldl(fun(Param, Mesg) -> lists:keydelete(Param, 1, Mesg) end, KQML_mesg, Params);
del_param(KQML_mesg, Param) ->
    lists:keydelete(Param, 1, KQML_mesg).
