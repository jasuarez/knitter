-module(knitter_util).
-author('$Author$').
-vsn('$Revision$').

-export([keysdelete/3, keyssearch/3]).
-export([get_param/2, set_param/3]).



get_param(KQML_mesg, Param) ->
    case lists:keysearch(Param, 1, KQML_mesg) of
	{value, {Param, Value}} ->
		{ok, Value};
	 false ->
		undef
	end.


set_param(KQML_mesg, Param, New_value) ->
    lists:keyreplace(Param, 1, KQML_mesg, {Param, New_value}).


keysdelete(Key, N, TupleList) ->
    keysdelete(Key, N, TupleList, []).


keysdelete(Key, N, [H | T], Accum) ->
    case catch element(N, H) of
	Key ->
	    keysdelete(Key, N, T, Accum);
	_ ->
	    keysdelete(Key, N, T, [H | Accum])
    end;
keysdelete(_, _, [], Accum) ->
    Accum.


keyssearch(Key, N, TupleList) ->
    keyssearch(Key, N, TupleList, []).


keyssearch(Key, N, [H | T], Accum) ->
    case catch element(N, H) of
	Key ->
	    keyssearch(Key, N, T, [H | Accum]);
	_ ->
	    keyssearch(Key, N, T, Accum)
    end;
keyssearch(_, _, [], Accum) ->
    case Accum of
	[] ->
	    false;
	_ ->
	    {values, Accum}
    end.
