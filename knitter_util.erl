-module(knitter_util).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([server/2]).
-export([keysdelete/3, keyssearch/3]).



start(Prefix) ->
    spawn(knitter_util, server, [Prefix, 1]).


server(Prefix, Counter) ->
    receive
	{From, get_new_id} ->
	    From ! {new_id, Prefix ++ integer_to_list(Counter)},
	    server(Prefix, Counter + 1);
	_ ->
	    server(Prefix, Counter)
    end.


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
