-module(knitter_util).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1]).
-export([server/1]).
-export([keysdelete/3, keyssearch/3]).

-include("knitter_util.hrl").



start(Prefix) ->
    spawn(?MODULE, server, [#server_state{prefix = Prefix}]).


server(State) ->
    receive
	{From, get_new_id} ->
	    From ! {new_id, State#server_state.prefix ++ integer_to_list(State#server_state.counter)},
	    server(State#server_state{counter = State#server_state.counter + 1});
	_ ->
	    server(State)
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
