-module(knitter_tr_simple).
-vc('$Id$ ').
-author('$Author$ ').
-vsn('$Revision$ ').

-export([start/1, stop/1]).
%-export([start_conversation/1, stop_conversation/1]).
-export([send/2]).


start(Agent) ->
    knitter_tr_simple_impl:start(Agent).


stop(Tr_simplePID) ->
    Tr_simplePID ! stop.


send(Tr_simplePID, Message) ->
    Tr_simplePID ! {sendMessage, Message},
    ok.
