-module(knitter_port_kek).
-author('$Author$').
-vsn('$Revision$').

-export([sendMessage/1]).



sendMessage(KQMLMesg) ->
    knitter_port_kek ! {sendMessage, KQMLMesg}.
