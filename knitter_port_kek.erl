-module(knitter_port_kek).
-vc('$Id$').
-author('$Author$').
-vsn('$Revision$').

-export([sendMessage/1]).



sendMessage(KQMLMesg) ->
    knitter_port_kek ! {sendMessage, KQMLMesg}.
