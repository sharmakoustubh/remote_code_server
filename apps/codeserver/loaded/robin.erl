-module(robin).
-compile(export_all).

seq(From, To) ->
    lists:seq(From, To).

map(F, L) ->
    lists:map(F, L).

sleep(Milliseconds) ->
    timer:sleep(Milliseconds).
