-module(robin).
-export([seq/2, map/2]).

seq(From, To) ->
    lists:seq(From, To).

map(F, L) ->
    lists:map(F, L).



