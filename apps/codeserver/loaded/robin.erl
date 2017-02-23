-module(robin).
-export([seq/2]).

seq(From, To) ->
    lists:seq(From, To).
