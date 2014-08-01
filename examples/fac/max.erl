-module(max).

-export([max/3]).

max(X, Y, Z) ->
    A = erlang:max(Y, X),
    erlang:max(A, Z).
