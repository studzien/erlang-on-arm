-module(fac2).

-export([fac/1]).

fac(N) ->
    fac(N, 1).

fac(0, Acc) ->
    Acc;
fac(N, Acc) ->
    fac(N-1, N*Acc).
