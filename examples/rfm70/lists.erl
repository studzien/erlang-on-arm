-module(lists).

-export([duplicate/2,
         nthtail/2]).

duplicate(N, Elem) ->
    duplicate(N, Elem, []).

duplicate(0, _, Acc) ->
    Acc;
duplicate(N, Elem, Acc) ->
    duplicate(N-1, Elem, [Elem|Acc]).

nthtail(0, List) ->
    List;
nthtail(N, [_|Tail]) ->
    nthtail(N-1, Tail).
