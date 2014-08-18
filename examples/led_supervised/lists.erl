-module(lists).

-export([nth/2,
         delete/2]).

nth(1, [Elem|_]) -> Elem;
nth(N, [_|Rest]) -> nth(N-1, Rest).

delete(Elem, List) ->
    delete(Elem, List, []).

delete(_, [], Acc) ->
    lists:reverse(Acc);
delete(Elem, [Elem|Rest], Acc) ->
    lists:reverse(Acc) ++ Rest;
delete(Elem, [Other|Rest], Acc) ->
    delete(Elem, Rest, [Other|Acc]).
