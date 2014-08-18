-module(lists).

-export([nth/2,
         delete/2,
         keyfind/3,
         keyreplace/4]).

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

keyfind(_, _, []) ->
    false;
keyfind(Key, N, [Elem|Rest]) ->
    case element(N, Elem) of
        Key -> Elem;
        _   -> keyfind(Key, N, Rest)
    end.

keyreplace(Key, N, TupleList, NewTuple) ->
    keyreplace(Key, N, TupleList, NewTuple, []).

keyreplace(_, _, [], _, Acc) ->
    lists:reverse(Acc);
keyreplace(Key, N, [Elem|Rest], NewTuple, Acc) ->
    case element(N, Elem) of
        Key ->
            lists:reverse([NewTuple|Acc]) ++ Rest;
        _   ->
            keyreplace(Key, N, Rest, NewTuple, [Elem|Acc])
    end.
