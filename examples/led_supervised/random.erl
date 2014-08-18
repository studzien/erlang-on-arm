-module(random).

-export([uniform/1]).

uniform(N) ->
    {_, _, Seed} = erlang:now(),
    Result = Seed rem N + 1,
    Result.
