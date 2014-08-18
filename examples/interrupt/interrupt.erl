-module(interrupt).

-export([start/0]).

start() ->
    lpc_gpio:input(0, 1),
    lpc_gpio:interrupt(0, 1, falling),
    loop().

loop() ->
    receive
        interrupt ->
            lpc_debug:print_term(interrupt)
    end,
    loop().
