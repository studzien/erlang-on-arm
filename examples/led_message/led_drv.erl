-module(led_drv).

-export([init/4]).

init(GreenPort, GreenPin, RedPort, RedPin) ->
    lpc_gpio:output(GreenPort, GreenPin),
    lpc_gpio:high(GreenPort, GreenPin),
    loop(GreenPort, GreenPin, RedPort, RedPin, low).

loop(GreenPort, GreenPin, RedPort, RedPin, State) ->
    %lpc_debug:print_heap_size(),
    receive
        toggle ->
            lpc_gpio:State(GreenPort, GreenPin),
            lpc_gpio:low(RedPort, RedPin),
            loop(GreenPort, GreenPin, RedPort, RedPin, next(State));
        _ ->
            loop(GreenPort, GreenPin, RedPort, RedPin, State)
    after 250 ->
            lpc_gpio:high(RedPort, RedPin),
            loop(GreenPort, GreenPin, RedPort, RedPin, State)
    end.

next(low)  -> high;
next(high) -> low.
