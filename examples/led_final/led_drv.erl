-module(led_drv).

-export([init/3]).

init(Id, GreenPort, GreenPin) ->
    lpc_gpio:output(GreenPort, GreenPin),
    lpc_gpio:high(GreenPort, GreenPin),
    loop(Id, GreenPort, GreenPin, 0, low).

loop(Id, _, _, 5, _) ->
    exit({timeout, Id});
loop(Id, GreenPort, GreenPin, Timeouts, State) ->
    receive
        toggle ->
            lpc_gpio:State(GreenPort, GreenPin),
            loop(Id, GreenPort, GreenPin, Timeouts, next(State));
        _ ->
            loop(Id, GreenPort, GreenPin, Timeouts, State)
    after 1500 ->
            loop(Id, GreenPort, GreenPin, Timeouts+1, State)
    end.

next(low)  -> high;
next(high) -> low.
