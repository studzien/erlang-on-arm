-module(led_drv).

-export([init/2]).

-define(TIMEOUT, 1000).

init(GreenPort, GreenPin) ->
    lpc_gpio:output(GreenPort, GreenPin),
    loop(GreenPort, GreenPin).

loop(GreenPort, GreenPin) ->
    lpc_debug:print_heap_size(),
    lpc_gpio:low(GreenPort, GreenPin),
    receive after ?TIMEOUT -> ok end,
    lpc_gpio:high(GreenPort, GreenPin),
    receive after ?TIMEOUT -> ok end,
    loop(GreenPort, GreenPin).
