-module(copy2).

-export([start/0,
         blink/1]).

-record(led, {green,
              red}).

start() ->
    [spawn(?MODULE, blink, [[Green, Red]]) || #led{green=Green, red=Red} <- children()].

blink([{GreenPort, GreenPin}=GreenLed, {RedPort, RedPin}]=Leds) ->
    lpc_gpio:output(GreenPort, GreenPin),
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:low(RedPort, RedPin),
    lpc_debug:print_term(Leds),
    lpc_debug:print_info(),
    do_blink(GreenLed).

do_blink({Port, Pin}=Led) ->
    lpc_gpio:high(Port, Pin),
    receive after 500 -> ok end,
    lpc_gpio:low(Port, Pin),
    receive after 500 -> ok end,
    do_blink(Led).

children() ->
    [#led{green = {2,0},  red = {2,1}},
     #led{green = {2,2},  red = {2,3}}].
