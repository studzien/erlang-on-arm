-module(copy3).

-export([start/0,
         blink/1]).

-record(led, {green,
              red}).

start() ->
    [spawn(?MODULE, blink, [[[{GPort,dummy},{GPin,dummy}],
                             [{RPort,dummy},{RPin,dummy}]]]) ||
        #led{green={GPort,GPin}, red={RPort,RPin}} <- children()].

blink([[{GreenPort,dummy},{GreenPin,dummy}],
       [{RedPort, dummy},{RedPin, dummy}]]=Leds) ->
    lpc_gpio:output(GreenPort, GreenPin),
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:low(RedPort, RedPin),
    lpc_debug:print_term(Leds),
    lpc_debug:print_info(),
    do_blink({GreenPort, GreenPin}).

do_blink({Port, Pin}=Led) ->
    lpc_gpio:high(Port, Pin),
    receive after 500 -> ok end,
    lpc_gpio:low(Port, Pin),
    receive after 500 -> ok end,
    do_blink(Led).

children() ->
    [#led{green = {2,0},  red = {2,1}},
     #led{green = {2,2},  red = {2,3}}].
