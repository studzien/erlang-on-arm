-module(led_sup).

-export([start/0]).

-record(led, {green, %% {Port, Pin}
              red,   %% {Port, Pin}
              pid
             }).

start() ->
    lpc_debug:print_heap_size(),
    init().

init() ->
    Leds = [start_led(Led) || Led <- children()],
    loop(Leds). 

loop(Leds) ->
    [Pid ! toggle || #led{pid=Pid} <- Leds],
    lpc_debug:print_heap_size(),
    receive after 500 -> ok end,
    loop(Leds).


start_led(#led{green={GreenPort,GreenPin}, red={RedPort,RedPin}}=Led) ->
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:low(RedPort, RedPin),
    Pid = spawn(led_drv, init, [GreenPort, GreenPin, RedPort, RedPin]),
    Led#led{pid=Pid}.

children() ->
    %[#led{green = {2,0},  red = {2,1}},
     %#led{green = {2,2},  red = {2,3}},
     %#led{green = {2,4},  red = {2,5}},
     %#led{green = {2,6},  red = {2,7}},
     %#led{green = {2,8},  red = {2,10}}].
    [#led{green = {2,0},  red = {2,1}},
     #led{green = {2,2},  red = {2,3}},
     #led{green = {2,4},  red = {2,5}}].
