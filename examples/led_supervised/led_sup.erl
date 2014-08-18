-module(led_sup).

-export([start/0]).

-record(led, {green, %% {Port, Pin}
              red,   %% {Port, Pin}
              pid
             }).

start() ->
    init().

init() ->
    Leds = [start_led(Led) || Led <- children()],
    loop(Leds). 

loop(Leds) ->
    RandomLeds = random_leds(Leds, length(Leds) div 2),
    [Pid ! {toggle, erlang:now()} || #led{pid=Pid} <- RandomLeds],
    receive after 1000 -> ok end,
    loop(Leds).

random_leds(Leds, Count) ->
    random_leds(Leds, Count, []).

random_leds(_Leds, 0, Acc) ->
    Acc;
random_leds(Leds, N, Acc) ->
    Nth = random:uniform(length(Leds)),
    Led = lists:nth(Nth, Leds),
    random_leds(lists:delete(Led, Leds), N-1, [Led|Acc]).

start_led(#led{green={GreenPort,GreenPin}, red={RedPort,RedPin}}=Led) ->
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:low(RedPort, RedPin),
    Pid = spawn(led_drv, init, [GreenPort, GreenPin, RedPort, RedPin]),
    Led#led{pid=Pid}.

children() ->
    [#led{green = {2,0},  red = {2,1}},
     #led{green = {2,2},  red = {2,3}},
     #led{green = {2,4},  red = {2,5}},
     #led{green = {2,6},  red = {2,7}}].
