-module(led_sup).

-export([start/0]).

-record(led, {id,
              green, %% {Port, Pin}
              red,   %% {Port, Pin}
              pid
             }).

start() ->
    init().

init() ->
    process_flag(trap_exit, true),
    Leds = [start_led(Led) || Led <- children()],
    self() ! toggle,
    loop(Leds). 

loop(Leds) ->
    receive
        toggle ->
            handle_toggle(Leds);
        {'EXIT', _Pid, {timeout, Id}} ->
            handle_exit(Id, Leds);
        {restart, Id} ->
            handle_restart(Id, Leds);
        _ ->
            loop(Leds)
    end.

handle_toggle(Leds) ->
    RandomLeds = random_leds(Leds, length(Leds) div 2),
    [Pid ! toggle || #led{pid=Pid} <- RandomLeds],
    erlang:send_after(500, self(), toggle),
    loop(Leds).

handle_exit(Id, Leds) ->
    Led = lists:keyfind(Id, #led.id, Leds),
    #led{red={RedPort, RedPin}} = Led,
    lpc_gpio:low(RedPort, RedPin),
    erlang:send_after(1000, self(), {restart, Id}),
    loop(Leds).

handle_restart(Id, Leds) ->
    Led = lists:keyfind(Id, #led.id, Leds),
    Led1 = start_led(Led),
    Leds1 = lists:keyreplace(Id, #led.id, Leds, Led1),
    loop(Leds1).

random_leds(Leds, Count) ->
    random_leds(Leds, Count, []).

random_leds(_Leds, 0, Acc) ->
    Acc;
random_leds(Leds, N, Acc) ->
    Nth = random:uniform(length(Leds)),
    Led = lists:nth(Nth, Leds),
    random_leds(lists:delete(Led, Leds), N-1, [Led|Acc]).

start_led(#led{id = Id,
               green={GreenPort,GreenPin},
               red={RedPort,RedPin}}=Led) ->
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:high(RedPort, RedPin),
    Pid = spawn_link(led_drv, init, [Id, GreenPort, GreenPin]),
    Led#led{pid=Pid}.

children() ->
    [#led{id = 1, green = {2,0},  red = {2,1}},
     #led{id = 2, green = {2,2},  red = {2,3}},
     #led{id = 3, green = {2,4},  red = {2,5}}].
