-module(gc1).

-export([start/0,
         blink/1]).

-record(led, {green,
              red}).

start() ->
    [spawn(?MODULE, blink, [[Green, Red]])
     || #led{green=Green, red=Red} <- children()].

blink([{GreenPort, GreenPin}=GreenLed, {RedPort, RedPin}]) ->
    lpc_gpio:output(GreenPort, GreenPin),
    lpc_gpio:output(RedPort, RedPin),
    lpc_gpio:low(RedPort, RedPin),
    lpc_debug:print_info(),
    do_blink(GreenLed, erlang:now()).

do_blink({Port, Pin}=Led, State) ->
    lpc_gpio:high(Port, Pin),
    receive after 500 -> ok end,
    lpc_gpio:low(Port, Pin),
    receive after 500 -> ok end,
    lpc_debug:print_term(Led),
    lpc_debug:print_term(State),
    do_blink(Led, erlang:now()).

children() ->
    [#led{green = {2,0},  red = {2,1}}].
