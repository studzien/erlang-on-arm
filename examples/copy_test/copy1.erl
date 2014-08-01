-module(copy1).

-export([start/0,
         blink/1]).

-record(led, {green,
              red}).

start() ->
    [spawn(?MODULE, blink, [Green]) || #led{green=Green} <- children()].

blink({Port, Pin}=Led) ->
    lpc_gpio:output(Port, Pin),
    lpc_debug:print_term(Led),
    lpc_debug:print_info(),
    do_blink(Led).

do_blink({Port, Pin}=Led) ->
    lpc_gpio:high(Port, Pin),
    receive after 500 -> ok end,
    lpc_gpio:low(Port, Pin),
    receive after 500 -> ok end,
    do_blink(Led).

children() ->
    [#led{green = {2,0},  red = {2,1}},
     #led{green = {2,2},  red = {2,3}}].
