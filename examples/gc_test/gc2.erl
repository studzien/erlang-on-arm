-module(gc2).

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
    do_blink(GreenLed, [erlang:now()]).

do_blink({Port, Pin}=Led, State) ->
    lpc_gpio:high(Port, Pin),
    receive after 500 -> ok end,
    lpc_gpio:low(Port, Pin),
    receive after 500 -> ok end,
    lpc_debug:print_term(length(State)),
    lpc_debug:print_term(Led),
    lpc_debug:print_term(State),
    State1 = case erlang:length(State) of
        L when L > 4 ->
            [_|Rest] = lists:reverse(State),
            [erlang:now()|lists:reverse(Rest)];
        _ ->
            [erlang:now()|State]
    end,
    do_blink(Led, State1).

children() ->
    [#led{green = {2,0},  red = {2,1}}].
