-module(main).

-export([main/0]).

main() ->
    led_sup:start().
