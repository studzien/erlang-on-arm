-module(main).

-export([main/0]).

main() ->
    Callback = fun(M) ->
            lpc_debug:print_term(erlang:now()),
            lpc_debug:print_term(M)
    end,
    rfm70:start([Callback]).
