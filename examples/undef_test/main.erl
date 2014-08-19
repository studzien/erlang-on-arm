-module(main).

-export([main/0]).

main() ->
    lpc_debug:print_term(erlang:now()),
    dummy_module:dummy_function().
