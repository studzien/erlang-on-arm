-module(main).

-export([main/0]).

main() ->
    spawn(fac2,fac,[25]),
    spawn(fac2,fac,[50]),
    spawn(fac2,fac,[75]),
    spawn(fac2,fac,[100]).
