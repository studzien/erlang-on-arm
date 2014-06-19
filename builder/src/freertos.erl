-module(freertos).

%% API
-export([main/1]).

%%%===================================================================
%%% API
%%%===================================================================
main(Args) ->
    Opts = getopt:parse(options_spec(), Args),
    process_options(Opts).

%%%===================================================================
%%% Internal functions
%%%===================================================================
options_spec() ->
    [
        {module,   $m, "module",   {string, "main"},     "Entry point module"},
        {function, $f, "function", {string, "main"},     "Entry point function"},
        {argument, $a, "argument", string,               "Entry point argument"},
        {output,   $o, "output",   {string, "modules.h"},"Output file"}
    ].

process_options({ok, {_, []}}) ->
    usage();
process_options({ok, {Opts, Files}}) ->
    freertos_compiler:compile(Files, Opts);
process_options(_Other) ->
    usage().

usage() ->
    getopt:usage(options_spec(), "freertos", "erl-files").
