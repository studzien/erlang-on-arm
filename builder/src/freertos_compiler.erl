-module(freertos_compiler).

%% API
-export([compile/2]).

%%%===================================================================
%%% API
%%%===================================================================
compile(Files, Opts) ->
    erlydtl:compile_template(template(), freertos_template),
    {Modules, Beams} = compile(Files, Opts, [], []),
    ModuleL = proplists:get_value(module, Opts),
    FunctionL = proplists:get_value(function, Opts),
    Args = arguments(Opts, []),
    Vars = [{modules_n,        length(Modules)},
            {modules_code,     generate_code(lists:zip(Modules, Beams), [])},
            {modules_list,     generate_list(Modules)},
            {entrypoint_arity, length(Args)},
            {entrypoint_m,     ModuleL},
            {entrypoint_m_len, length(ModuleL)},
            {entrypoint_f,     FunctionL},
            {entrypoint_f_len, length(FunctionL)}],
    {ok, Output} = freertos_template:render(Vars), 
    file:write_file(proplists:get_value(output, Opts), Output).

%%%===================================================================
%%% Internal functions
%%%===================================================================
compile([], _Opts, Modules, Beams) ->
    {Modules, Beams};
compile([File|Rest], Opts, Modules, Beams) ->
    {ok, Module, Beam} = compile:file(File, [binary]),
    ModuleFixed = list_to_atom("module_" ++ atom_to_list(Module)),
    UncompressedBeam = uncompress_literals(Beam),
    io:format("Size of ~p module file is ~p~n",
              [Module, byte_size(UncompressedBeam)]),
    compile(Rest, Opts, [ModuleFixed|Modules], [UncompressedBeam|Beams]).

generate_code([], Acc) ->
    Acc;
generate_code([{Module,Beam}|Rest], Acc) ->
    Bytes = [integer_to_list(Byte) || Byte <- binary_to_list(Beam)],
    Output = ["const byte ", Module, "[] = {", string:join(Bytes, ","), "};\n"],
    generate_code(Rest, [Output|Acc]).

generate_list(Modules) ->
    string:join([atom_to_list(M) || M <- lists:reverse(Modules)], ",").

uncompress_literals(Beam) ->
    case beam_lib:chunks(Beam, ["LitT"]) of
        {ok, {_, [{"LitT", <<_Uncompressed:32, Lit/binary>>}]}} ->
            NewLit = zlib:uncompress(Lit),
            swap_literals(Beam, NewLit, <<>>);
        _ ->
            Beam
    end.

swap_literals(<<>>, _Lit, <<"FOR1", _:32, Rest/binary>>) ->
    <<"FOR1", (byte_size(Rest)):32, Rest/binary>>;
swap_literals(<<"LitT",Size:32,Rest/binary>>, Lit, Acc) ->
    LitSize = byte_size(Lit),
    Padding = 4-(LitSize rem 4),
    PaddingBits = case Padding of
        4 -> 0;
        _ -> Padding*8
    end,
    Acc1 = <<Acc/binary,
             "LitT",
             (byte_size(Lit)):32,
             Lit/binary,
             0:PaddingBits>>,
    SkipBits = case Size rem 4 of
        0 -> Size*8;
        R -> (Size+4-R)*8
    end,
    <<_:SkipBits,Rest1/binary>> = Rest,
    swap_literals(Rest1, Lit, Acc1);
swap_literals(<<Byte:8, Rest/binary>>, Lit, Acc) ->
    swap_literals(Rest, Lit, <<Acc/binary, Byte:8>>).

arguments([], Acc) ->
    lists:reverse(Acc);
arguments([{argument, Arg}|Rest], Acc) ->
    arguments(Rest, [Arg|Acc]);
arguments([_|Rest], Acc) ->
    arguments(Rest, Acc).

template() ->
    <<"#ifndef MODULES_H_\n"
      "#define MODULES_H_\n"
      "#define MODULES_N {{modules_n}}\n\n"
      "#define ENTRYPOINT_ARITY {{entrypoint_arity}}\n"
      "#define ENTRYPOINT_M \"{{entrypoint_m}}\"\n"
      "#define ENTRYPOINT_F \"{{entrypoint_f}}\"\n"
      "#define ENTRYPOINT_M_LEN {{entrypoint_m_len}}\n"
      "#define ENTRYPOINT_F_LEN {{entrypoint_f_len}}\n\n"
      "{{modules_code}}\n"
      "const byte* code[] = { {{modules_list}} };\n\n"
      "const Eterm entrypoint_a[] = {};\n"
      "#endif">>.
