-module(beamcode).

-compile(export_all).

pp_ops_hex(Binary) ->
    io:format("{"),
    [io:format("0x~2.16.0B,", [Op]) || Op <- ops(Binary)],
    io:format("}").

pp_ops(Binary) ->
    [io:format("~2.16.0B ~B~n", [Op, Op]) || Op <- ops(Binary)].

ops(Binary) ->
    ops(Binary, []).

ops(<<>>, Acc) -> lists:reverse(Acc);
ops(<<Op:8, Rest/binary>>, Acc) -> ops(Rest, [Op | Acc]).

parse_beam(<<"FOR1", Size:32, "BEAM", Rest/binary>>) ->
    parse_chunk(Rest, [{size, Size}]).

parse_chunk(<<>>, Acc) ->
    Acc;
parse_chunk(<<Chunk:32, Size:32, Data/binary>>, Acc) ->
    RealSize = case Size rem 4 of
        0 -> Size;
        Rem -> Size + (4-Rem)
    end,
    {ChunkData, Rest} = split_binary(Data, RealSize),
    parse_chunk(Rest, [{binary:encode_unsigned(Chunk), Size, ChunkData} | Acc]).

atoms(<<Size:32, Atoms/binary>>) ->
    {Size, atoms(Atoms, [])}.

atoms(<<>>, Acc) ->
    lists:reverse(Acc);
atoms(<<AtomSize:8, Data/binary>>, Acc) ->
    Size = AtomSize*8,
    <<Atom:Size, Rest/binary>> = Data,
    atoms(Rest, [binary:encode_unsigned(Atom)|Acc]).
