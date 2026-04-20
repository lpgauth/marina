-module(marina_buffer_tests).

-include("test.hrl").

new_is_empty_test() ->
    Buf = marina_buffer:new(),
    {[], _} = marina_buffer:decode(<<>>, Buf).

single_frame_test() ->
    Data = response(1, ?OP_READY, <<>>),
    {[Frame], _} = marina_buffer:decode(Data, marina_buffer:new()),
    ?assertEqual(1, Frame#frame.stream).

multiple_frames_in_one_chunk_test() ->
    F1 = response(1, ?OP_READY, <<>>),
    F2 = response(2, ?OP_READY, <<"ab">>),
    {Frames, _} = marina_buffer:decode(<<F1/binary, F2/binary>>,
        marina_buffer:new()),
    Streams = [F#frame.stream || F <- Frames],
    ?assertEqual([1, 2], lists:sort(Streams)).

splits_across_chunks_test() ->
    %% A frame arriving one byte at a time should decode when the last
    %% byte lands and not before.
    Frame = response(42, ?OP_READY, <<"hello">>),
    feed_byte_by_byte(Frame, 42).

splits_before_header_complete_test() ->
    %% Fewer than 9 bytes is not enough to read the header. Buffer must
    %% not panic and must emit the frame once the rest arrives.
    Frame = response(7, ?OP_READY, <<"world">>),
    H = binary:part(Frame, 0, 5),
    T = binary:part(Frame, 5, byte_size(Frame) - 5),
    B0 = marina_buffer:new(),
    {[], B1} = marina_buffer:decode(H, B0),
    {[F], _} = marina_buffer:decode(T, B1),
    ?assertEqual(7, F#frame.stream),
    ?assertEqual(<<"world">>, F#frame.body).

%% helpers
response(Stream, Opcode, Body) ->
    Encoded = iolist_to_binary(marina_frame:encode(#frame {
        stream = Stream, flags = 0, opcode = Opcode, body = Body
    })),
    <<V:8, Rest/binary>> = Encoded,
    <<(V bor 16#80):8, Rest/binary>>.

feed_byte_by_byte(Binary, ExpectedStream) ->
    feed_byte_by_byte(Binary, ExpectedStream, marina_buffer:new()).

feed_byte_by_byte(<<Last:8>>, ExpectedStream, Buf) ->
    {[F], _} = marina_buffer:decode(<<Last>>, Buf),
    ?assertEqual(ExpectedStream, F#frame.stream);
feed_byte_by_byte(<<Byte:8, Rest/binary>>, ExpectedStream, Buf) ->
    {[], Buf2} = marina_buffer:decode(<<Byte>>, Buf),
    feed_byte_by_byte(Rest, ExpectedStream, Buf2).
