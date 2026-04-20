-module(marina_frame_tests).

-include("test.hrl").

encode_decode_roundtrip_test_() ->
    [{lists:flatten(io_lib:format(
        "stream=~p flags=~p opcode=~p body_size=~p",
        [Stream, Flags, Opcode, byte_size(Body)])),
      fun () -> assert_roundtrip(Stream, Flags, Opcode, Body) end}
     || Stream <- [0, 1, -1, ?MAX_STREAM_ID - 1],
        Flags <- [0, 1, 2, 4, 8, 15],
        Opcode <- [?OP_QUERY, ?OP_RESULT, ?OP_EVENT, ?OP_BATCH],
        Body <- [<<>>, <<"x">>, crypto:strong_rand_bytes(4096)]].

assert_roundtrip(Stream, Flags, Opcode, Body) ->
    Encoded = iolist_to_binary(marina_frame:encode(#frame {
        stream = Stream,
        flags = Flags,
        opcode = Opcode,
        body = Body
    })),
    %% encode/1 emits with the request bit (high bit = 0); decode/1
    %% only parses response frames (high bit = 1). Flip it to simulate
    %% the server side echoing the same body back.
    {<<>>, [Decoded]} = marina_frame:decode(flip_to_response(Encoded)),
    ?assertEqual(Stream, Decoded#frame.stream),
    ?assertEqual(Flags, Decoded#frame.flags),
    ?assertEqual(Opcode, Decoded#frame.opcode),
    ?assertEqual(Body, Decoded#frame.body).

flip_to_response(<<V:8, Rest/binary>>) ->
    <<(V bor 16#80):8, Rest/binary>>.

decode_splits_multiple_frames_test() ->
    F1 = flip_to_response(iolist_to_binary(marina_frame:encode(
        #frame {stream = 1, flags = 0, opcode = ?OP_READY, body = <<>>}))),
    F2 = flip_to_response(iolist_to_binary(marina_frame:encode(
        #frame {stream = 2, flags = 0, opcode = ?OP_READY, body = <<"ok">>}))),
    {<<>>, [D2, D1]} = marina_frame:decode(<<F1/binary, F2/binary>>),
    ?assertEqual(1, D1#frame.stream),
    ?assertEqual(2, D2#frame.stream).

decode_leaves_partial_frame_in_rest_test() ->
    Full = flip_to_response(iolist_to_binary(marina_frame:encode(#frame {
        stream = 1, flags = 0, opcode = ?OP_READY, body = <<"abcdef">>
    }))),
    %% Slice off the last 3 bytes so the frame header parses but the
    %% body is short. decode/1 should return [] and keep the partial.
    Short = binary:part(Full, 0, byte_size(Full) - 3),
    {Rest, []} = marina_frame:decode(Short),
    ?assertEqual(Short, Rest).

pending_size_knows_wire_length_test() ->
    Response = flip_to_response(iolist_to_binary(marina_frame:encode(#frame {
        stream = 7, flags = 0, opcode = ?OP_READY, body = <<"hello">>
    }))),
    ?assertEqual(byte_size(Response), marina_frame:pending_size(Response)).

pending_size_undefined_for_partial_header_test() ->
    ?assertEqual(undefined, marina_frame:pending_size(<<>>)),
    ?assertEqual(undefined, marina_frame:pending_size(<<16#84, 0, 0>>)).
