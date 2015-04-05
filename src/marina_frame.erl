-module(marina_frame).
-include("marina.hrl").

-export([
    decode/1,
    encode/1,
    flags/1,
    pending_size/1
]).

%% public
-spec decode(binary()) -> {binary(), [frame()]}.

decode(Bin) ->
    decode(Bin, []).

-spec encode(frame()) -> binary().

encode(#frame {
        stream = Stream,
        opcode = ?OP_STARTUP,
        body = Body
    }) ->

    [<<0:1, ?PROTO_VERSION:7/unsigned-integer, ?DEFAULT_FLAGS:8/unsigned-integer,
        Stream:16/signed-integer, ?OP_STARTUP:8/unsigned-integer,
        (size(Body)):32/unsigned-integer>>, Body];
encode(#frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body
    }) ->

    Body2 = encode_body(Flags, Body),
    [<<0:1, ?PROTO_VERSION:7/unsigned-integer, Flags:8/unsigned-integer,
        Stream:16/signed-integer, Opcode:8/unsigned-integer,
        (size(Body2)):32/unsigned-integer>>, Body2].

-spec flags(boolean()) -> frame_flag().

flags(true) -> 1;
flags(false) -> 0.

-spec pending_size(binary()) -> pos_integer() | undefined.

pending_size(<<1:1, ?PROTO_VERSION:7/unsigned-integer, _Flags:8/unsigned-integer,
        _Stream:16/signed-integer, _Opcode:8/unsigned-integer,
        Length:32/unsigned-integer, _Rest/binary>>) ->

    Length + ?HEADER_SIZE;
pending_size(_) ->
    undefined.

%% private
decode_body(0, Body) ->
    Body;
decode_body(1, Body) ->
    {ok, Body2} = lz4:unpack(Body),
    Body2.

decode(<<1:1, ?PROTO_VERSION:7/unsigned-integer, Flags:8/unsigned-integer,
        Stream:16/signed-integer, Opcode:8/unsigned-integer,
        Length:32/unsigned-integer, Body:Length/binary, Rest/binary>>, Acc) ->

    Body2 = decode_body(Flags, Body),
    decode(Rest, [#frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body2
    } | Acc]);
decode(Rest, Acc) ->
    {Rest, Acc}.

encode_body(0, Body) ->
    Body;
encode_body(1, Body) ->
    {ok, Body2} = lz4:pack(Body),
    Body2.
