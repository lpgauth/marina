-module(marina_frame).
-include("marina_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    decode/1,
    encode/1,
    pending_size/1
]).

%% public
-spec decode(binary()) -> {binary(), [frame()]}.

decode(Bin) ->
    decode(Bin, []).

-spec encode(frame()) -> iolist().

encode(#frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body
    }) ->

    [<<0:1, ?PROTO_VERSION:7/unsigned-integer, Flags:8/unsigned-integer,
        Stream:16/signed-integer, Opcode:8/unsigned-integer,
        (iolist_size(Body)):32/unsigned-integer>>, Body].

-spec pending_size(binary()) -> pos_integer() | undefined.

pending_size(<<1:1, ?PROTO_VERSION:7/unsigned-integer,
    _Flags:8/unsigned-integer, _Stream:16/signed-integer,
    _Opcode:8/unsigned-integer, Length:32/unsigned-integer,
    _Rest/binary>>) ->

    Length + ?HEADER_SIZE;
pending_size(_) ->
    undefined.

%% private
decode(<<1:1, ?PROTO_VERSION:7/unsigned-integer, Flags:8/unsigned-integer,
    Stream:16/signed-integer, Opcode:8/unsigned-integer,
    Length:32/unsigned-integer, Body:Length/binary, Rest/binary>>, Acc) ->

    decode(Rest, [#frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body
    } | Acc]);
decode(Rest, Acc) ->
    {Rest, Acc}.
