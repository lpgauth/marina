-module(marina_frame).
-include("marina.hrl").

-export([
    decode/1,
    encode/1
]).

%% public
encode(#frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body
    }) ->

    <<0:1, ?PROTO_VERSION:7/unsigned-integer, Flags, Stream:16, Opcode, (size(Body)):32, Body/binary>>.

decode(<<1:1, ?PROTO_VERSION:7/unsigned-integer, Flags:8/unsigned-integer, Stream:16/signed-integer,
        Opcode:8/unsigned-integer, _Length:32/unsigned-integer, Body/binary>>) ->

    #frame {
        flags = Flags,
        stream = Stream,
        opcode = Opcode,
        body = Body
    }.