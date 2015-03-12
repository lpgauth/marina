-module(marina_body).
-include("marina.hrl").

-export([
    decode/1
]).

%% public
-spec decode(frame()) -> {ok, term()} | {error, atom()}.
decode(#frame {opcode = ?OP_READY}) ->
    {ok, undefined};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<1:32/integer, _Rest/binary>>
    }) ->

    {ok, undefined};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<2:32/integer, Rest/binary>>
    }) ->

    {ok, Rest};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<3:32/integer, Rest/binary>>
    }) ->

    {ok, Rest};
decode(#frame {}) ->
    {error, unknown_response}.