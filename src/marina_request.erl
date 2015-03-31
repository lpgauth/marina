-module(marina_request).
-include("marina.hrl").

-export([
    execute/5,
    prepare/2,
    query/4,
    startup/0
]).

%% public
execute(Stream, StatementId, Values, ConsistencyLevel, Flags) ->
    ValuesCount = length(Values),
    EncodedValues = [marina_types:encode_bytes(Value) || Value <- Values],

    marina_frame:encode(#frame {
        flags = ?DEFAULT_FLAGS,
        stream = Stream,
        opcode = ?OP_EXECUTE,
        body = <<(marina_types:encode_short_bytes(StatementId))/binary,
            (marina_types:encode_short(ConsistencyLevel))/binary,
            Flags,
            (marina_types:encode_short(ValuesCount))/binary,
            (iolist_to_binary(EncodedValues))/binary>>
    }).

prepare(Stream, Query) ->
    marina_frame:encode(#frame {
        flags = ?DEFAULT_FLAGS,
        stream = Stream,
        opcode = ?OP_PREPARE,
        body = <<(marina_types:encode_long_string(Query))/binary>>
    }).

query(Stream, Query, ConsistencyLevel, Flags) ->
    marina_frame:encode(#frame {
        flags = ?DEFAULT_FLAGS,
        stream = Stream,
        opcode = ?OP_QUERY,
        body = <<(marina_types:encode_long_string(Query))/binary,
            (marina_types:encode_short(ConsistencyLevel))/binary, Flags>>
    }).

startup() ->
    marina_frame:encode(#frame {
        flags = ?DEFAULT_FLAGS,
        stream = ?DEFAULT_STREAM,
        opcode = ?OP_STARTUP,
        body = marina_types:encode_string_map([?CQL_VERSION])
    }).
