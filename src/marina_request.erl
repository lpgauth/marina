-module(marina_request).
-include("marina.hrl").

-export([
    execute/6,
    prepare/3,
    query/5,
    startup/1
]).

%% public
-spec execute(stream(), frame_flag(), statement_id(), [binary()], consistency(), flags()) ->
    binary().

execute(Stream, FrameFlags, StatementId, Values, ConsistencyLevel, Flags) ->
    ValuesCount = length(Values),
    EncodedValues = [marina_types:encode_bytes(Value) || Value <- Values],

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_EXECUTE,
        flags = FrameFlags,
        body = <<(marina_types:encode_short_bytes(StatementId))/binary,
            (marina_types:encode_short(ConsistencyLevel))/binary,
            Flags,
            (marina_types:encode_short(ValuesCount))/binary,
            (iolist_to_binary(EncodedValues))/binary>>
    }).

-spec prepare(stream(), frame_flag(), query()) -> binary().

prepare(Stream, FrameFlags, Query) ->
    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_PREPARE,
        flags = FrameFlags,
        body = <<(marina_types:encode_long_string(Query))/binary>>
    }).

-spec query(stream(), frame_flag(), query(), consistency(), flags()) -> binary().

query(Stream, FrameFlags, Query, ConsistencyLevel, Flags) ->
    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_QUERY,
        flags = FrameFlags,
        body = <<(marina_types:encode_long_string(Query))/binary,
            (marina_types:encode_short(ConsistencyLevel))/binary, Flags>>
    }).

-spec startup(frame_flag()) -> binary().

startup(FrameFlags) ->
    Body = case FrameFlags of
        1 -> [?CQL_VERSION, ?LZ4_COMPRESSION];
        0 -> [?CQL_VERSION]
    end,

    marina_frame:encode(#frame {
        stream = ?DEFAULT_STREAM,
        opcode = ?OP_STARTUP,
        flags = FrameFlags,
        body = marina_types:encode_string_map(Body)
    }).
