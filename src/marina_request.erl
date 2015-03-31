-module(marina_request).
-include("marina.hrl").

-export([
    query/4,
    startup/0
]).

%% public
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
