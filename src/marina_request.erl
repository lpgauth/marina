-module(marina_request).
-include("marina.hrl").

-export([
    query/2,
    startup/0
]).

%% public
query(StreamId, Query) ->
    marina_frame:encode(#frame {
        flags = 0,
        stream = StreamId,
        opcode = ?OP_QUERY,
        body = <<(marina_types:encode_long_string(Query))/binary,
            (marina_types:encode_short(?CONSISTENCY_ONE))/binary, 0>>
    }).

startup() ->
    marina_frame:encode(#frame {
        flags = 0,
        stream = 0,
        opcode = ?OP_STARTUP,
        body = marina_types:encode_string_map([
            {<<"CQL_VERSION">>, ?CQL_VERSION}
        ])
    }).
