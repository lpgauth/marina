-module(marina_request).
-include("marina_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).
-compile({no_auto_import, [register/2]}).

-export([
    auth_response/3,
    batch/4,
    execute/4,
    options/1,
    prepare/3,
    query/4,
    register/3,
    startup/1
]).

%% public
-spec auth_response(frame_flag(), binary(), binary()) -> iolist().

auth_response(FrameFlags, Username, Password) ->
    Body = <<0, Username/binary, 0, Password/binary>>,
    Body2 = encode_body(FrameFlags, [marina_types:encode_bytes(Body)]),

    marina_frame:encode(#frame {
        stream = ?DEFAULT_STREAM,
        opcode = ?OP_AUTH_RESPONSE,
        flags = FrameFlags,
        body = Body2
    }).

-spec batch(stream(), frame_flag(), [batch_query()], query_opts()) -> iolist().

batch(Stream, FrameFlags, Queries, QueryOpts) ->
    BatchType = encode_batch_type(
        marina_utils:query_opts(batch_type, QueryOpts)),
    ConsistencyLevel = marina_utils:query_opts(consistency_level, QueryOpts),
    EncodedQueries = [encode_batch_query(Q) || Q <- Queries],

    Body = encode_body(FrameFlags, [
        BatchType,
        marina_types:encode_short(length(Queries)),
        EncodedQueries,
        marina_types:encode_short(ConsistencyLevel),
        <<0>>
    ]),

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_BATCH,
        flags = FrameFlags,
        body = Body
    }).

-spec execute(stream(), frame_flag(), statement_id(), query_opts()) -> iolist().

execute(Stream, FrameFlags, StatementId, QueryOpts) ->
    ConsistencyLevel = marina_utils:query_opts(consistency_level, QueryOpts),
    Flags = flags(QueryOpts),

    Body2 = encode_body(FrameFlags, [
        marina_types:encode_short_bytes(StatementId),
        marina_types:encode_short(ConsistencyLevel),
        Flags
    ]),

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_EXECUTE,
        flags = FrameFlags,
        body = Body2
    }).

-spec options(frame_flag()) -> iolist().

options(FrameFlags) ->
    marina_frame:encode(#frame {
        stream = ?DEFAULT_STREAM,
        opcode = ?OP_OPTIONS,
        flags = FrameFlags,
        body = []
    }).

-spec prepare(stream(), frame_flag(), query()) -> iolist().

prepare(Stream, FrameFlags, Query) ->
    Body2 = encode_body(FrameFlags, [
        marina_types:encode_long_string(Query)
    ]),

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_PREPARE,
        flags = FrameFlags,
        body = Body2
    }).

-spec query(stream(), frame_flag(), query(), query_opts()) ->
    iolist().

query(Stream, FrameFlags, Query, QueryOpts) ->
    ConsistencyLevel = maps:get(consistency_level, QueryOpts,
        ?DEFAULT_CONSISTENCY_LEVEL),
    Flags = flags(QueryOpts),

    Body2 = encode_body(FrameFlags, [
        marina_types:encode_long_string(Query),
        marina_types:encode_short(ConsistencyLevel),
        Flags
    ]),

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_QUERY,
        flags = FrameFlags,
        body = Body2
    }).

-spec register(stream(), frame_flag(), [binary()]) -> iolist().

register(Stream, FrameFlags, EventTypes) ->
    Body = encode_body(FrameFlags,
        [marina_types:encode_string_list(EventTypes)]),

    marina_frame:encode(#frame {
        stream = Stream,
        opcode = ?OP_REGISTER,
        flags = FrameFlags,
        body = Body
    }).

-spec startup(frame_flag()) -> iolist().

startup(FrameFlags) ->
    Body = case FrameFlags of
        1 ->
            [?CQL_VERSION, ?LZ4_COMPRESSION];
        0 ->
            [?CQL_VERSION]
    end,

    marina_frame:encode(#frame {
        stream = ?DEFAULT_STREAM,
        opcode = ?OP_STARTUP,
        flags = 0,
        body = [marina_types:encode_string_map(Body)]
    }).

%% private
encode_body(0, Body) ->
    Body;
encode_body(1, Body) ->
    {ok, Body2} = marina_utils:pack(Body),
    Body2.

encode_batch_type(logged) -> <<0>>;
encode_batch_type(unlogged) -> <<1>>;
encode_batch_type(counter) -> <<2>>.

encode_batch_query({query, Query, Values}) ->
    [<<0>>, marina_types:encode_long_string(Query), encode_batch_values(Values)];
encode_batch_query({prepared, StatementId, Values}) ->
    [<<1>>, marina_types:encode_short_bytes(StatementId),
     encode_batch_values(Values)].

encode_batch_values(Values) ->
    [marina_types:encode_short(length(Values)),
     [marina_types:encode_bytes(V) || V <- Values]].

flags(QueryOpts) ->
    {Mask1, Values} = values_flag(QueryOpts),
    Mask2 = skip_metadata(QueryOpts),
    {Mask3, PageSize} = page_size_flag(QueryOpts),
    {Mask4, PagingState} = paging_state(QueryOpts),
    Flags = Mask1 + Mask2 + Mask3 + Mask4,
    [Flags, Values, PageSize, PagingState].

page_size_flag(QueryOpts) ->
    case marina_utils:query_opts(page_size, QueryOpts) of
        undefined ->
            {0, []};
        PageSize ->
            {4, marina_types:encode_int(PageSize)}
    end.

paging_state(QueryOpts) ->
    case marina_utils:query_opts(paging_state, QueryOpts) of
        undefined ->
            {0, []};
        PagingState ->
            {8, marina_types:encode_bytes(PagingState)}
    end.

skip_metadata(QueryOpts) ->
    case marina_utils:query_opts(skip_metadata, QueryOpts) of
        false -> 0;
        true -> 2
    end.

values_flag(QueryOpts) ->
    case marina_utils:query_opts(values, QueryOpts) of
        undefined ->
            {0, []};
        Values ->
            ValuesCount = length(Values),
            EncodedValues = [marina_types:encode_bytes(Value) ||
                Value <- Values],
            Values2 = [marina_types:encode_short(ValuesCount),
                EncodedValues],
            {1, Values2}
    end.
