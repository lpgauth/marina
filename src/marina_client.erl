-module(marina_client).
-include("marina.hrl").

-behavior(shackle_client).
-export([
    after_connect/2,
    handle_data/2,
    handle_request/2,
    handle_timing/2,
    options/0,
    terminate/1
]).

-record(state, {
    buffer      = marina_buffer:new(),
    frame_flags = [] :: [frame_flag()],
    keyspace    = undefined,
    requests    = 0
}).

%% shackle_server callbacks
-spec after_connect(inet:socket(), #state {}) -> {ok, #state {}} |
    {error, atom(), #state {}}.

after_connect(Socket, #state {frame_flags = FrameFlags} = State) ->
    Msg = marina_request:startup(FrameFlags),
    case sync_msg(Socket, Msg) of
        {ok, undefined} ->
            set_keyspace(Socket, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec handle_data(binary(), #state {}) ->
    {ok, [{pos_integer(), term()}], #state {}}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    {Frames, Buffer2} = marina_buffer:decode(Data, Buffer),
    Replies = [{Frame#frame.stream, {ok, Frame}} || Frame <- Frames],

    {ok, Replies, State#state {
        buffer = Buffer2
    }}.

-spec handle_request(term(), #state {}) ->
    {ok, pos_integer(), iodata(), #state {}}.

handle_request(Request, #state {
        frame_flags = FrameFlags,
        requests = Requests
    } = State) ->

    RequestId = Requests rem ?MAX_STREAM_ID,
    Data = case Request of
        {execute, StatementId, Values, ConsistencyLevel, Flags} ->
            marina_request:execute(RequestId, FrameFlags, StatementId, Values, ConsistencyLevel, Flags);
        {prepare, Query} ->
            marina_request:prepare(RequestId, FrameFlags, Query);
        {query, Query, Values, ConsistencyLevel, Flags} ->
            marina_request:query(RequestId, FrameFlags, Query, Values, ConsistencyLevel, Flags)
    end,

    {ok, RequestId, Data, State#state {
        requests = Requests + 1
    }}.

-spec handle_timing(term(), [non_neg_integer()]) -> ok.

handle_timing(_Cast, _Timings) ->
    ok.

-spec options() -> {ok, [
    {ip, inet:ip_address() | inet:hostname()} |
    {port, inet:port_number()} |
    {reconnect, boolean()} |
    {state, #state {}}
]}.

options() ->
    Ip = application:get_env(?APP, ip, ?DEFAULT_IP),
    Keyspace = application:get_env(?APP, keyspace, undefined),
    Port = application:get_env(?APP, port, ?DEFAULT_PORT),
    Reconnect = application:get_env(?APP, reconnect, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = application:get_env(?APP, reconnect_time_max, ?DEFAULT_RECONNECT),
    ReconnectTimeMin = application:get_env(?APP, reconnect_time_min, ?DEFAULT_RECONNECT),

    {ok, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {reconnect_time_max, ReconnectTimeMax},
        {reconnect_time_min, ReconnectTimeMin},
        {state, #state {
            frame_flags = frame_flags(),
            keyspace = Keyspace
        }}
    ]}.

-spec terminate(#state {}) -> ok.

terminate(_State) ->
    ok.

%% private
frame_flags() ->
    Compression = application:get_env(?APP, compression, false),
    case Compression of
        true -> [{compression, true}];
        _ -> []
    end.

set_keyspace(_Socket, #state {keyspace = undefined} = State) ->
    {ok, State};
set_keyspace(Socket, #state {
        frame_flags = FrameFlags,
        keyspace = Keyspace
    } = State) ->

    Query = <<"USE \"", Keyspace/binary, "\"">>,
    Msg = marina_request:query(0, FrameFlags, Query, [], ?CONSISTENCY_ONE, []),
    case sync_msg(Socket, Msg) of
        {ok, Keyspace} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

sync_msg(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?DEFAULT_RECV_TIMEOUT) of
                {ok, Msg2} ->
                    {_Rest, [Frame | _]} = marina_frame:decode(Msg2),
                    marina_body:decode(Frame);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
