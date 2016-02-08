-module(marina_client).
-include("marina.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    options/0,
    init/0,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer      = marina_buffer:new(),
    frame_flags = [] :: [frame_flag()],
    keyspace    = undefined,
    requests    = 0
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec options() -> {ok, shackle:client_options()}.

options() ->
    Ip = marina_utils:get_env(ip, ?DEFAULT_IP),
    Port = marina_utils:get_env(port, ?DEFAULT_PORT),
    Reconnect = marina_utils:get_env(reconnect, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = marina_utils:get_env(reconnect_time_max,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = marina_utils:get_env(reconnect_time_min,
        ?DEFAULT_RECONNECT_MIN),

    {ok, [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {reconnect_time_max, ReconnectTimeMax},
        {reconnect_time_min, ReconnectTimeMin},
        {socket_options, [
            binary,
            {packet, raw},
            {send_timeout, 50},
            {send_timeout_close, true}
        ]}
    ]}.

-spec init() -> {ok, state()}.

init() ->
    Keyspace = marina_utils:get_env(keyspace, undefined),

    {ok, #state {
        frame_flags = frame_flags(),
        keyspace = Keyspace
    }}.

-spec setup(inet:socket(), state()) -> {ok, state()} |
    {error, atom(), state()}.

setup(Socket, #state {frame_flags = FrameFlags} = State) ->
    Msg = marina_request:startup(FrameFlags),
    case sync_msg(Socket, Msg) of
        {ok, undefined} ->
            set_keyspace(Socket, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec handle_request(term(), state()) ->
    {ok, pos_integer(), iodata(), state()}.

handle_request(Request, #state {
        frame_flags = FrameFlags,
        requests = Requests
    } = State) ->

    RequestId = Requests rem ?MAX_STREAM_ID,
    Data = case Request of
        {execute, StatementId, Values, ConsistencyLevel, Flags} ->
            marina_request:execute(RequestId, FrameFlags, StatementId, Values,
                ConsistencyLevel, Flags);
        {prepare, Query} ->
            marina_request:prepare(RequestId, FrameFlags, Query);
        {query, Query, Values, ConsistencyLevel, Flags} ->
            marina_request:query(RequestId, FrameFlags, Query, Values,
                ConsistencyLevel, Flags)
    end,

    {ok, RequestId, Data, State#state {
        requests = Requests + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()}.

handle_data(Data, #state {
        buffer = Buffer
    } = State) ->

    {Frames, Buffer2} = marina_buffer:decode(Data, Buffer),
    Replies = [{Frame#frame.stream, {ok, Frame}} || Frame <- Frames],

    {ok, Replies, State#state {
        buffer = Buffer2
    }}.

-spec terminate(state()) -> ok.

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
