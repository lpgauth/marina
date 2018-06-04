-module(marina_client).
-include("marina_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    init/0,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer      = marina_buffer:new() :: buffer(),
    frame_flags = 0                   :: frame_flag(),
    keyspace    = undefined           :: binary() | undefined,
    requests    = 0                   :: non_neg_integer()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init() -> {ok, state()}.

init() ->
    Keyspace = ?GET_ENV(keyspace, undefined),

    {ok, #state {
        frame_flags = frame_flags(),
        keyspace = Keyspace
    }}.

-spec setup(inet:socket(), state()) -> {ok, state()} |
    {error, atom(), state()}.

setup(Socket, #state {frame_flags = FrameFlags} = State) ->
    Msg = marina_request:startup(FrameFlags),
    case marina_utils:sync_msg(Socket, Msg) of
        {ok, undefined} ->
            set_keyspace(Socket, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec handle_request(term(), state()) ->
    {ok, pos_integer(), iodata(), state()}.

handle_request({Request, QueryOpts}, #state {
        frame_flags = FrameFlags,
        requests = Requests
    } = State) ->

    RequestId = Requests rem ?MAX_STREAM_ID,
    Data = case Request of
        {execute, StatementId} ->
            marina_request:execute(RequestId, FrameFlags, StatementId,
                QueryOpts);
        {prepare, Query} ->
            marina_request:prepare(RequestId, FrameFlags, Query);
        {query, Query} ->
            marina_request:query(RequestId, FrameFlags, Query, QueryOpts)
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
    case ?GET_ENV(compression, false) of
        true -> 1;
        _ -> 0
    end.

set_keyspace(_Socket, #state {keyspace = undefined} = State) ->
    {ok, State};
set_keyspace(Socket, #state {
        frame_flags = FrameFlags,
        keyspace = Keyspace
    } = State) ->

    Query = <<"USE \"", Keyspace/binary, "\"">>,
    Msg = marina_request:query(0, FrameFlags, Query, #{}),
    case marina_utils:sync_msg(Socket, Msg) of
        {ok, Keyspace} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
