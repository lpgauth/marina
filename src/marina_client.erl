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
    requests    = 0                   :: non_neg_integer()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init() ->
    {ok, state()}.

init() ->
    {ok, #state {
        frame_flags = marina_utils:frame_flags()
    }}.

-spec setup(inet:socket(), state()) ->
    {ok, state()} |
    {error, atom(), state()}.

setup(Socket, State) ->
    case marina_utils:startup(Socket) of
        {ok, undefined} ->
            case marina_utils:use_keyspace(Socket) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;
        {ok, <<"org.apache.cassandra.auth.PasswordAuthenticator">>} ->
            case marina_utils:authenticate(Socket) of
                ok ->
                    case marina_utils:use_keyspace(Socket) of
                        ok ->
                            {ok, State};
                        {error, Reason} ->
                            {error, Reason, State}
                    end;
                {error, Reason} ->
                    {error, Reason, State}
            end;
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

-spec terminate(state()) ->
    ok.

terminate(_State) ->
    ok.
