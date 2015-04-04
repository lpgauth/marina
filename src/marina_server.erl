-module(marina_server).
-include("marina.hrl").

-export([
    init/2,
    start_link/1
]).

-record(state, {
    buffer   = marina_buffer:new(),
    ip       = undefined,
    keyspace = undefined,
    name     = undefined,
    port     = undefined,
    requests = 0,
    socket   = undefined
}).

%% public
-spec init(pid(), atom()) -> no_return().

init(Parent, Name) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    marina_backlog:new(Name),
    self() ! newsocket,

    loop(#state {
        name = Name,
        ip = application:get_env(?APP, ip, ?DEFAULT_IP),
        port = application:get_env(?APP, port, ?DEFAULT_PORT),
        keyspace = application:get_env(?APP, keyspace, undefined)
    }).

-spec start_link(atom()) -> {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
handle_msg({call, Ref, From, _Msg}, #state {
        socket = undefined,
        name = Name
    } = State) ->

    reply(Name, Ref, From, {error, no_socket}),
    {ok, State};
handle_msg({call, Ref, From, Call}, #state {
        socket = Socket,
        requests = Requests,
        name = Name
    } = State) ->

    Stream = Requests rem ?MAX_STREAM_ID,

    Msg = case Call of
        {execute, StatementId, Values, ConsistencyLevel, Flags} ->
            marina_request:execute(Stream, StatementId, Values, ConsistencyLevel, Flags);
        {prepare, Query} ->
            marina_request:prepare(Stream, Query);
        {query, Query, ConsistencyLevel, Flags} ->
            marina_request:query(Stream, Query, ConsistencyLevel, Flags)
    end,

    case gen_tcp:send(Socket, Msg) of
        ok ->
            marina_queue:in(Name, Stream, {Ref, From}),
            {ok, State#state {
                requests = Requests + 1
            }};
        {error, Reason} ->
            marina_utils:warning_msg("tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            tcp_close(State)
    end;
handle_msg(newsocket, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, false},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],

    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            Msg = marina_request:startup(),
            case sync_msg(Socket, Msg) of
                {ok, undefined} ->
                    default_keyspace(State#state {
                        socket = Socket
                    });
                {error, Reason} ->
                    marina_utils:warning_msg("startup error: ~p", [Reason]),
                    gen_tcp:close(Socket),
                    reconnect_timer(),
                    {ok, State}
            end;
        {error, Reason} ->
            marina_utils:warning_msg("tcp connect error: ~p", [Reason]),
            reconnect_timer(),
            {ok, State}
    end;
handle_msg({tcp, _Port, Msg}, #state {
        buffer = Buffer
    } = State) ->

    {Frames, Buffer2} = marina_buffer:decode(Msg, Buffer),
    reply_frames(Frames, State#state {
        buffer = Buffer2
    });
handle_msg({tcp_closed, Socket}, #state {
        socket = Socket
    } = State) ->

    marina_utils:warning_msg("tcp closed", []),
    tcp_close(State);
handle_msg({tcp_error, Socket, Reason}, #state {
        socket = Socket
    } = State) ->

    marina_utils:warning_msg("tcp error: ~p", [Reason]),
    gen_tcp:close(Socket),
    tcp_close(State).

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

reconnect_timer() ->
    erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket).

reply(Name, Ref, From, Msg) ->
    marina_backlog:decrement(Name),
    From ! {?APP, Ref, Msg}.

reply_frames([], State) ->
    {ok, State};
reply_frames([#frame {stream = -1} | T], State) ->
    reply_frames(T, State);
reply_frames([#frame {stream = Stream} = Frame | T], #state {name = Name} = State) ->
    {Ref, From} = marina_queue:out(Name, Stream),
    reply(Name, Ref, From, {ok, Frame}),
    reply_frames(T, State).

sync_msg(Socket, Msg) ->
    gen_tcp:send(Socket, Msg),
    case gen_tcp:recv(Socket, 0, ?DEFAULT_RECV_TIMEOUT) of
        {ok, Msg2} ->
            {_Rest, [Frame | _]} = marina_frame:decode(Msg2),
            marina_body:decode(Frame);
        {error, Reason} ->
            {error, Reason}
    end.

tcp_close(#state {name = Name} = State) ->
    Msg = {error, tcp_close},
    Items = marina_queue:empty(Name),
    [reply(Name, Ref, From, Msg) || {_, {Ref, From}} <- Items],
    reconnect_timer(),

    {ok, State#state {
        socket = undefined
    }}.

default_keyspace(#state {
        keyspace = undefined,
        socket = Socket
    } = State) ->

    inet:setopts(Socket, [{active, true}]),
    {ok, State};
default_keyspace(#state {
        socket = Socket,
        keyspace = Keyspace
    } = State) ->

    Query = <<"USE \"", Keyspace/binary, "\"">>,
    Msg = marina_request:query(0, Query, ?CONSISTENCY_ONE, ?DEFAULT_FLAGS),
    case sync_msg(Socket, Msg) of
        {ok, Keyspace} ->
            inet:setopts(Socket, [{active, true}]),
            {ok, State};
        {error, Reason} ->
            marina_utils:warning_msg("query error: ~p", [Reason]),
            gen_tcp:close(Socket),
            reconnect_timer(),

            {ok, State#state {
                socket = undefined
            }}
    end.
