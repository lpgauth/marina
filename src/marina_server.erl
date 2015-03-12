-module(marina_server).
-include("marina.hrl").

-export([
    init/2,
    start_link/1
]).

-record(state, {
    buffer   = marina_buffer:new(),
    ip       = undefined,
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
        port = application:get_env(?APP, port, ?DEFAULT_PORT)
    }).

-spec start_link(atom()) -> {ok, pid()}.
start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [self(), Name]).

%% private
handle_msg({call, Ref, From, _Msg}, #state {
        socket = undefined,
        name = Name
    } = State) ->

    marina_backlog:decrement(Name),
    reply(Ref, From, {error, no_socket}),
    {ok, State};
handle_msg({call, Ref, From, Msg}, #state {
        socket = Socket,
        requests = Requests,
        name = Name
    } = State) ->

    Stream = Requests rem ?MAX_STREAM_ID,
    Query = marina_request:query(Stream, Msg),

    case gen_tcp:send(Socket, Query) of
        ok ->
            marina_queue:in(Name, Stream, {Ref, From}),
            {ok, State#state {
                requests = Requests + 1
            }};
        {error, Reason} ->
            marina_utils:warning_msg("tcp send error: ~p", [Reason]),
            gen_tcp:close(Socket),
            marina_backlog:decrement(Name),
            reply(Ref, From, {error, Reason}),

            {ok, State#state {
                socket = undefined
            }}
    end;
handle_msg(newsocket, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, false},
        {nodelay, true},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],

    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            case sync_msg(Socket, marina_request:startup()) of
                {ok, undefined} ->
                    % TODO: set default keyspace
                    inet:setopts(Socket, [{active, true}]),

                    {ok, State#state {
                        socket = Socket
                    }};
                {error, Reason} ->
                    marina_utils:warning_msg("startup error: ~p", [Reason]),
                    gen_tcp:close(Socket),
                    erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket),
                    {ok, State}
            end;
        {error, Reason} ->
            marina_utils:warning_msg("tcp connect error: ~p", [Reason]),
            erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket),
            {ok, State}
    end;
handle_msg({tcp, _Port, Msg}, #state {
        buffer = Buffer
    } = State) ->

    {Frames, Buffer2} = marina_buffer:decode(Msg, Buffer),
    reply_frames(Frames, State#state {
        buffer = Buffer2
    });
handle_msg(Msg, State) ->
    marina_utils:warning_msg("unexpected msg: ~p~n", [Msg]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

reply(Ref, From, Msg) ->
    From ! {?APP, Ref, Msg}.

reply_frames([], State) ->
    {ok, State};
reply_frames([#frame {stream = -1} | T], State) ->
    reply_frames(T, State);
reply_frames([#frame {stream = Stream} = Frame | T], #state {name = Name} = State) ->
    {Ref, From} = marina_queue:out(Name, Stream),
    marina_backlog:decrement(Name),
    reply(Ref, From, {ok, Frame}),
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
