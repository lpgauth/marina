-module(marina_server).
-include("marina.hrl").

-export([
    call/2,
    init/1,
    start_link/0
]).

-record(state, {
    ip          = undefined,
    port        = undefined,
    socket      = undefined
}).

%% public
% TODO: spec
call(Msg, Timeout) ->
    Ref = make_ref(),
    Pid = self(),
    ?MODULE ! {call, Ref, Pid, Msg},
    receive
        {?APP, Ref, Reply} ->
            Reply
        after Timeout ->
            {error, timeout}
    end.

-spec init(pid()) -> no_return().
init(Parent) ->
    register(?MODULE, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    self() ! newsocket,

    loop(#state {
        ip = application:get_env(?APP, ip, ?DEFAULT_IP),
        port = application:get_env(?APP, port, ?DEFAULT_PORT)
    }).

-spec start_link() -> {ok, pid()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%% private
loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

handle_msg({call, Ref, From, _Msg}, #state {
        socket = undefined
    } = State) ->

    reply(Ref, From, {error, no_socket}),
    {ok, State};
handle_msg(newsocket, #state {
        ip = Ip,
        port = Port
    } = State) ->

    Opts = [
        binary,
        {active, once},
        {nodelay, true},
        {packet, raw},
        {send_timeout, ?DEFAULT_SEND_TIMEOUT},
        {send_timeout_close, true}
    ],
    case gen_tcp:connect(Ip, Port, Opts) of
        {ok, Socket} ->
            ReadyFrame = marina_frame:encode(#frame {
                flags = 0,
                stream = 0,
                opcode = ?OP_STARTUP,
                body = marina_types:encode_string_map([{<<"CQL_VERSION">>, ?CQL_VERSION}])
            }),
            gen_tcp:send(Socket, ReadyFrame),

            {ok, State#state {
                socket = Socket
            }};
        {error, Reason} ->
            error_logger:error_msg("tcp connect error: ~p", [Reason]),
            erlang:send_after(?DEFAULT_RECONNECT, self(), newsocket),
            {ok, State}
    end;
handle_msg({tcp, _Port, Msg}, State) ->
    io:format("tcp: ~p~n", [marina_frame:decode(Msg)]),
    {ok, State};
handle_msg(Msg, State) ->
    io:format("unexpected msg: ~p~n", [Msg]),
    {ok, State}.

reply(Ref, From, Msg) ->
    From ! {?APP, Ref, Msg}.
