-module(marina_utils).
-include("marina_internal.hrl").

-export([
    authenticate/1,
    connect/2,
    frame_flags/0,
    pack/1,
    query/2,
    query_opts/2,
    sync_msg/2,
    startup/1,
    timeout/2,
    unpack/1,
    use_keyspace/1
]).

%% public
-spec authenticate(inet:socket()) ->
    ok | {error, atom()}.

authenticate(Socket) ->
    Username = ?GET_ENV(username, undefined),
    Password = ?GET_ENV(password, undefined),
    authenticate(Username, Password, Socket).

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) ->
    {ok, inet:socket()} | {error, atom()}.

connect(Ip, Port) ->
    SocketOpts = ?DEFAULT_SOCKET_OPTIONS ++ [{active, false}],
    case gen_tcp:connect(Ip, Port, SocketOpts) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

-spec frame_flags() ->
    frame_flag().

frame_flags() ->
    case ?GET_ENV(compression, false) of
        true -> 1;
        _ -> 0
    end.

-spec pack(binary() | iolist()) ->
    {ok, binary()} | {error, term()}.

pack(Iolist) when is_list(Iolist) ->
    pack(iolist_to_binary(Iolist));
pack(Binary) ->
    case lz4:compress(Binary, []) of
        {ok, Compressed} ->
            {ok, <<(size(Binary)):32/unsigned-integer, Compressed/binary>>};
        {error, Reason} ->
            {error, Reason}
    end.

-spec query(inet:socket(), iodata()) ->
    {ok, term()} | {error, term()}.

query(Socket, Query) ->
    FrameFlags = frame_flags(),
    Msg = marina_request:query(0, FrameFlags, Query, #{skip_metadata => true}),
    sync_msg(Socket, Msg).

-spec query_opts(atom(), query_opts()) ->
    term().

query_opts(consistency_level, QueryOpts) ->
    maps:get(consistency_level, QueryOpts, ?DEFAULT_CONSISTENCY_LEVEL);
query_opts(flags, QueryOpts) ->
    maps:get(flags, QueryOpts, ?DEFAULT_FLAGS);
query_opts(page_size, QueryOpts) ->
    maps:get(page_size, QueryOpts, undefined);
query_opts(paging_state, QueryOpts) ->
    maps:get(paging_state, QueryOpts, undefined);
query_opts(pid, QueryOpts) ->
    maps:get(pid, QueryOpts, ?DEFAULT_PID);
query_opts(routing_key, QueryOpts) ->
    maps:get(routing_key, QueryOpts, ?DEFAULT_ROUTING_KEY);
query_opts(skip_metadata, QueryOpts) ->
    maps:get(skip_metadata, QueryOpts, false);
query_opts(timeout, QueryOpts) ->
    maps:get(timeout, QueryOpts, ?DEFAULT_TIMEOUT);
query_opts(values, QueryOpts) ->
    maps:get(values, QueryOpts, undefined).

-spec sync_msg(inet:socket(), iodata()) ->
    {ok, term()} | {error, term()}.

sync_msg(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            rcv_buf(Socket, <<>>);
        {error, Reason} ->
            {error, Reason}
    end.

-spec startup(inet:socket()) ->
    {ok, binary() | undefined} | {error, atom()}.

startup(Socket) ->
    FrameFlags = frame_flags(),
    Msg = marina_request:startup(FrameFlags),
    case marina_utils:sync_msg(Socket, Msg) of
        {ok, Response} ->
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end.

-spec unpack(binary()) ->
    {ok, binary()} | {error, term()}.

unpack(<<Size:32/unsigned-integer, Binary/binary>>) ->
    lz4:uncompress(Binary, Size).

-spec timeout(pos_integer(), erlang:timestamp()) ->
    integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.

-spec use_keyspace(inet:socket()) ->
    ok | {error, atom()}.

use_keyspace(Socket) ->
    Keyspace = ?GET_ENV(keyspace, undefined),
    use_keyspace(Keyspace, Socket).

%% private
authenticate(undefined, undefined, _Socket) ->
    ok;
authenticate(Username, Password, Socket) when is_binary(Username);
    is_binary(Username) ->

    FrameFlags = frame_flags(),
    Msg = marina_request:auth_response(FrameFlags, Username, Password),

    case marina_utils:sync_msg(Socket, Msg) of
        {ok, undefined} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

rcv_buf(Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0, ?DEFAULT_RECV_TIMEOUT) of
        {ok, Msg} ->
            Buffer2 = <<Buffer/binary, Msg/binary>>,
            case marina_frame:decode(Buffer2) of
                {_Rest, []} ->
                    rcv_buf(Socket, Buffer2);
                {_Rest, [Frame | _]} ->
                    marina_body:decode(Frame)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

use_keyspace(undefined, _Socket) ->
    ok;
use_keyspace(Keyspace, Socket) when is_binary(Keyspace)->
    FrameFlags = frame_flags(),
    Query = <<"USE \"", Keyspace/binary, "\"">>,
    Msg = marina_request:query(0, FrameFlags, Query, #{}),

    case marina_utils:sync_msg(Socket, Msg) of
        {ok, Keyspace} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
