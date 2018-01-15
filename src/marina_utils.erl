-module(marina_utils).
-include("marina_internal.hrl").

-export([
    connect/2,
    ip_to_bin/1,
    pack/1,
    sync_msg/2,
    unpack/1,
    timeout/2,
    uuid_to_string/1
]).

%% public
-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) ->
    {ok, inet:socket()} | {error, atom()}.

connect(Ip, Port) ->
    SocketOpts = ?DEFAULT_SOCKET_OPTIONS ++ [{active, false}],
    case gen_tcp:connect(Ip, Port, SocketOpts) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            error_logger:error_msg("failed to connect: ~p~n", [Reason]),
            {error, Reason}
    end.

-spec ip_to_bin(string()) ->
    binary().

ip_to_bin(Ip) ->
    {ok, {A, B, C, D}} = inet:parse_ipv4_address(Ip),
    <<A/integer, B/integer, C/integer, D/integer>>.

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

-spec sync_msg(inet:socket(), iodata()) ->
    {ok, term()} | {error, term()}.

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

-spec unpack(binary()) ->
    {ok, binary()} | {error, term()}.

unpack(<<Size:32/unsigned-integer, Binary/binary>>) ->
    lz4:uncompress(Binary, Size).

-spec timeout(pos_integer(), erlang:timestamp()) ->
    integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.

-spec uuid_to_string(<<_:128>>) ->
    list().

uuid_to_string(<<Value:128>>) ->
    [N01, N02, N03, N04, N05, N06, N07, N08, N09, N10, N11, N12, N13, N14, N15,
     N16, N17, N18, N19, N20, N21, N22, N23, N24, N25, N26, N27, N28, N29, N30,
     N31, N32] = int_to_hex_list(Value, 32),

    [N01, N02, N03, N04, N05, N06, N07, N08, $-, N09, N10, N11, N12, $-, N13,
     N14, N15, N16, $-, N17, N18, N19, N20, $-, N21, N22, N23, N24, N25, N26,
     N27, N28, N29, N30, N31, N32].

%% private
int_to_hex(I) when 0 =< I, I =< 9 ->
    I + $0;
int_to_hex(I) when 10 =< I, I =< 15 ->
    (I - 10) + $a.

int_to_hex_list(I, N) when is_integer(I), I >= 0 ->
    int_to_hex_list([], I, 1, N).

int_to_hex_list(L, I, Count, N) when I < 16 ->
    int_to_hex_list_pad([int_to_hex(I) | L], N - Count);
int_to_hex_list(L, I, Count, N) ->
    int_to_hex_list([int_to_hex(I rem 16) | L], I div 16, Count + 1, N).

int_to_hex_list_pad(L, 0) ->
    L;
int_to_hex_list_pad(L, Count) ->
    int_to_hex_list_pad([$0 | L], Count - 1).
