-module(marina_utils).
-include("marina_internal.hrl").

-export([
    pack/1,
    unpack/1,
    timeout/2
]).

%% public
-spec pack(binary() | iolist()) -> {ok, binary()} | {error, term()}.

pack(Iolist) when is_list(Iolist) ->
    pack(iolist_to_binary(Iolist));
pack(Binary) ->
    case lz4:compress(Binary, []) of
        {ok, Compressed} ->
            {ok, <<(size(Binary)):32/unsigned-integer, Compressed/binary>>};
        {error, Reason} ->
            {error, Reason}
    end.

-spec unpack(binary()) -> {ok, binary()} | {error, term()}.

unpack(<<Size:32/unsigned-integer, Binary/binary>>) ->
    lz4:uncompress(Binary, Size).

-spec timeout(pos_integer(), erlang:timestamp()) ->
    integer().

timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    Timeout - Diff.
