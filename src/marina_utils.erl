-module(marina_utils).
-include("marina.hrl").

-export([
    get_env/2,
    pack/1,
    unpack/1
]).

%% public
-spec get_env(term(), term()) -> term().

get_env(Key, Default) ->
    application:get_env(?APP, Key, Default).

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
