-module(marina_utils).
-include("marina.hrl").

-export([
    child_name/1,
    child_specs/0,
    info_msg/2,
    lookup/3,
    pack/1,
    timeout/2,
    unpack/1,
    warning_msg/2
]).

%% public
-spec child_name(integer()) -> atom().

child_name(N) ->
    list_to_atom(?SERVER_BASE_NAME ++ integer_to_list(N)).

-spec child_specs() -> [supervisor:child_spec()].

child_specs() ->
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    [child_spec(N) || N <- lists:seq(1, PoolSize)].

-spec info_msg(string(), [term()]) -> ok.

-ifdef(TEST).
info_msg(_Format, _Data) -> ok.
-else.
info_msg(Format, Data) ->
    error_logger:info_msg(Format, Data).
-endif.

-spec lookup(term(), [{term(), term()}], term()) -> term().

lookup(Key, Values, Default) ->
    case lists:keyfind(Key, 1, Values) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec pack(binary() | iolist()) -> {ok, binary()} | {error, term()}.

pack(Iolist) when is_list(Iolist) ->
    pack(iolist_to_binary(Iolist));
pack(Binary) ->
    case lz4:compress(Binary, []) of
        {ok, Compressed} ->
            {ok, <<(size(Binary)):32/unsigned-integer, Compressed/binary>>};
        Error ->
            Error
    end.

-spec timeout(erlang:timestamp(), non_neg_integer()) -> non_neg_integer().

timeout(Timestamp, Timeout) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    case Timeout - Diff of
        X when X < 0 -> 0;
        X -> X
    end.

-spec unpack(binary()) -> {ok, binary()} | {error, term()}.

unpack(<<Size:32/unsigned-integer, Binary/binary>>) ->
    lz4:uncompress(Binary, Size).

-spec warning_msg(string(), [term()]) -> ok.

-ifdef(TEST).
warning_msg(_Format, _Data) -> ok.
-else.
warning_msg(Format, Data) ->
    error_logger:warning_msg(Format, Data).
-endif.

%% private
child_spec(N) ->
    Name = child_name(N),
    Module = marina_server,
    StartFunc = {Module, start_link, [Name]},
    {Name, StartFunc, permanent, 5000, worker, [Module]}.
