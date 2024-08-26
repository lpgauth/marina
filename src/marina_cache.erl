-module(marina_cache).
-include("marina_internal.hrl").

-export([
    erase/2,
    erase_server/1,
    get/2,
    init/0,
    put/3
]).

%% public
-spec erase(atom(), binary()) -> ok | {error, not_found}.

erase(Pool, Key) ->
    try
        ets:delete(?ETS_TABLE_CACHE, {Pool, Key}),
        ok
    catch
        error:badarg ->
            {error, not_found}
    end.


-spec erase_server(shackle:request_id()) -> ok.

erase_server({ServerName, _}) ->
    Pool = marina_utils:server_to_pool(ServerName),
    ets:select_delete(?ETS_TABLE_CACHE, [{{{'$1', '$2'}, '_'}, [], [{'==', '$1', Pool}]}]),
    ok.

-spec get(atom(), binary()) -> {ok, term()} | {error, not_found}.

get(Pool, Key) ->
    try
        Term = ets:lookup_element(?ETS_TABLE_CACHE, {Pool, Key}, 2),
        {ok, Term}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec init() -> ?ETS_TABLE_CACHE.

init() ->
    ets:new(?ETS_TABLE_CACHE, [
        named_table,
        public,
        {read_concurrency, true}
    ]).

-spec put(atom(), binary(), term()) -> ok.

put(Pool, Key, Value) ->
    ets:insert(?ETS_TABLE_CACHE, {{Pool, Key}, Value}),
    ok.
