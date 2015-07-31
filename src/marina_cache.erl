-module(marina_cache).
-include("marina.hrl").

-export([
    erase/1,
    get/1,
    init/0,
    put/2
]).

%% public
-spec erase(binary()) -> ok | {error, not_found}.

erase(Key) ->
    try
        ets:delete(?ETS_TABLE_CACHE, Key),
        ok
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec get(binary()) -> {ok, term()} | {error, not_found}.

get(Key) ->
    try
        Term = ets:lookup_element(?ETS_TABLE_CACHE, Key, 2),
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

-spec put(binary(), term()) -> ok.

put(Key, Value) ->
    ets:insert(?ETS_TABLE_CACHE, {Key, Value}),
    ok.
