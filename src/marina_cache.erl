-module(marina_cache).
-include("marina.hrl").

-export([
    get/1,
    init/0,
    put/2
]).

-define(CACHE_TABLE_ID, marina_cache).

%% public
-spec get(binary()) -> {ok, term()} | {error, not_found}.
get(Key) ->
    try
        Term = ets:lookup_element(?CACHE_TABLE_ID, Key, 2),
        {ok, Term}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec init() -> ?CACHE_TABLE_ID.
init() ->
    ets:new(?CACHE_TABLE_ID, [
        named_table,
        public,
        {read_concurrency, true}
    ]).

-spec put(binary(), term()) -> true.
put(Key, Value) ->
    ets:insert(?CACHE_TABLE_ID, {Key, Value}).
