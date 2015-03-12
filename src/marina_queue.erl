-module(marina_queue).

-export([
    empty/1,
    in/3,
    init/0,
    out/2
]).

-define(QUEUE_TABLE_ID, marina_queue).

%% public
-spec empty(atom()) -> [term()].
empty(ServerName) ->
    Items = ets:match_object(?QUEUE_TABLE_ID, {{ServerName, '_'}, '_'}),
    ets:match_delete(?QUEUE_TABLE_ID, {{ServerName, '_'}, '_'}),
    Items.

-spec init() -> ?QUEUE_TABLE_ID.
init() ->
    ets:new(?QUEUE_TABLE_ID, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec in(atom(), non_neg_integer(), term()) -> true.
in(ServerName, Stream, Item) ->
    ets:insert(?QUEUE_TABLE_ID, {{ServerName, Stream}, Item}).

-spec out(atom(), non_neg_integer()) -> term().
out(ServerName, Stream) ->
    Item = ets:lookup_element(?QUEUE_TABLE_ID, {ServerName, Stream}, 2),
    ets:delete(?QUEUE_TABLE_ID, {ServerName, Stream}),
    Item.

