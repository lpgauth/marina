-module(marina_backlog).
-include("marina.hrl").

-export([
    check/1,
    decrement/1,
    init/0,
    new/1
]).

-define(BACKLOG_MAX_SIZE, 1024).
-define(BACKLOG_TABLE_ID, marina_backlog).

%% public
-spec check(atom()) -> boolean().
check(ServerName) ->
    case increment(ServerName) of
        [?BACKLOG_MAX_SIZE, ?BACKLOG_MAX_SIZE] ->
            false;
        [_, Value] when Value =< ?BACKLOG_MAX_SIZE ->
            true;
        {error, tid_missing} ->
            false
    end.

-spec decrement(atom()) -> non_neg_integer() | {error, tid_missing}.
decrement(ServerName) ->
    safe_update_counter(ServerName, {2, -1, 0, 0}).

-spec init() -> ?BACKLOG_TABLE_ID.
init() ->
    ets:new(?BACKLOG_TABLE_ID, [
        named_table,
        public,
        {write_concurrency, true}
    ]).

-spec new(atom()) -> true.
new(ServerName) ->
    ets:insert(?BACKLOG_TABLE_ID, {ServerName, 0}).

%% private
increment(ServerName) ->
    safe_update_counter(ServerName, [{2, 0}, {2, 1, ?BACKLOG_MAX_SIZE, ?BACKLOG_MAX_SIZE}]).

safe_update_counter(ServerName, UpdateOp) ->
    try ets:update_counter(?BACKLOG_TABLE_ID, ServerName, UpdateOp)
    catch
        error:badarg ->
            {error, tid_missing}
    end.
