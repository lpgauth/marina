-module(marina).
-include("marina.hrl").

-export([
    query/0,
    query/1
]).

%% public
query() ->
    query(<<"SELECT * FROM \"RTB\".users LIMIT 1;">>).

query(Query) ->
    case call(Query, ?DEFAULT_TIMEOUT) of
        {ok, Frame} ->
            marina_body:decode(Frame);
        {error, Reason} ->
            {error, Reason}
    end.

%% private
-spec call(term(), pos_integer()) -> ok | {ok, term()} | {error, atom()}.
call(Msg, Timeout) ->
    case async_call(Msg, self()) of
        {ok, Ref} ->
            receive
                {?APP, Ref, Reply} ->
                    Reply
                after Timeout ->
                    {error, timeout}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec async_call(term(), pid()) -> {ok, erlang:ref()} | {error, backlog_full}.
async_call(Msg, Pid) ->
    Ref = make_ref(),
    Server = random_server(),
    case marina_backlog:check(Server) of
        true ->
            Server ! {call, Ref, Pid, Msg},
            {ok, Ref};
        _ ->
            {error, backlog_full}
    end.

random_server() ->
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    Random = erlang:phash2({os:timestamp(), self()}, PoolSize) + 1,
    list_to_atom("marina_server_" ++ integer_to_list(Random)).
