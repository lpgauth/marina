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
    case marina_backlog:check(marina_server_1) of
        true ->
            marina_server_1 ! {call, Ref, Pid, Msg},
            {ok, Ref};
        _ ->
            {error, backlog_full}
    end.