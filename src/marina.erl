-module(marina).
-include("marina_internal.hrl").

-export([
    async_query/5,
    async_query/6,
    async_reusable_query/5,
    async_reusable_query/6,
    query/5,
    receive_response/1,
    response/1,
    reusable_query/5
]).

%% public
-spec async_query(query(), [value()], cl(), [flag()], pid()) ->
    {ok, reference()} | error().

async_query(Query, Values, CL, Flags, Pid) ->
    async_query(Query, Values, CL, Flags, Pid, ?DEFAULT_TIMEOUT).

-spec async_query(query(), [value()], cl(), [flag()], pid(), timeout()) ->
    {ok, reference()} | error().

async_query(Query, Values, CL, Flags, Pid, Timeout) ->
    async_call({query, Query, Values, CL, Flags}, Pid, Timeout).

-spec async_reusable_query(query(), [value()], cl(), [flag()], pid()) ->
    {ok, reference()} | error().

async_reusable_query(Query, Values, CL, Flags, Pid) ->
    async_reusable_query(Query, Values, CL, Flags, Pid, ?DEFAULT_TIMEOUT).

-spec async_reusable_query(query(), [value()], cl(), [flag()], pid(),
    timeout()) -> {ok, reference()} | error().

async_reusable_query(Query, Values, CL, Flags, Pid, Timeout) ->
    case marina_pool:random() of
        {ok, Pool} ->
            async_reusable_query(Pool, Query, Values, CL, Flags, Pid, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

-spec query(query(), [value()], cl(), [flag()], timeout()) ->
    {ok, term()} | error().

query(Query, Values, CL, Flags, Timeout) ->
    call({query, Query, Values, CL, Flags}, Timeout).

-spec receive_response(term()) ->
    {ok, term()} | error().

receive_response(RequestId) ->
    response(shackle:receive_response(RequestId)).

-spec response({ok, term()} | error()) ->
    {ok, term()} | error().

response({ok, Frame}) ->
    marina_body:decode(Frame);
response({error, Reason}) ->
    {error, Reason}.

-spec reusable_query(query(), [value()], cl(), [flag()], timeout()) ->
    {ok, term()} | error().

reusable_query(Query, Values, CL, Flags, Timeout) ->
    case marina_pool:random() of
        {ok, Pool} ->
            reusable_query(Pool, Query, Values, CL, Flags, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

%% private
async_call(Msg, Pid, Timeout) ->
    case marina_pool:random() of
        {ok, Pool} ->
            async_call(Pool, Msg, Pid, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

async_call(Pool, Msg, Pid, Timeout) ->
    shackle:cast(Pool, Msg, Pid, Timeout).

async_reusable_query(Pool, Query, Values, CL, Flags, Pid, Timeout) ->
    case marina_cache:get(Pool, Query) of
        {ok, StatementId} ->
            async_call(Pool, {execute, StatementId, Values, CL, Flags},
                Pid, Timeout);
        {error, not_found} ->
            case call(Pool, {prepare, Query}, Timeout) of
                {ok, StatementId} ->
                    marina_cache:put(Pool, Query, StatementId),
                    async_call(Pool, {execute, StatementId, Values, CL, Flags},
                        Pid, Timeout);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

call(Msg, Timeout) ->
    case marina_pool:random() of
        {ok, Pool} ->
            call(Pool, Msg, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

call(Pool, Msg, Timeout) ->
    response(shackle:call(Pool, Msg, Timeout)).

reusable_query(Pool, Query, Values, CL, Flags, Timeout) ->
    Timestamp = os:timestamp(),
    case marina_cache:get(Pool, Query) of
        {ok, StatementId} ->
            Execute = call(Pool, {execute, StatementId, Values, CL, Flags},
                Timeout),
            case Execute of
                {error, {9472, _}} ->
                    marina_cache:erase(Pool, Query),
                    Timeout3 = marina_utils:timeout(Timeout, Timestamp),
                    reusable_query(Pool, Query, Values, CL, Flags, Timeout3);
                Response ->
                    Response
            end;
        {error, not_found} ->
            case call(Pool, {prepare, Query}, Timeout) of
                {ok, StatementId} ->
                    marina_cache:put(Pool, Query, StatementId),
                    Timeout2 = marina_utils:timeout(Timeout, Timestamp),
                    call(Pool, {execute, StatementId, Values, CL, Flags},
                        Timeout2);
                {error, Reason} ->
                    {error, Reason}
            end
    end.
