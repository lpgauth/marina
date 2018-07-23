-module(marina).
-include("marina_internal.hrl").

-export([
    async_query/2,
    async_reusable_query/2,
    query/2,
    receive_response/1,
    response/1,
    reusable_query/2
]).

%% public
-spec async_query(query(), query_opts()) ->
    {ok, shackle:request_id()} | error().

async_query(Query, QueryOpts) ->
    async_call({query, Query}, QueryOpts).

-spec async_reusable_query(query(), query_opts()) ->
    {ok, shackle:request_id()} | error().

async_reusable_query(Query, QueryOpts) ->
    RoutingKey = marina_utils:query_opts(routing_key, QueryOpts),
    case marina_pool:node(RoutingKey) of
        {ok, Pool} ->
            async_reusable_query(Pool, Query, QueryOpts);
        {error, Reason} ->
            {error, Reason}
    end.

-spec query(query(), query_opts()) ->
    {ok, term()} | error().

query(Query, QueryOpts) ->
    call({query, Query}, QueryOpts).

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

-spec reusable_query(query(), query_opts()) ->
    {ok, term()} | error().

reusable_query(Query, QueryOpts) ->
    RoutingKey = marina_utils:query_opts(routing_key, QueryOpts),
    case marina_pool:node(RoutingKey) of
        {ok, Pool} ->
            reusable_query(Pool, Query, QueryOpts);
        {error, Reason} ->
            {error, Reason}
    end.

%% private
async_call(Msg, QueryOpts) ->
    RoutingKey = marina_utils:query_opts(routing_key, QueryOpts),
    case marina_pool:node(RoutingKey) of
        {ok, Pool} ->
            async_call(Pool, Msg, QueryOpts);
        {error, Reason} ->
            {error, Reason}
    end.


async_call(Pool, Msg, QueryOpts) ->
    Pid = marina_utils:query_opts(pid, QueryOpts),
    Timeout = marina_utils:query_opts(timeout, QueryOpts),
    shackle:cast(Pool, {Msg, QueryOpts}, Pid, Timeout).

async_reusable_query(Pool, Query, QueryOpts) ->
    case marina_cache:get(Pool, Query) of
        {ok, StatementId} ->
            async_call(Pool, {execute, StatementId}, QueryOpts);
        {error, not_found} ->
            case call(Pool, {prepare, Query}, QueryOpts) of
                {ok, StatementId} ->
                    marina_cache:put(Pool, Query, StatementId),
                    async_call(Pool, {execute, StatementId}, QueryOpts);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

call(Msg, QueryOpts) ->
    RoutingKey = marina_utils:query_opts(routing_key, QueryOpts),
    case marina_pool:node(RoutingKey) of
        {ok, Pool} ->
            call(Pool, Msg, QueryOpts);
        {error, Reason} ->
            {error, Reason}
    end.

call(Pool, Msg, QueryOpts) ->
    Timeout = marina_utils:query_opts(timeout, QueryOpts),
    response(shackle:call(Pool, {Msg, QueryOpts}, Timeout)).

reusable_query(Pool, Query, QueryOpts) ->
    Timeout = marina_utils:query_opts(timeout, QueryOpts),
    Timestamp = os:timestamp(),
    case marina_cache:get(Pool, Query) of
        {ok, StatementId} ->
            Execute = call(Pool, {execute, StatementId}, QueryOpts),
            case Execute of
                {error, {9472, _}} ->
                    marina_cache:erase(Pool, Query),
                    Timeout2 = marina_utils:timeout(Timeout, Timestamp),
                    reusable_query(Pool, Query,
                        QueryOpts#{timeout => Timeout2});
                Response ->
                    Response
            end;
        {error, not_found} ->
            case call(Pool, {prepare, Query}, QueryOpts) of
                {ok, StatementId} ->
                    marina_cache:put(Pool, Query, StatementId),
                    Timeout2 = marina_utils:timeout(Timeout, Timestamp),
                    call(Pool, {execute, StatementId},
                        QueryOpts#{timeout => Timeout2});
                {error, Reason} ->
                    {error, Reason}
            end
    end.
