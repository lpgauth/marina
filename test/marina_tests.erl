-module(marina_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("marina/include/marina.hrl").

-compile(export_all).

-define(TEST_KEYSPACE, <<"test">>).
-define(TEST_TIMEOUT, 10000).

%% runners
marina_test_() ->
    {setup,
        fun () -> set_env([{keyspace, ?TEST_KEYSPACE}]) end,
        fun (_) -> cleanup() end,
    {inparallel, [
        {"test_async_execute", ?MODULE, test_async_execute},
        {"test_async_prepare", ?MODULE, test_async_prepare},
        {"test_async_query", ?MODULE, test_async_query},
        {"test_async_reusable_query", ?MODULE, test_async_reusable_query},
        {"test_async_reusable_query_invalid_query", ?MODULE, test_async_reusable_query_invalid_query},
        {"test_execute", ?MODULE, test_execute},
        {"test_query", ?MODULE, test_query},
        {"test_query_no_metadata", ?MODULE, test_query_no_metadata},
        {"test_reusable_query", ?MODULE, test_reusable_query},
        {"test_reusable_query_invalid_query", ?MODULE, test_reusable_query_invalid_query},
        {"test_timeout_async", ?MODULE, test_timeout_async},
        {"test_timeout_sync", ?MODULE, test_timeout_sync}
    ]}}.

marina_compression_test_() ->
    {setup,
        fun () ->
            set_env([
                {compression, true},
                {keyspace, ?TEST_KEYSPACE}
            ])
        end,
        fun (_) -> cleanup() end,
    {inparallel, [
        {"test_async_query", ?MODULE, test_async_query},
        {"test_async_reusable_query", ?MODULE, test_async_reusable_query},
        {"test_query", ?MODULE, test_query},
        {"test_query_no_metadata", ?MODULE, test_query_no_metadata}
    ]}}.

marina_connection_error_test_() ->
    {setup,
        fun () ->
            set_env([
                {keyspace, ?TEST_KEYSPACE},
                {port, 9043}
            ])
        end,
        fun (_) -> cleanup() end,
    [{"test_no_socket", ?MODULE, test_no_socket}]}.

marina_backlog_test_() ->
    {setup,
        fun () ->
            set_env([
                {backlog_size, 1},
                {keyspace, ?TEST_KEYSPACE}
            ])
        end,
        fun (_) -> cleanup() end,
    {inparallel, [
        {"test_backlogfull_async", ?MODULE, test_backlogfull_async},
        {"test_backlogfull_sync", ?MODULE, test_backlogfull_sync}
    ]}}.

%% tests
test_async_execute() ->
    {ok, StatementId} = marina:prepare(<<"SELECT * FROM users LIMIT 1;">>, ?TEST_TIMEOUT),
    {ok, Ref} = marina:async_execute(StatementId, ?CONSISTENCY_ONE, [], self()),
    {X, _} = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?_assertEqual(ok, X).

test_async_prepare() ->
    {ok, Ref} = marina:async_prepare(<<"SELECT * FROM users LIMIT 1;">>, self()),
    {X, _} = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?_assertEqual(ok, X).

test_async_query() ->
    {ok, Ref} = async_query(<<"SELECT * FROM users LIMIT 1;">>),
    Response = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response).

test_async_reusable_query() ->
    Query = <<"SELECT * FROM users WHERE key = 99492dfe-d94a-11e4-af39-58f44110757d;">>,
    {ok, Ref} = marina:async_reusable_query(Query, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref, ?TEST_TIMEOUT),

    Query2 = <<"SELECT * FROM users WHERE key = ?;">>,
    Values = [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>],
    {ok, Ref2} = marina:async_reusable_query(Query2, Values, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref2, ?TEST_TIMEOUT),
    {ok, Ref3} = marina:async_reusable_query(Query2, Values, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref3, ?TEST_TIMEOUT),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response).

test_async_reusable_query_invalid_query() ->
    Query = <<"SELECT * FROM user WHERE key = ?;">>,
    Values = [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>],
    Response = marina:async_reusable_query(Query, Values, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),

    ?_assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response).

test_backlogfull_async() ->
    Responses = [async_query(<<"SELECT * FROM users LIMIT 1;">>) || _ <- lists:seq(1,100)],
    ?_assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, Responses)).

test_backlogfull_sync() ->
    Responses = [spawn(fun () -> query(<<"SELECT * FROM users LIMIT 1;">>) end) || _ <- lists:seq(1,20)],
    ?_assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, Responses)).

test_execute() ->
    {ok, StatementId} = marina:prepare(<<"SELECT * FROM users LIMIT 1;">>, ?TEST_TIMEOUT),
    Response = marina:execute(StatementId, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response).

test_query() ->
    Response = query(<<"SELECT * FROM users LIMIT 1;">>),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response).

test_query_no_metadata() ->
    Response2 = query(<<"SELECT * FROM users LIMIT 1;">>, [{skip_metadata, true}]),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, []}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response2).

test_no_socket() ->
    Response = query(<<"SELECT * FROM users LIMIT 1;">>),

    ?_assertEqual({error, no_socket}, Response).

test_reusable_query() ->
    Query = <<"SELECT * FROM users LIMIT 1;">>,
    Response = marina:reusable_query(Query, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    Response = marina:reusable_query(Query, [], ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    Query2 = <<"SELECT * FROM users WHERE key = ?;">>,
    Values = [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>],
    Response = marina:reusable_query(Query2, Values, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?_assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response).

test_reusable_query_invalid_query() ->
    Response = reusable_query(<<"SELECT * FROM user LIMIT 1;">>, []),

    ?_assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response).

test_timeout_async() ->
    {ok, Ref} = marina:async_query(<<"SELECT * FROM users LIMIT 1;">>, ?CONSISTENCY_ONE, [], self()),
    Response = marina:receive_response(Ref, 0),

    ?_assertEqual({error, timeout}, Response).

test_timeout_sync() ->
    Response = marina:query(<<"SELECT * FROM users LIMIT 1;">>, ?CONSISTENCY_ONE, [], 0),

    ?_assertEqual({error, timeout}, Response).

%% setup
cleanup() ->
    error_logger:tty(false),
    application:stop(marina),
    error_logger:tty(true).

setup_schema() ->
    marina_app:start(),
    query(<<"DROP KEYSPACE test;">>),
    query(<<"CREATE KEYSPACE test WITH REPLICATION = {'class':'SimpleStrategy', 'replication_factor':1};">>),
    query(<<"CREATE TABLE test.users (key uuid, column1 text, column2 text, value blob, PRIMARY KEY (key, column1, column2));">>),
    query(<<"INSERT INTO test.users (key, column1, column2, value) values (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2', intAsBlob(0))">>),
    application:stop(marina).

set_env(Vars) ->
    application:load(marina),
    [application:set_env(?APP, K, V) || {K, V} <- Vars],
    application:start(marina).

%% helpers
async_query(Query) ->
    marina:async_query(Query, ?CONSISTENCY_ONE, [], self()).

query(Query) ->
    query(Query, []).

query(Query, Flags) ->
    marina:query(Query, ?CONSISTENCY_ONE, Flags, ?TEST_TIMEOUT).

reusable_query(Query, Values) ->
    reusable_query(Query, Values, []).

reusable_query(Query, Values, Flags) ->
    marina:reusable_query(Query, Values, ?CONSISTENCY_ONE, Flags, ?TEST_TIMEOUT).
