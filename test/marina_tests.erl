-module(marina_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("marina/include/marina.hrl").

-define(TEST_KEYSPACE, <<"test">>).
-define(TEST_TIMEOUT, 10000).

%% public
marina_test_() ->
    setup_schema(),
    set_env([{keyspace, ?TEST_KEYSPACE}]),

    {inparallel, [
        test_async_prepare(),
        test_async_query(),
        test_async_reusable_query(),
        test_async_reusable_query_invalid_query(),
        test_query(),
        test_query_no_metadata(),
        test_reusable_query(),
        test_reusable_query_invalid_query(),
        test_timeout()
    ]}.

marina_compression_test_() ->
    setup_schema(),
    set_env([
        {keyspace, <<"test">>},
        {compression, true}
    ]),

    test_query().

marina_connection_error_test_() ->
    setup_schema(),
    set_env([
        {keyspace, <<"test">>},
        {port, 9043}
    ]),

    test_no_socket().

marina_backlog_test_() ->
    setup_schema(),
    set_env([
        {keyspace, <<"test">>},
        {backlog_size, 1}
    ]),

    Responses = [async_query(<<"SELECT * FROM users LIMIT 1;">>) || _ <- lists:seq(1,100)],
    BacklogFull = lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, Responses),

    ?_assert(BacklogFull).

%% tests
test_async_prepare() ->
    {ok, Ref} = async_prepare(<<"SELECT * FROM users LIMIT 1;">>),
    {X, _} = receive_response(Ref),

    ?_assertEqual(ok, X).

test_async_query() ->
    {ok, Ref} = async_query(<<"SELECT * FROM users LIMIT 1;">>),
    Response = receive_response(Ref),

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
    {ok, Ref} = async_reusable_query(<<"SELECT * FROM users WHERE key = ?;">>, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]),
    Response = receive_response(Ref),

    {ok, Ref2} = async_reusable_query(<<"SELECT * FROM users WHERE key = ?;">>, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]),
    Response = receive_response(Ref2),

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
    Response = async_reusable_query(<<"SELECT * FROM user WHERE key = ?;">>, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]),

    ?_assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response).

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
    Response = reusable_query(<<"SELECT * FROM users LIMIT 1;">>, []),
    Response = reusable_query(<<"SELECT * FROM users LIMIT 1;">>, []),
    Response = reusable_query(<<"SELECT * FROM users WHERE key = ?;">>, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]),

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

test_timeout() ->
    Response = marina:query(<<"SELECT * FROM users LIMIT 1;">>, ?CONSISTENCY_ONE, [], 0),

    ?_assertEqual({error, timeout}, Response).

%% setup
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
async_prepare(Query) ->
    marina:async_prepare(Query, self()).

async_query(Query) ->
    marina:async_query(Query, ?CONSISTENCY_ONE, [], self()).

async_reusable_query(Query, Values) ->
    marina:async_reusable_query(Query, Values, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT).

query(Query) ->
    query(Query, []).

query(Query, Flags) ->
    marina:query(Query, ?CONSISTENCY_ONE, Flags, ?TEST_TIMEOUT).

receive_response(Ref) ->
    receive
        {?APP, Ref, Reply} ->
            marina:response(Reply)
    after ?TEST_TIMEOUT ->
        {error, timeout}
    end.

reusable_query(Query, Values) ->
    reusable_query(Query, Values, []).

reusable_query(Query, Values, Flags) ->
    marina:reusable_query(Query, Values, ?CONSISTENCY_ONE, Flags, ?TEST_TIMEOUT).
