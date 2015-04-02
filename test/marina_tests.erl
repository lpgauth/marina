-module(marina_tests).
-include("test.hrl").

%% runner
marina_test() ->
    % setup
    setup_schema(),
    set_keyspace(),

    % tests
    test_query(),
    test_reusable_query().

%% tests
test_query() ->
    Response = query(<<"INSERT INTO users (key, column1, column2, value) values (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2', intAsBlob(0))">>),
    ?assertEqual({ok, undefined}, Response).

test_reusable_query() ->
    Response = reusable_query(<<"SELECT * FROM user LIMIT 1;">>, []),
    ?assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response),

    Response2 = reusable_query(<<"SELECT * FROM users LIMIT 1;">>, []),
    Response2 = reusable_query(<<"SELECT * FROM users LIMIT 1;">>, []),
    Response2 = reusable_query(<<"SELECT * FROM users WHERE key = ?;">>, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>], 1),

    ?assertEqual({ok,
        {result,
            {result_metadata, 4, [
                {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
            ]}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response2).

%% setup
setup_schema() ->
    application:start(marina),
    query(<<"DROP KEYSPACE test;">>),
    query(<<"CREATE KEYSPACE test WITH REPLICATION = {'class':'SimpleStrategy', 'replication_factor':1};">>),
    query(<<"CREATE TABLE test.users (key uuid, column1 text, column2 text, value blob, PRIMARY KEY (key, column1, column2));">>),
    application:stop(marina).

set_keyspace() ->
    application:load(marina),
    ok = application:set_env(?APP, keyspace, <<"test">>),
    application:start(marina).

%% helpers
query(Query) ->
    marina:query(Query, ?CONSISTENCY_ONE, ?DEFAULT_FLAGS, ?TEST_TIMEOUT).

reusable_query(Query, Values) ->
    reusable_query(Query, Values, ?TEST_TIMEOUT).

reusable_query(Query, Values, Flags) ->
    marina:reusable_query(Query, Values, ?CONSISTENCY_ONE, Flags, ?TEST_TIMEOUT).
