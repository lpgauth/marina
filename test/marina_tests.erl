-module(marina_tests).
-include("test.hrl").

%% runners
marina_test_() ->
    {setup,
        fun () -> setup([
            {keyspace, <<"test">>},
            {pool_size, 1}
        ]) end,
        fun (_) -> cleanup() end,
    {inparallel, [
        fun async_query_subtest/0,
        fun async_reusable_query_subtest/0,
        fun async_reusable_query_invalid_query_subtest/0,
        fun counters_subtest/0,
        fun paging_subtest/0,
        fun query_subtest/0,
        fun query_metedata_types_subtest/0,
        fun query_no_metadata_subtest/0,
        fun reusable_query_subtest/0,
        fun reusable_query_invalid_query_subtest/0,
        fun schema_changes_subtest/0,
        fun tuples_subtest/0
    ]}}.

marina_compression_test_() ->
    {setup,
        fun () -> setup([
            {compression, true},
            {keyspace, <<"test">>}
        ]) end,
        fun (_) -> cleanup() end,
        [fun query_subtest/0]
    }.

%% tests
async_query_subtest() ->
    {ok, Ref} = marina:async_query(?QUERY1, #{}),
    Response = marina:receive_response(Ref),

    ?assertEqual(?QUERY1_RESULT, Response).

async_reusable_query_subtest() ->
    {ok, Ref} = marina:async_reusable_query(?QUERY3, #{}),
    Response = marina:receive_response(Ref),
    {ok, Ref2} = marina:async_reusable_query(?QUERY2,
        #{values => ?QUERY2_VALUES}),
    Response = marina:receive_response(Ref2),
    {ok, Ref3} = marina:async_reusable_query(?QUERY2,
        #{values => ?QUERY2_VALUES}),
    Response = marina:receive_response(Ref3),

    ?assertEqual(?QUERY1_RESULT, Response).

async_reusable_query_invalid_query_subtest() ->
    Query = <<"SELECT * FROM user LIMIT 1;">>,
    {error, {8704, _}} = marina:async_reusable_query(Query, #{}).

counters_subtest() ->
    query(<<"DROP TABLE test.page_view_counts;">>),
    query(<<"CREATE TABLE test.page_view_counts (counter_value counter,
        url_name varchar, page_name varchar,
        PRIMARY KEY (url_name, page_name));">>),
    query(<<"UPDATE test.page_view_counts SET counter_value = counter_value + 1
        WHERE url_name='adgear.com' AND page_name='home';">>),
    Response = query(<<"SELECT * FROM test.page_view_counts">>),

    ?assertEqual({ok, {result,
        {result_metadata, 3,
            [{column_spec, <<"test">>, <<"page_view_counts">>, <<"url_name">>,
                varchar},
             {column_spec, <<"test">>, <<"page_view_counts">>, <<"page_name">>,
                 varchar},
             {column_spec, <<"test">>, <<"page_view_counts">>,
                 <<"counter_value">>, counter}],
            undefined},
        1,
        [[<<"adgear.com">>, <<"home">>, <<0, 0, 0, 0, 0, 0, 0, 1>>]]
    }}, Response).

paging_subtest() ->
    query(<<"INSERT INTO test.users (key, column1, column2, value) values
        (99492dfe-d94a-11e4-af39-58f44110757e, 'test', 'test2',
        intAsBlob(0));">>),
    Query = <<"SELECT * FROM users LIMIT 10;">>,
    {ok, {result, Metadata, 1, Rows}} = marina:query(Query,
        #{page_size => 1}),
    {result_metadata, 4, _, PagingState} = Metadata,

    {ok, {result, Metadata2, 1, Rows2}} = marina:query(Query,
         #{page_size => 1, paging_state => PagingState}),
    {result_metadata, 4, _, PagingState2} = Metadata2,

    ?assertNotEqual(PagingState, PagingState2),
    ?assertNotEqual(Rows, Rows2).

query_subtest() ->
    Response = query(?QUERY1),
    ?assertEqual(?QUERY1_RESULT, Response).

query_metedata_types_subtest() ->
    query(<<"DROP TABLE types;">>),
    Columns = datatypes_columns(?DATA_TYPES),
    Query = <<"CREATE TABLE types (",  Columns/binary, " PRIMARY KEY(col1));">>,
    Response = query(Query),
    ?assertEqual({ok, {<<"CREATED">>, <<"TABLE">>, {<<"test">>, <<"types">>}}},
        Response),

    Values = [
        <<"hello">>,
        marina_types:encode_long(100000),
        <<"blob">>,
        marina_types:encode_boolean(true)
    ] ,
    Response2 = marina:query(<<"INSERT INTO types (col1, col2, col3, col4)
        VALUES (?, ?, ?, ?)">>, #{values => Values}),

    ?assertEqual({ok, undefined}, Response2),

    Response3 = query(<<"SELECT * FROM types LIMIT 1;">>),

    ?assertEqual({ok, {result,
        {result_metadata, 17,
            [{column_spec, <<"test">>, <<"types">>, <<"col1">>, ascii},
             {column_spec, <<"test">>, <<"types">>, <<"col10">>, uid},
             {column_spec, <<"test">>, <<"types">>, <<"col11">>, varchar},
             {column_spec, <<"test">>, <<"types">>, <<"col12">>, varint},
             {column_spec, <<"test">>, <<"types">>, <<"col13">>, timeuuid},
             {column_spec, <<"test">>, <<"types">>, <<"col14">>, inet},
             {column_spec, <<"test">>, <<"types">>, <<"col15">>,
                 {list, varchar}},
             {column_spec, <<"test">>, <<"types">>, <<"col16">>,
                 {map, varchar, varchar}},
             {column_spec, <<"test">>, <<"types">>, <<"col17">>,
                 {set, varchar}},
             {column_spec, <<"test">>, <<"types">>, <<"col2">>, bigint},
             {column_spec, <<"test">>, <<"types">>, <<"col3">>, blob},
             {column_spec, <<"test">>, <<"types">>, <<"col4">>, boolean},
             {column_spec, <<"test">>, <<"types">>, <<"col5">>, decimal},
             {column_spec, <<"test">>, <<"types">>, <<"col6">>, double},
             {column_spec, <<"test">>, <<"types">>, <<"col7">>, float},
             {column_spec, <<"test">>, <<"types">>, <<"col8">>, int},
             {column_spec, <<"test">>, <<"types">>, <<"col9">>, timestamp}],
            undefined},
        1,
        [[<<"hello">>, null, null, null, null, null, null, null, null,
            <<0, 0, 0, 0, 0, 1, 134, 160>>, <<"blob">>, <<1>>, null, null,
            null, null, null]]
    }}, Response3).

query_no_metadata_subtest() ->
    Response2 = marina:query(?QUERY1, #{skip_metadata => true}),

    ?assertEqual({ok, {result,
    {result_metadata, 4, [], undefined}, 1, [
        [<<153, 73, 45, 254, 217, 74, 17, 228, 175, 57, 88, 244, 65, 16, 117,
            125>>, <<"test">>, <<"test2">>, <<0, 0, 0, 0>>]
    ]}}, Response2).

reusable_query_subtest() ->
    Response = marina:reusable_query(?QUERY1, #{}),
    Response = marina:reusable_query(?QUERY1, #{}),
    Response = marina:reusable_query(?QUERY2, #{values => ?QUERY2_VALUES}),

    ?assertEqual(?QUERY1_RESULT, Response).

reusable_query_invalid_query_subtest() ->
    Query = <<"SELECT * FROM user LIMIT 1;">>,
    {error, {8704, _}} = marina:reusable_query(Query, #{}).

schema_changes_subtest() ->
    query(<<"DROP KEYSPACE test2;">>),
    query(<<"CREATE KEYSPACE test2 WITH REPLICATION =
        {'class':'SimpleStrategy', 'replication_factor':1};">>),
    query(<<"CREATE TYPE test2.address (street text, city text, zip_code int,
        phones set<text>);">>),
    query(<<"CREATE TABLE test2.users (key uuid, column1 text,
        column2 frozen<test2.address>, value blob, PRIMARY KEY
        (key, column1, column2));">>),
    Response = query(<<"SELECT * FROM test2.users LIMIT 1;">>),

    ?assertEqual({ok, {result,
    {result_metadata, 4,
        [{column_spec, <<"test2">>, <<"users">>, <<"key">>, uid},
         {column_spec, <<"test2">>, <<"users">>, <<"column1">>, varchar},
         {column_spec, <<"test2">>, <<"users">>, <<"column2">>,
             {udt, <<"test2">>, <<"address">>,
                 [{<<"street">>, varchar},
                  {<<"city">>, varchar},
                  {<<"zip_code">>, int},
                  {<<"phones">>, {set, varchar}}]}},
         {column_spec, <<"test2">>, <<"users">>, <<"value">>, blob}],
             undefined},
    0, []}}, Response),

    query(<<"DROP TABLE test2.users;">>),
    query(<<"DROP KEYSPACE test2;">>).

tuples_subtest() ->
    query(<<"CREATE TABLE collect_things (k int PRIMARY KEY,
        v frozen <tuple<int, text, float>>);">>),
    Response = query(<<"SELECT * FROM test.collect_things;">>),

    ?assertEqual({ok, {result,
        {result_metadata, 2,
            [{column_spec, <<"test">>, <<"collect_things">>, <<"k">>, int},
             {column_spec, <<"test">>, <<"collect_things">>, <<"v">>,
                 {tuple, [int, varchar, float]}}],
            undefined},
    0, []}}, Response),

    query(<<"DROP TABLE collect_things;">>).

%% utils
bootstrap() ->
    marina_app:start(),
    timer:sleep(500),
    query(<<"CREATE KEYSPACE test WITH REPLICATION =
        {'class':'SimpleStrategy', 'replication_factor':1};">>),
    query(<<"CREATE TABLE test.users (key uuid, column1 text,
        column2 text, value blob, PRIMARY KEY (key, column1, column2));">>),
    query(<<"INSERT INTO test.users (key, column1, column2, value)
        values (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2',
        intAsBlob(0))">>),
    marina_app:stop().

cleanup() ->
    marina_app:stop().

datatypes_columns(Cols) ->
    list_to_binary(datatypes_columns(1, Cols)).

datatypes_columns(_I, []) ->
    [];
datatypes_columns(I, [ColumnType|Rest]) ->
    Column = io_lib:format("col~B ~s, ", [I, ColumnType]),
    [Column | datatypes_columns(I + 1, Rest)].

query(Query) ->
    marina:query(Query, #{
        consistency_level => ?CONSISTENCY_LOCAL_ONE,
        timeout => ?TIMEOUT
    }).

setup(KeyVals) ->
    error_logger:tty(false),
    bootstrap(),
    application:load(marina),
    set_env(KeyVals),
    marina_app:start(),
    timer:sleep(250).

set_env([]) ->
    ok;
set_env([{K, V} | T]) ->
    application:set_env(?APP, K, V),
    set_env(T).
