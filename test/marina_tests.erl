-module(marina_tests).

-include("test.hrl").

-define(IP, "172.18.0.2").

%% --- test generators ------------------------------------------------

marina_test_() ->
    marina_suite([], [
        fun async_query/0,
        fun async_reusable_query/0,
        fun async_reusable_query_invalid/0,
        fun batch/0,
        fun counters/0,
        fun paging/0,
        fun query/0,
        fun query_data_types/0,
        fun query_skip_metadata/0,
        fun reusable_query/0,
        fun reusable_query_invalid/0,
        fun schema_change_udt/0,
        fun tuples/0
    ]).

marina_compression_test_() ->
    marina_suite([{compression, true}], [fun query/0]).

%% --- setup ----------------------------------------------------------

marina_suite(ExtraEnv, Tests) ->
    {setup,
        fun () -> start(ExtraEnv) end,
        fun (_) -> marina_app:stop() end,
        {inparallel, Tests}}.

start(ExtraEnv) ->
    error_logger:tty(false),
    application:load(marina),
    application:set_env(marina, ip, ?IP),
    application:set_env(marina, keyspace, <<"test">>),
    [application:set_env(marina, K, V) || {K, V} <- ExtraEnv],
    marina_app:start(),
    timer:sleep(250),
    ensure_base_schema().

ensure_base_schema() ->
    q(<<"CREATE KEYSPACE IF NOT EXISTS test WITH REPLICATION ="
        " {'class':'SimpleStrategy', 'replication_factor':1};">>),
    q(<<"CREATE TABLE IF NOT EXISTS test.users ("
        " key uuid, column1 text, column2 text, value blob,"
        " PRIMARY KEY (key, column1, column2));">>),
    q(<<"INSERT INTO test.users (key, column1, column2, value) VALUES"
        " (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2',"
        " intAsBlob(0));">>).

%% --- API helpers ----------------------------------------------------

opts() ->
    #{consistency_level => ?CONSISTENCY_LOCAL_ONE, timeout => ?TIMEOUT}.

opts(Extra) ->
    maps:merge(opts(), Extra).

q(Query)         -> marina:query(Query, opts()).
q(Query, Extra)  -> marina:query(Query, opts(Extra)).

rq(Query)        -> marina:reusable_query(Query, opts()).
rq(Query, Extra) -> marina:reusable_query(Query, opts(Extra)).

aq(Query)        -> marina:async_query(Query, opts()).
arq(Query)       -> marina:async_reusable_query(Query, opts()).
arq(Query, Extra)-> marina:async_reusable_query(Query, opts(Extra)).

b(Queries, Extra) -> marina:batch(Queries, opts(Extra)).

with_scratch(Name, DDL, Fun) ->
    Drop = <<"DROP TABLE IF EXISTS test.", Name/binary, ";">>,
    q(Drop),
    q(DDL),
    try Fun() after q(Drop) end.

%% --- tests ----------------------------------------------------------

async_query() ->
    {ok, Ref} = aq(?QUERY1),
    ?assertEqual(?QUERY1_RESULT, marina:receive_response(Ref)).

async_reusable_query() ->
    {ok, R1} = arq(?QUERY3),
    ?QUERY1_RESULT = marina:receive_response(R1),
    {ok, R2} = arq(?QUERY2, #{values => ?QUERY2_VALUES}),
    ?QUERY1_RESULT = marina:receive_response(R2),
    {ok, R3} = arq(?QUERY2, #{values => ?QUERY2_VALUES}),
    ?assertEqual(?QUERY1_RESULT, marina:receive_response(R3)).

async_reusable_query_invalid() ->
    ?assertMatch({error, {8704, _}},
        marina:async_reusable_query(<<"SELECT * FROM user LIMIT 1;">>, #{})).

batch() ->
    with_scratch(<<"batch_rows">>,
        <<"CREATE TABLE test.batch_rows (k int PRIMARY KEY, v text);">>,
        fun () ->
            %% LOGGED batch of raw inserts
            {ok, undefined} = b([
                {query, <<"INSERT INTO test.batch_rows (k, v) VALUES (1, 'a')">>, []},
                {query, <<"INSERT INTO test.batch_rows (k, v) VALUES (2, 'b')">>, []}
            ], #{batch_type => logged}),
            ?assertMatch({ok, {result, _, 2, _}},
                q(<<"SELECT * FROM test.batch_rows;">>)),

            %% Warm cache + pull prepared id, then UNLOGGED with mixed kinds
            Insert = <<"INSERT INTO test.batch_rows (k, v) VALUES (?, ?)">>,
            {ok, _} = rq(Insert,
                #{values => [marina_types:encode_int(3), <<"c">>]}),
            {ok, Pool} = marina_pool:node(undefined),
            {ok, StatementId} = marina_cache:get(Pool, Insert),
            {ok, undefined} = b([
                {prepared, StatementId,
                    [marina_types:encode_int(4), <<"d">>]},
                {query, <<"INSERT INTO test.batch_rows (k, v) VALUES (5, 'e')">>, []}
            ], #{batch_type => unlogged}),
            ?assertMatch({ok, {result, _, 5, _}},
                q(<<"SELECT * FROM test.batch_rows;">>))
        end).

counters() ->
    with_scratch(<<"page_view_counts">>,
        <<"CREATE TABLE test.page_view_counts ("
          " counter_value counter,"
          " url_name varchar,"
          " page_name varchar,"
          " PRIMARY KEY (url_name, page_name));">>,
        fun () ->
            q(<<"UPDATE test.page_view_counts SET"
                " counter_value = counter_value + 1"
                " WHERE url_name='adgear.com' AND page_name='home';">>),
            ?assertEqual({ok, {result,
                {result_metadata, 3,
                    [{column_spec, <<"test">>, <<"page_view_counts">>,
                        <<"url_name">>, varchar},
                     {column_spec, <<"test">>, <<"page_view_counts">>,
                        <<"page_name">>, varchar},
                     {column_spec, <<"test">>, <<"page_view_counts">>,
                        <<"counter_value">>, counter}],
                    undefined},
                1,
                [[<<"adgear.com">>, <<"home">>, <<0, 0, 0, 0, 0, 0, 0, 1>>]]}},
                q(<<"SELECT * FROM test.page_view_counts">>))
        end).

paging() ->
    q(<<"INSERT INTO test.users (key, column1, column2, value) VALUES"
        " (99492dfe-d94a-11e4-af39-58f44110757e, 'test', 'test2',"
        " intAsBlob(0));">>),
    Query = <<"SELECT * FROM users LIMIT 10;">>,
    {ok, {result, M1, 1, R1}} = q(Query, #{page_size => 1}),
    {result_metadata, 4, _, State1} = M1,
    {ok, {result, M2, 1, R2}} = q(Query,
        #{page_size => 1, paging_state => State1}),
    {result_metadata, 4, _, State2} = M2,
    ?assertNotEqual(State1, State2),
    ?assertNotEqual(R1, R2).

query() ->
    ?assertEqual(?QUERY1_RESULT, q(?QUERY1)).

query_data_types() ->
    q(<<"DROP TABLE IF EXISTS test.types;">>),
    ColumnDefs = build_column_defs(?DATA_TYPES),
    q(<<"CREATE TABLE test.types (", ColumnDefs/binary,
        " PRIMARY KEY(col1));">>),

    Values = [
        <<"hello">>,
        marina_types:encode_long(100000),
        <<"blob">>,
        marina_types:encode_boolean(true)
    ],
    ?assertEqual({ok, undefined},
        q(<<"INSERT INTO test.types (col1, col2, col3, col4)"
            " VALUES (?, ?, ?, ?)">>, #{values => Values})),

    ExpectedRow = [
        <<"hello">>,                       %% col1 (ascii)
        null, null, null, null, null,      %% col10..col14
        null, null, null,                  %% col15..col17
        <<0, 0, 0, 0, 0, 1, 134, 160>>,    %% col2 (bigint 100000)
        <<"blob">>,                        %% col3
        <<1>>,                             %% col4 (boolean true)
        null, null, null, null, null       %% col5..col9
    ],
    ExpectedMetadata = expected_types_metadata(?DATA_TYPES),
    ?assertEqual({ok, {result,
        {result_metadata, length(?DATA_TYPES), ExpectedMetadata, undefined},
        1, [ExpectedRow]}},
        q(<<"SELECT * FROM test.types LIMIT 1;">>)).

query_skip_metadata() ->
    ?assertEqual({ok, {result,
        {result_metadata, 4, [], undefined}, 1,
        [[<<153, 73, 45, 254, 217, 74, 17, 228, 175, 57, 88,
            244, 65, 16, 117, 125>>,
          <<"test">>, <<"test2">>, <<0, 0, 0, 0>>]]}},
        q(?QUERY1, #{skip_metadata => true})).

reusable_query() ->
    R = rq(?QUERY1),
    R = rq(?QUERY1),
    R = rq(?QUERY2, #{values => ?QUERY2_VALUES}),
    ?assertEqual(?QUERY1_RESULT, R).

reusable_query_invalid() ->
    ?assertMatch({error, {8704, _}},
        marina:reusable_query(<<"SELECT * FROM user LIMIT 1;">>, #{})).

schema_change_udt() ->
    q(<<"DROP KEYSPACE IF EXISTS test2;">>),
    q(<<"CREATE KEYSPACE test2 WITH REPLICATION ="
        " {'class':'SimpleStrategy', 'replication_factor':1};">>),
    q(<<"CREATE TYPE test2.address ("
        " street text, city text, zip_code int, phones set<text>);">>),
    q(<<"CREATE TABLE test2.users ("
        " key uuid, column1 text,"
        " column2 frozen<test2.address>, value blob,"
        " PRIMARY KEY (key, column1, column2));">>),
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
        0, []}},
        q(<<"SELECT * FROM test2.users LIMIT 1;">>)),
    q(<<"DROP TABLE test2.users;">>),
    q(<<"DROP KEYSPACE test2;">>).

tuples() ->
    with_scratch(<<"collect_things">>,
        <<"CREATE TABLE test.collect_things ("
          " k int PRIMARY KEY,"
          " v frozen<tuple<int, text, float>>);">>,
        fun () ->
            ?assertEqual({ok, {result,
                {result_metadata, 2,
                    [{column_spec, <<"test">>, <<"collect_things">>,
                        <<"k">>, int},
                     {column_spec, <<"test">>, <<"collect_things">>,
                        <<"v">>, {tuple, [int, varchar, float]}}],
                    undefined},
                0, []}},
                q(<<"SELECT * FROM test.collect_things;">>))
        end).

%% --- data-types fixtures --------------------------------------------

build_column_defs(Types) ->
    Indexed = lists:zip(lists:seq(1, length(Types)), Types),
    iolist_to_binary(
        [io_lib:format("col~B ~s, ", [I, T]) || {I, T} <- Indexed]).

expected_types_metadata(Types) ->
    Indexed = lists:zip(lists:seq(1, length(Types)), Types),
    Sorted = lists:sort(fun ({I1, _}, {I2, _}) ->
        integer_to_list(I1) =< integer_to_list(I2)
    end, Indexed),
    [{column_spec, <<"test">>, <<"types">>,
        list_to_binary("col" ++ integer_to_list(I)),
        type_atom(T)} || {I, T} <- Sorted].

type_atom(ascii)             -> ascii;
type_atom(bigint)            -> bigint;
type_atom(blob)              -> blob;
type_atom(boolean)           -> boolean;
type_atom(decimal)           -> decimal;
type_atom(double)            -> double;
type_atom(float)             -> float;
type_atom(inet)              -> inet;
type_atom(int)               -> int;
type_atom('list<text>')      -> {list, varchar};
type_atom('map<text, text>') -> {map, varchar, varchar};
type_atom('set<text>')       -> {set, varchar};
type_atom(timestamp)         -> timestamp;
type_atom(timeuuid)          -> timeuuid;
type_atom(uuid)              -> uid;
type_atom(varchar)           -> varchar;
type_atom(varint)            -> varint.
