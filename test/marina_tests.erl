-module(marina_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("marina/include/marina.hrl").

-compile(export_all).

-define(TEST_TIMEOUT, 10000).
-define(VAR_KEYSPACE, {keyspace, <<"test">>}).

-define(QUERY1, <<"SELECT * FROM users LIMIT 1;">>).
-define(QUERY1_RESULT, {ok,
    {result,
        {result_metadata, 4, [
            {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
            {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
            {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
            {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
        ]}, 1, [
            [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
    ]}
}).

-define(QUERY2, <<"SELECT * FROM users WHERE key = ?;">>).
-define(QUERY2_VALUES, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]).

-define(QUERY3, <<"SELECT * FROM users WHERE key = 99492dfe-d94a-11e4-af39-58f44110757d;">>).

%% runners
marina_test_() ->
    {setup,
        fun () -> setup([?VAR_KEYSPACE]) end,
        fun (_) -> cleanup() end,
    {inparallel, [
        {"test_async_execute", ?MODULE, test_async_execute},
        {"test_async_prepare", ?MODULE, test_async_prepare},
        {"test_async_query", ?MODULE, test_async_query},
        {"test_async_reusable_query", ?MODULE, test_async_reusable_query},
        {"test_async_reusable_query_invalid_query", ?MODULE, test_async_reusable_query_invalid_query},
        {"test_execute", ?MODULE, test_execute},
        {"test_query", ?MODULE, test_query},
        {"test_query_metedata_types", ?MODULE, test_query_metedata_types},
        {"test_query_no_metadata", ?MODULE, test_query_no_metadata},
        {"test_reusable_query", ?MODULE, test_reusable_query},
        {"test_reusable_query_invalid_query", ?MODULE, test_reusable_query_invalid_query},
        {"test_timeout_async", ?MODULE, test_timeout_async},
        {"test_timeout_sync", ?MODULE, test_timeout_sync}
    ]}}.

marina_compression_test_() ->
    {setup,
        fun () -> setup([{compression, true}, ?VAR_KEYSPACE]) end,
        fun (_) -> cleanup() end,
    [{"test_query", ?MODULE, test_query}]}.

marina_connection_error_test_() ->
    {setup,
        fun () -> setup([?VAR_KEYSPACE, {port, 9043}]) end,
        fun (_) -> cleanup() end,
    [{"test_no_socket", ?MODULE, test_no_socket}]}.

marina_backlog_test_() ->
    {setup,
        fun () -> setup([{backlog_size, 1}, ?VAR_KEYSPACE]) end,
        fun (_) -> cleanup() end,
    {inparallel, [
        {"test_backlogfull_async", ?MODULE, test_backlogfull_async},
        {"test_backlogfull_sync", ?MODULE, test_backlogfull_sync}
    ]}}.

%% tests
test_async_execute() ->
    {ok, StatementId} = marina:prepare(?QUERY1, ?TEST_TIMEOUT),
    {ok, Ref} = marina:async_execute(StatementId, ?CONSISTENCY_ONE, [], self()),
    {X, _} = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?assertEqual(ok, X).

test_async_prepare() ->
    {ok, Ref} = marina:async_prepare(?QUERY1, self()),
    {X, _} = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?assertEqual(ok, X).

test_async_query() ->
    {ok, Ref} = marina:async_query(?QUERY1, ?CONSISTENCY_ONE, [], self()),
    Response = marina:receive_response(Ref, ?TEST_TIMEOUT),

    ?assertEqual(?QUERY1_RESULT, Response).

test_async_reusable_query() ->
    {ok, Ref} = marina:async_reusable_query(?QUERY3, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref, ?TEST_TIMEOUT),
    {ok, Ref2} = marina:async_reusable_query(?QUERY2, ?QUERY2_VALUES, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref2, ?TEST_TIMEOUT),
    {ok, Ref3} = marina:async_reusable_query(?QUERY2, ?QUERY2_VALUES, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),
    Response = marina:receive_response(Ref3, ?TEST_TIMEOUT),

    ?assertEqual(?QUERY1_RESULT, Response).

test_async_reusable_query_invalid_query() ->
    Response = marina:async_reusable_query(<<"SELECT * FROM user LIMIT 1;">>, ?CONSISTENCY_ONE, [], self(), ?TEST_TIMEOUT),

    ?assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response).

test_backlogfull_async() ->
    Responses = [marina:async_query(?QUERY1, ?CONSISTENCY_ONE, [], self()) || _ <- lists:seq(1,100)],
    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, Responses)).

test_backlogfull_sync() ->
    Pid = self(),
    [spawn(fun () ->
        X = marina:query(?QUERY1, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
             Pid ! {response, X}
    end) || _ <- lists:seq(1,20)],

    ?assert(lists:any(fun
        ({error, backlog_full}) -> true;
        (_) -> false
    end, receive_loop(20))).

test_execute() ->
    {ok, StatementId} = marina:prepare(?QUERY1, ?TEST_TIMEOUT),
    Response = marina:execute(StatementId, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual(?QUERY1_RESULT, Response).

test_query() ->
    Response = marina:query(?QUERY1, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual(?QUERY1_RESULT, Response).

test_query_metedata_types() ->
    marina:query(<<"DROP TABLE entries;">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    Columns = datatypes_columns([ascii, bigint, blob, boolean, decimal, double,
        float, int, timestamp, uuid, varchar, varint, timeuuid, inet,
        'list<text>', 'map<text,text>', 'set<text>']),
    Query = <<"CREATE TABLE entries(",  Columns/binary, " PRIMARY KEY(col1));">>,
    Response = marina:query(Query, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual({ok,{<<"CREATED">>,<<"TABLE">>,{<<"test">>,<<"entries">>}}}, Response),
    Response2 = marina:query(<<"SELECT * FROM entries LIMIT 1;">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual({ok, {result,
        {result_metadata,17,
            [{column_spec,<<"test">>,<<"entries">>,<<"col1">>,ascii},
             {column_spec,<<"test">>,<<"entries">>,<<"col10">>,uid},
             {column_spec,<<"test">>,<<"entries">>,<<"col11">>,varchar},
             {column_spec,<<"test">>,<<"entries">>,<<"col12">>,varint},
             {column_spec,<<"test">>,<<"entries">>,<<"col13">>,timeuuid},
             {column_spec,<<"test">>,<<"entries">>,<<"col14">>,inet},
             {column_spec,<<"test">>,<<"entries">>,<<"col15">>,{list,varchar}},
             {column_spec,<<"test">>,<<"entries">>,<<"col16">>,{map,varchar,varchar}},
             {column_spec,<<"test">>,<<"entries">>,<<"col17">>,{set,varchar}},
             {column_spec,<<"test">>,<<"entries">>,<<"col2">>,bigint},
             {column_spec,<<"test">>,<<"entries">>,<<"col3">>,blob},
             {column_spec,<<"test">>,<<"entries">>,<<"col4">>,boolean},
             {column_spec,<<"test">>,<<"entries">>,<<"col5">>,decimal},
             {column_spec,<<"test">>,<<"entries">>,<<"col6">>,double},
             {column_spec,<<"test">>,<<"entries">>,<<"col7">>,float},
             {column_spec,<<"test">>,<<"entries">>,<<"col8">>,int},
             {column_spec,<<"test">>,<<"entries">>,<<"col9">>,timestamp}]},
            0, []}}, Response2).

test_query_no_metadata() ->
    Response2 = marina:query(?QUERY1, ?CONSISTENCY_ONE, [{skip_metadata, true}], ?TEST_TIMEOUT),

    ?assertEqual({ok,
        {result,
            {result_metadata, 4, []}, 1, [
                [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]
        ]}
    }, Response2).

test_no_socket() ->
    Response = marina:query(?QUERY1, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual({error, no_socket}, Response).

test_reusable_query() ->
    Response = marina:reusable_query(?QUERY1, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    Response = marina:reusable_query(?QUERY1, [], ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    Response = marina:reusable_query(?QUERY2, ?QUERY2_VALUES, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual(?QUERY1_RESULT, Response).

test_reusable_query_invalid_query() ->
    Response = marina:reusable_query(<<"SELECT * FROM user LIMIT 1;">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    ?assertEqual({error, {8704, <<"unconfigured columnfamily user">>}}, Response).

test_timeout_async() ->
    {ok, Ref} = marina:async_query(?QUERY1, ?CONSISTENCY_ONE, [], self()),
    Response = marina:receive_response(Ref, 0),

    ?assertEqual({error, timeout}, Response).

test_timeout_sync() ->
    Response = marina:query(?QUERY1, ?CONSISTENCY_ONE, [], 0),

    ?assertEqual({error, timeout}, Response).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(marina),
    error_logger:tty(true).

datatypes_columns(Cols) ->
    list_to_binary(datatypes_columns(1, Cols)).

datatypes_columns(_I, []) ->
    [];
datatypes_columns(I, [ColumnType|Rest]) ->
    Column = io_lib:format("col~B ~s, ", [I, ColumnType]),
    [Column | datatypes_columns(I+1, Rest)].

receive_loop(0) -> [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

setup(EnvironmentVars) ->
    error_logger:tty(false),
    application:stop(marina),
    application:load(marina),

    [application:set_env(?APP, K, V) || {K, V} <- EnvironmentVars],

    marina:query(<<"DROP KEYSPACE test;">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    marina:query(<<"CREATE KEYSPACE test WITH REPLICATION = {'class':'SimpleStrategy', 'replication_factor':1};">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    marina:query(<<"CREATE TABLE test.users (key uuid, column1 text, column2 text, value blob, PRIMARY KEY (key, column1, column2));">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),
    marina:query(<<"INSERT INTO test.users (key, column1, column2, value) values (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2', intAsBlob(0))">>, ?CONSISTENCY_ONE, [], ?TEST_TIMEOUT),

    error_logger:tty(false),
    marina_app:start(),
    error_logger:tty(true).
