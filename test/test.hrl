-include_lib("eunit/include/eunit.hrl").
-include_lib("marina/include/marina.hrl").

-define(DATA_TYPES, [
    ascii,
    bigint,
    blob,
    boolean,
    decimal,
    double,
    float,
    int,
    timestamp,
    uuid,
    varchar,
    varint,
    timeuuid,
    inet,
    'list<text>',
    'map<text, text>',
    'set<text>'
]).
-define(QUERY1, <<"SELECT * FROM users LIMIT 1;">>).
-define(QUERY1_RESULT, {ok, {result,
    {result_metadata, 4, [
        {column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
        {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
        {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
        {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}
    ], undefined},
    1,
    [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>, <<"test">>, <<"test2">>, <<0,0,0,0>>]]
}}).
-define(QUERY2, <<"SELECT * FROM users WHERE key = ?;">>).
-define(QUERY2_VALUES, [<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>]).
-define(QUERY3, <<"SELECT * FROM users WHERE key = 99492dfe-d94a-11e4-af39-58f44110757d;">>).
-define(TIMEOUT, 1000).
