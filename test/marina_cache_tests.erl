-module(marina_cache_tests).

-include("test.hrl").

marina_cache_test_() ->
    {setup,
        fun () -> marina_cache:init(), ok end,
        fun (_) -> ets:delete(?ETS_TABLE_CACHE) end,
        [fun put_then_get/0,
         fun get_missing_is_not_found/0,
         fun erase_removes_single_entry/0,
         fun erase_missing_is_not_found/0,
         fun erase_pool_removes_all_for_pool/0,
         fun erase_pool_leaves_other_pools_alone/0,
         fun erase_server_strips_index_suffix/0]}.

put_then_get() ->
    ok = marina_cache:put(pool_a, <<"q1">>, <<"id1">>),
    ?assertEqual({ok, <<"id1">>}, marina_cache:get(pool_a, <<"q1">>)).

get_missing_is_not_found() ->
    ?assertEqual({error, not_found}, marina_cache:get(pool_a, <<"nope">>)).

erase_removes_single_entry() ->
    ok = marina_cache:put(pool_a, <<"q2">>, <<"id2">>),
    ok = marina_cache:erase(pool_a, <<"q2">>),
    ?assertEqual({error, not_found}, marina_cache:get(pool_a, <<"q2">>)).

erase_missing_is_not_found() ->
    %% Deleting a non-existent entry returns ok from ETS (idempotent),
    %% not {error, not_found}. erase_server/1 was called on every socket
    %% close during the dead-code era, so fast-path ok is the right
    %% shape here.
    ?assertEqual(ok, marina_cache:erase(pool_a, <<"never-cached">>)).

erase_pool_removes_all_for_pool() ->
    ok = marina_cache:put(pool_b, <<"q1">>, <<"id1">>),
    ok = marina_cache:put(pool_b, <<"q2">>, <<"id2">>),
    ok = marina_cache:put(pool_b, <<"q3">>, <<"id3">>),
    ?assertEqual(3, marina_cache:erase_pool(pool_b)),
    ?assertEqual({error, not_found}, marina_cache:get(pool_b, <<"q1">>)),
    ?assertEqual({error, not_found}, marina_cache:get(pool_b, <<"q2">>)),
    ?assertEqual({error, not_found}, marina_cache:get(pool_b, <<"q3">>)).

erase_pool_leaves_other_pools_alone() ->
    ok = marina_cache:put(pool_c, <<"q1">>, <<"id_c">>),
    ok = marina_cache:put(pool_d, <<"q1">>, <<"id_d">>),
    ?assertEqual(1, marina_cache:erase_pool(pool_c)),
    ?assertEqual({error, not_found}, marina_cache:get(pool_c, <<"q1">>)),
    ?assertEqual({ok, <<"id_d">>}, marina_cache:get(pool_d, <<"q1">>)).

erase_server_strips_index_suffix() ->
    %% shackle registers each pool connection as `pool_name_<index>`.
    %% erase_server/1 takes a request_id `{server_name, _ref}` and
    %% must strip the trailing index to land back on the pool atom.
    %% Regression guard — this public API is consumed by external
    %% callers that hit it from the 9472 Unprepared error path.
    ok = marina_cache:put('marina_10.0.0.1', <<"q">>, <<"id">>),
    ok = marina_cache:erase_server({'marina_10.0.0.1_3', make_ref()}),
    ?assertEqual({error, not_found},
        marina_cache:get('marina_10.0.0.1', <<"q">>)).
