-module(marina_shard_tests).

-include_lib("eunit/include/eunit.hrl").

parse_cassandra_multimap_returns_undefined_test() ->
    %% A Cassandra SUPPORTED response has CQL_VERSION / COMPRESSION but
    %% no SCYLLA_* keys. parse/1 must return undefined so callers can
    %% skip shard routing and fall back to node-level dispatch.
    Multimap = [
        {<<"CQL_VERSION">>, [<<"3.4.4">>]},
        {<<"COMPRESSION">>, [<<"lz4">>, <<"snappy">>]}
    ],
    ?assertEqual(undefined, marina_shard:parse(Multimap)).

parse_scylla_multimap_populates_record_test() ->
    Multimap = [
        {<<"CQL_VERSION">>, [<<"3.4.4">>]},
        {<<"SCYLLA_SHARD">>, [<<"3">>]},
        {<<"SCYLLA_NR_SHARDS">>, [<<"16">>]},
        {<<"SCYLLA_PARTITIONER">>,
            [<<"org.apache.cassandra.dht.Murmur3Partitioner">>]},
        {<<"SCYLLA_SHARDING_ALGORITHM">>, [<<"biased_token_round_robin">>]},
        {<<"SCYLLA_SHARDING_IGNORE_MSB">>, [<<"12">>]}
    ],
    Info = marina_shard:parse(Multimap),
    ?assertEqual(3,  element(2, Info)),  %% shard
    ?assertEqual(16, element(3, Info)),  %% nr_shards
    ?assertEqual(12, element(4, Info)),  %% ignore_msb
    ?assertEqual(<<"biased_token_round_robin">>, element(5, Info)),
    ?assertEqual(<<"org.apache.cassandra.dht.Murmur3Partitioner">>,
        element(6, Info)).

parse_defaults_optional_keys_test() ->
    %% If a server advertises SCYLLA_NR_SHARDS but omits the algorithm
    %% name or the shard index on this connection, we should still
    %% produce a usable record with the documented default values.
    Multimap = [{<<"SCYLLA_NR_SHARDS">>, [<<"8">>]}],
    Info = marina_shard:parse(Multimap),
    ?assertEqual(0, element(2, Info)),
    ?assertEqual(8, element(3, Info)),
    ?assertEqual(0, element(4, Info)),
    ?assertEqual(<<"biased_token_round_robin">>, element(5, Info)),
    ?assertEqual(<<>>, element(6, Info)).

shard_of_token_is_stable_for_known_token_test() ->
    %% Reference value cross-checked against Scylla's own
    %% sharding_algorithm implementation: a token biased, left-shifted,
    %% multiplied by nr_shards, and folded back into the top 64 bits.
    Info = fake_info(16, 12),
    ?assertEqual(shard_of_token_reference(0, 16, 12),
        marina_shard:shard_of_token(0, Info)),
    ?assertEqual(shard_of_token_reference(-1, 16, 12),
        marina_shard:shard_of_token(-1, Info)),
    ?assertEqual(shard_of_token_reference(12345, 16, 12),
        marina_shard:shard_of_token(12345, Info)).

shard_of_token_covers_full_range_test() ->
    %% Sweep a few thousand random tokens and make sure every returned
    %% shard is within [0, nr_shards).
    Info = fake_info(7, 12),
    lists:foreach(fun (_) ->
        Token = rand:uniform(1 bsl 64) - (1 bsl 63),
        Shard = marina_shard:shard_of_token(Token, Info),
        ?assert(Shard >= 0),
        ?assert(Shard < 7)
    end, lists:seq(1, 5000)).

shard_of_token_single_shard_is_always_zero_test() ->
    %% Degenerate case: a 1-shard node must return shard 0 for every
    %% token.
    Info = fake_info(1, 12),
    ?assertEqual(0, marina_shard:shard_of_token(0, Info)),
    ?assertEqual(0, marina_shard:shard_of_token(-1, Info)),
    ?assertEqual(0, marina_shard:shard_of_token(1 bsl 62, Info)),
    ?assertEqual(0, marina_shard:shard_of_token(-(1 bsl 62), Info)).

%% helpers
fake_info(NrShards, IgnoreMsb) ->
    Multimap = [
        {<<"SCYLLA_NR_SHARDS">>, [integer_to_binary(NrShards)]},
        {<<"SCYLLA_SHARDING_IGNORE_MSB">>, [integer_to_binary(IgnoreMsb)]}
    ],
    marina_shard:parse(Multimap).

shard_of_token_reference(Token, NrShards, IgnoreMsb) ->
    Biased = (Token + (1 bsl 63)) band ((1 bsl 64) - 1),
    Shifted = (Biased bsl IgnoreMsb) band ((1 bsl 64) - 1),
    (Shifted * NrShards) bsr 64.
