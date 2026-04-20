%% Scylla shard-awareness helpers.
%%
%% During the CQL handshake, ScyllaDB advertises a handful of SCYLLA_*
%% options in the SUPPORTED frame that let a driver route each query to
%% the physical CPU shard that owns the partition. Cassandra doesn't
%% ship these keys; `parse/1` returns `undefined` in that case and the
%% caller falls back to node-level routing.
%%
%% Keys of interest:
%%   SCYLLA_SHARD                 — shard this connection currently maps to
%%   SCYLLA_NR_SHARDS             — shard count on the node
%%   SCYLLA_SHARDING_ALGORITHM    — typically <<"biased_token_round_robin">>
%%   SCYLLA_SHARDING_IGNORE_MSB   — left-shift applied before the fast-mod
%%   SCYLLA_PARTITIONER           — e.g. <<"org.apache.cassandra.dht.Murmur3Partitioner">>
-module(marina_shard).

-export([
    parse/1,
    shard_of_token/2
]).

-record(shard_info, {
    shard       :: non_neg_integer(),
    nr_shards   :: pos_integer(),
    ignore_msb  :: non_neg_integer(),
    algorithm   :: binary(),
    partitioner :: binary()
}).

-type shard_info() :: #shard_info {}.
-export_type([shard_info/0]).

-spec parse([{binary(), [binary()]}]) -> shard_info() | undefined.

parse(Multimap) ->
    case lookup(<<"SCYLLA_NR_SHARDS">>, Multimap) of
        undefined ->
            undefined;
        NrShardsBin ->
            #shard_info {
                shard = parse_int(
                    lookup(<<"SCYLLA_SHARD">>, Multimap), 0),
                nr_shards = binary_to_integer(NrShardsBin),
                ignore_msb = parse_int(
                    lookup(<<"SCYLLA_SHARDING_IGNORE_MSB">>, Multimap), 0),
                algorithm = lookup_default(
                    <<"SCYLLA_SHARDING_ALGORITHM">>, Multimap,
                    <<"biased_token_round_robin">>),
                partitioner = lookup_default(
                    <<"SCYLLA_PARTITIONER">>, Multimap, <<>>)
            }
    end.

%% Scylla's biased_token_round_robin: bias the signed i64 token into a
%% u64, shift left by IgnoreMsb (dropping high bits), multiply by
%% NrShards as a 128-bit operation, and take the top 64 bits.
%%
%% Equivalent to the C++ reference in Scylla's sharding_algorithm.hh:
%%   biased        = uint64(token + 2^63)
%%   biased_shifted = (biased << ignore_msb) & (2^64 - 1)
%%   shard         = (biased_shifted * nr_shards) >> 64
-spec shard_of_token(integer(), shard_info()) -> non_neg_integer().

shard_of_token(Token, #shard_info {
        nr_shards = NrShards,
        ignore_msb = IgnoreMsb
    }) ->

    Biased = (Token + (1 bsl 63)) band ((1 bsl 64) - 1),
    Shifted = (Biased bsl IgnoreMsb) band ((1 bsl 64) - 1),
    (Shifted * NrShards) bsr 64.

%% private
lookup(Key, Multimap) ->
    case lists:keyfind(Key, 1, Multimap) of
        {Key, [Value | _]} -> Value;
        {Key, []} -> undefined;
        false -> undefined
    end.

lookup_default(Key, Multimap, Default) ->
    case lookup(Key, Multimap) of
        undefined -> Default;
        Value -> Value
    end.

parse_int(undefined, Default) -> Default;
parse_int(Bin, _Default) -> binary_to_integer(Bin).
