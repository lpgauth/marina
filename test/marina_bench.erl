%% Ring-lookup micro-benchmark. Compares the status-quo compiled-module
%% implementation (marina_ring_utils) against two persistent_term variants.
%%
%% Run via `make bench`, or from a rebar3 shell:
%%   rebar3 as test shell
%%   1> marina_bench:run().
-module(marina_bench).

-export([
    run/0
]).

-define(SIZES, [{3, 256}, {12, 256}, {48, 256}]).
-define(KEYS, 10000).
-define(RUNS, 7).
-define(WARMUP_ITERS, 3).

-spec run() -> ok.
run() ->
    io:format("~n=== marina ring lookup benchmark ===~n"),
    io:format("~-10s ~-30s ~12s ~14s~n",
        ["cluster", "variant", "ns/op", "ops/sec"]),
    io:format("~s~n", [lists:duplicate(70, $-)]),
    lists:foreach(fun ({N, V}) -> run_cluster(N, V) end, ?SIZES),
    io:format("~n"),
    ok.

%% private
run_cluster(NumNodes, VNodes) ->
    Label = lists:flatten(io_lib:format("~Bx~B", [NumNodes, VNodes])),
    {Nodes, SortedTokens, Hashes, Keys} = synth(NumNodes, VNodes),

    %% Hash-only (no ring lookup) to isolate the Murmur3 NIF cost.
    warmup(fun () -> loop_hash(Keys) end),
    TH = bench_best(?RUNS, fun () -> loop_hash(Keys) end),
    report(Label, "murmur3 hash only", TH),

    %% Ring lookup variants on pre-hashed integer keys.
    ok = setup_compiled(Nodes),
    warmup(fun () -> loop_compiled(Hashes) end),
    T1 = bench_best(?RUNS, fun () -> loop_compiled(Hashes) end),
    report(Label, "compiled lookup (hashed)", T1),

    ok = marina_ring_pt:build(SortedTokens),
    warmup(fun () -> loop_pt(Hashes) end),
    T2 = bench_best(?RUNS, fun () -> loop_pt(Hashes) end),
    report(Label, "pt tuple lookup (hashed)", T2),

    ok = marina_ring_bin:build(SortedTokens),
    warmup(fun () -> loop_bin(Hashes) end),
    T3 = bench_best(?RUNS, fun () -> loop_bin(Hashes) end),
    report(Label, "pt binary lookup (hashed)", T3),

    %% Pre-9be2a67 linear-scan implementation — quantifies the gap the recent
    %% tree rewrite closed. Production was likely running this version.
    ok = marina_ring_linear:build(SortedTokens),
    warmup(fun () -> loop_linear(Hashes) end),
    TL = bench_best(?RUNS, fun () -> loop_linear(Hashes) end),
    report(Label, "linear (pre-tree, hashed)", TL),
    marina_ring_linear:teardown(),

    %% Full path: marina_ring:lookup/1 (hash + compiled lookup).
    warmup(fun () -> loop_ring(Keys) end),
    TR = bench_best(?RUNS, fun () -> loop_ring(Keys) end),
    report(Label, "marina_ring:lookup/1 full", TR),

    %% End-to-end: marina_pool:node/1 (foil + hash + compiled lookup).
    setup_foil(NumNodes),
    warmup(fun () -> loop_pool(Keys) end),
    TP = bench_best(?RUNS, fun () -> loop_pool(Keys) end),
    report(Label, "marina_pool:node/1 full", TP),
    teardown_foil(),

    marina_ring_pt:teardown(),
    marina_ring_bin:teardown(),
    ok.

synth(NumNodes, VNodes) ->
    rand:seed(exsss, {NumNodes, VNodes, 42}),
    Nodes = [{ip(X), random_tokens_bin(VNodes)} || X <- lists:seq(1, NumNodes)],
    SortedTokens = lists:usort(lists:append(
        [[{Tok, marina_pool:node_id(Addr)} || Tok <- decode_tokens(TokBin)]
         || {Addr, TokBin} <- Nodes])),
    Hashes = [rand:uniform(1 bsl 63) * 2 - (1 bsl 63)
              || _ <- lists:seq(1, ?KEYS)],
    %% UUID-shaped binary keys, representative of partition-key sizes.
    Keys = [crypto:strong_rand_bytes(16) || _ <- lists:seq(1, ?KEYS)],
    {Nodes, SortedTokens, Hashes, Keys}.

ip(X) ->
    %% 10.0.X1.X2 — unique per node within one benchmark run
    <<10, 0, (X div 256):8, (X rem 256):8>>.

random_tokens_bin(N) ->
    Tokens = [rand:uniform(1 bsl 63) * 2 - (1 bsl 63) - 1
              || _ <- lists:seq(1, N)],
    Strings = [integer_to_binary(T) || T <- Tokens],
    encode_long_string_set(Strings).

decode_tokens(TokBin) ->
    {Toks, <<>>} = marina_types:decode_long_string_set(TokBin),
    [binary_to_integer(T) || T <- Toks].

encode_long_string_set(Strings) ->
    Count = length(Strings),
    Parts = [<<(byte_size(S)):32, S/binary>> || S <- Strings],
    iolist_to_binary([<<Count:32>>, Parts]).

setup_compiled(Nodes) ->
    ok = marina_ring:build(Nodes).

loop_compiled([]) -> ok;
loop_compiled([H | T]) ->
    _ = marina_ring_utils:lookup(H),
    loop_compiled(T).

loop_pt([]) -> ok;
loop_pt([H | T]) ->
    _ = marina_ring_pt:lookup(H),
    loop_pt(T).

loop_bin([]) -> ok;
loop_bin([H | T]) ->
    _ = marina_ring_bin:lookup(H),
    loop_bin(T).

loop_linear([]) -> ok;
loop_linear([H | T]) ->
    _ = marina_ring_linear:lookup(H),
    loop_linear(T).

loop_hash([]) -> ok;
loop_hash([K | T]) ->
    _ = marina_token:m3p(K),
    loop_hash(T).

loop_ring([]) -> ok;
loop_ring([K | T]) ->
    _ = marina_ring:lookup(K),
    loop_ring(T).

loop_pool([]) -> ok;
loop_pool([K | T]) ->
    _ = marina_pool:node(K),
    loop_pool(T).

setup_foil(NumNodes) ->
    %% foil needs its supervisor tree running to own/compile the lookup table.
    _ = application:ensure_all_started(foil),
    _ = catch foil:delete(marina_pool),
    ok = marina_pool:init(),
    ok = foil:insert(marina_pool, strategy, {token_aware, NumNodes}),
    ok = foil:load(marina_pool),
    %% Sanity: the lookup must actually succeed or the benchmark is measuring
    %% the foil `try/catch undef` error path, not the success path.
    {ok, {token_aware, NumNodes}} = foil:lookup(marina_pool, strategy),
    ok.

teardown_foil() ->
    _ = catch foil:delete(marina_pool),
    ok.

warmup(Fun) ->
    lists:foreach(fun (_) -> Fun() end, lists:seq(1, ?WARMUP_ITERS)),
    ok.

bench_best(Runs, Fun) ->
    lists:min([run_once(Fun) || _ <- lists:seq(1, Runs)]).

run_once(Fun) ->
    T0 = erlang:monotonic_time(nanosecond),
    Fun(),
    T1 = erlang:monotonic_time(nanosecond),
    T1 - T0.

report(Cluster, Variant, TimeNs) ->
    NsPerOp = TimeNs / ?KEYS,
    OpsPerSec = trunc(1.0e9 / NsPerOp),
    io:format("~-10s ~-26s ~12.1f ~14w~n",
        [Cluster, Variant, NsPerOp, OpsPerSec]).
