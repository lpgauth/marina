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
    io:format("~-10s ~-26s ~12s ~14s~n",
        ["cluster", "variant", "ns/op", "ops/sec"]),
    io:format("~s~n", [lists:duplicate(66, $-)]),
    lists:foreach(fun ({N, V}) -> run_cluster(N, V) end, ?SIZES),
    io:format("~n"),
    ok.

%% private
run_cluster(NumNodes, VNodes) ->
    Label = lists:flatten(io_lib:format("~Bx~B", [NumNodes, VNodes])),
    {Nodes, SortedTokens, Hashes} = synth(NumNodes, VNodes),

    ok = setup_compiled(Nodes),
    warmup(fun () -> loop_compiled(Hashes) end),
    T1 = bench_best(?RUNS, fun () -> loop_compiled(Hashes) end),
    report(Label, "compiled (status quo)", T1),

    ok = marina_ring_pt:build(SortedTokens),
    warmup(fun () -> loop_pt(Hashes) end),
    T2 = bench_best(?RUNS, fun () -> loop_pt(Hashes) end),
    report(Label, "persistent_term tuple", T2),

    ok = marina_ring_bin:build(SortedTokens),
    warmup(fun () -> loop_bin(Hashes) end),
    T3 = bench_best(?RUNS, fun () -> loop_bin(Hashes) end),
    report(Label, "persistent_term binary", T3),

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
    {Nodes, SortedTokens, Hashes}.

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
