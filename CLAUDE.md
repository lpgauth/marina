# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

`marina` is a high-performance Erlang CQL client for Cassandra / Scylla. It speaks CQL binary protocol v3 directly over TCP, implemented on top of [shackle](https://github.com/lpgauth/shackle) for the connection pool and request/response pipelining.

## Common commands

The `Makefile` delegates to `rebar3` (bundled at `./bin/rebar3` as a fallback). Standard targets:

- `make compile` — compile with strict warnings (`warnings_as_errors`, `warn_missing_spec`, etc.) via the `compile` profile.
- `make xref` — cross-reference checks (undefined calls, locals not used, deprecated calls).
- `make eunit` — run EUnit suite with coverage.
- `make dialyzer` — typecheck.
- `make test` — runs `xref eunit dialyzer` together (what CI runs).
- `make profile` — full fprofx profiling pass; calls `marina_profile:fprofx()` and opens qcachegrind.

EUnit tests talk to a real Scylla/Cassandra node at `172.18.0.2:9042` (see `test/marina_tests.erl`). CI (`.github/workflows/erlang.yml`) runs inside `lpgauth/erlang-scylla:26.2.2-5.2-amd64` and boots Scylla via `python3 /docker-entrypoint.py` before tests. The bundled `Dockerfile` (Scylla 5.2 + OTP 26.2.3) is the matching local image.

Running a single test: `./bin/rebar3 eunit --module=marina_tests --test=<test_name>` (e.g. `query_subtest`). Most test functions are defined as `_subtest/0` helpers invoked from the `marina_test_/0` generator under `{inparallel, ...}`.

## Architecture

### Startup / supervision

`marina_app` → `marina_sup` (one_for_one) → `marina_pool_server` (a `metal` gen_server-like process). `marina_sup:init/1` also initializes two registries before starting children:

- `marina_cache:init/0` — named public ETS table `marina_cache` for prepared-statement IDs, keyed by `{Pool, QueryBinary}`.
- `marina_pool:init/0` — a `foil` table (ETS + compiled module) that holds the current strategy and the `{node, N}` → `NodeId` mapping.

`marina_pool_server` bootstraps on first message: it dials each IP in `bootstrap_ips` in order, runs `system.local` + `system.peers` queries (see `marina_pool_server:peers_query/1`), filters peers to the bootstrap node's datacenter, then calls `marina_pool:start/2` to spin up one `shackle` pool per node. If bootstrap fails it retries every 500ms.

### Request path

All public entry points live in `marina.erl` (`query/2`, `async_query/2`, `reusable_query/2`, `async_reusable_query/2`). Each one:

1. Extracts `routing_key` from `QueryOpts` via `marina_utils:query_opts/2`.
2. Calls `marina_pool:node(RoutingKey)` to pick a pool.
3. Dispatches to `shackle:call` / `shackle:cast` against that pool.

`marina_client` is the `shackle_client` behaviour implementation: `setup/2` handles STARTUP / authenticate / USE keyspace; `handle_request/2` assigns a stream id (`Requests rem ?MAX_STREAM_ID`) and serializes via `marina_request`; `handle_data/2` accumulates bytes in a `marina_buffer` and decodes zero or more frames per packet. Response bodies are decoded *in the caller process* via `marina:response/1` → `marina_body:decode/1` so the client process stays lean.

Reusable queries cache the prepared statement id per pool in the `marina_cache` ETS table. On Cassandra error 9472 ("prepared statement not found"), the cache entry is evicted and the call retries within the remaining timeout budget (`marina_utils:timeout/2`).

### Token-aware routing (the interesting bit)

With `strategy = token_aware` (the default), `marina_pool:start/2` calls `marina_ring:build/1` with the `{RpcAddress, Tokens}` list from bootstrap. `marina_ring:build/1`:

1. Flattens each node's token list into `{Token :: integer(), RpcAddress}` pairs and sorts them.
2. Builds a balanced binary search tree (`build_tree/2`) keyed on token value, wrapping around at the top with the first node as the "fallthrough."
3. Calls `marina_compiler:ring_utils/1`, which **generates an Erlang module `marina_ring_utils` at runtime** (via `erl_syntax` → `compile:forms` → `code:load_binary`) whose single exported function `lookup/1` is a nested `case` expression that performs the token range lookup in O(log n) with zero allocations per query.

`marina_ring:lookup/1` then hashes the routing key with Murmur3 (`marina_token:m3p/1`) and delegates to this compiled module. This is why `marina_compiler` and `marina_ring` have `-ignore_xref`/`-dialyzer` attributes for `marina_ring_utils:lookup/1` — the module does not exist on disk.

When modifying ring logic, remember: changes to the shape of the tree or the generated clauses must happen in `marina_compiler:tree_lookup_body/3`, not in a static `marina_ring_utils.erl` (there isn't one).

### Pool naming

`marina_pool:node_id/1` converts an `<<A, B, C, D>>` RPC address into the atom `marina_A.B.C.D`. This atom is both the `shackle_pool` name and what `marina_cache:erase_server/1` uses (via `marina_utils:server_to_pool/1`) to wipe cached prepared statements when a server dies.

## Dependencies worth knowing

- **shackle** — connection pool, request pipelining, stream id management. Marina's `marina_client` is a `shackle_client`.
- **foil** — compile-time-generated ETS lookup table used for pool strategy / node indexing.
- **metal** — minimal gen_server alternative used by `marina_pool_server`.
- **granderl** — random number generation.
- **murmur** — Murmur3 hash used by `marina_token:m3p/1` for token hashing.
- **lz4** — optional frame compression (enabled by the `compression` app env).

## Configuration

All runtime config is via the `marina` application env (see `include/marina_internal.hrl` for defaults). Key ones: `bootstrap_ips`, `keyspace`, `pool_size`, `strategy` (`random` | `token_aware`), `compression`, `reconnect_time_{min,max}`, `socket_options`.

## Conventions

- Strict compile warnings are enabled under the `compile` profile — any new code must have `-spec` annotations and no unused vars/imports; the build treats warnings as errors.
- Records and types are in `include/marina.hrl` (public) and `include/marina_internal.hrl` (internal macros + defaults). Add new type aliases to `marina.hrl` only if they appear in public specs.
- Cover is excluded for `marina_profile` and `marina_tests` (see `rebar.config`).
