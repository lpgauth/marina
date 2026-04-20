# marina

High-performance Erlang client for [Apache Cassandra](https://cassandra.apache.org/) and [ScyllaDB](https://www.scylladb.com/), speaking the CQL binary protocol directly over TCP.

[![Erlang CI](https://github.com/lpgauth/marina/actions/workflows/erlang.yml/badge.svg)](https://github.com/lpgauth/marina/actions/workflows/erlang.yml)

## Features

- **CQL native protocol v4** — covers queries, prepared statements, batches, paging, and server-side events.
- **Batch queries** — LOGGED / UNLOGGED / COUNTER, mixing raw and prepared statements.
- **Prepared statement cache** — per-pool, keyed on the query binary.
- **Load-balancing** — `random` or `token_aware` (Murmur3-hash routing keys to the owning replica).
- **Topology awareness** — a persistent control connection subscribes to `TOPOLOGY_CHANGE` events and drives a ring re-sync on any membership change. No polling.
- **LZ4 compression** — optional, opt-in via `compression`.
- **Authentication** — plaintext `PasswordAuthenticator`.

## Requirements

- Cassandra 2.1+ / ScyllaDB 2.x+
- Erlang/OTP 24+

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {marina, {git, "https://github.com/lpgauth/marina.git", {branch, "master"}}}
]}.
```

## Configuration

All settings are read from the `marina` application env.

| Name                 | Type                          | Default                    | Description                                                               |
| -------------------- | ----------------------------- | -------------------------- | ------------------------------------------------------------------------- |
| `backlog_size`       | `pos_integer()`               | `1024`                     | Per-connection [shackle](https://github.com/lpgauth/shackle) backlog.     |
| `bootstrap_ips`      | `[string()]`                  | `["127.0.0.1"]`            | IPs tried in order until one responds with `system.peers`.                |
| `compression`        | `boolean()`                   | `false`                    | Negotiate LZ4 compression on every connection.                            |
| `keyspace`           | `undefined` \| `binary()`     | `undefined`                | Default keyspace; issued as `USE …` after startup.                        |
| `password`           | `binary()`                    | `undefined`                | Password for `PasswordAuthenticator`.                                     |
| `pool_size`          | `pos_integer()`               | `16`                       | Number of shackle connections per node.                                   |
| `pool_strategy`      | `random` \| `round_robin`     | `random`                   | Shackle's strategy for picking a connection from a per-node pool.         |
| `port`               | `pos_integer()`               | `9042`                     | Server port.                                                              |
| `reconnect`          | `boolean()`                   | `true`                     | Auto-reconnect closed connections.                                        |
| `reconnect_time_max` | `pos_integer()` \| `infinity` | `120000`                   | Upper bound on the reconnect backoff (ms).                                |
| `reconnect_time_min` | `pos_integer()`               | `1500`                     | Lower bound on the reconnect backoff (ms).                                |
| `socket_options`     | `[gen_tcp:option()]`          | see `marina_internal.hrl`  | `gen_tcp` options applied to every connection.                            |
| `strategy`           | `random` \| `token_aware`     | `token_aware`              | Node selection: random, or Murmur3-hash the `routing_key` to the replica. |
| `username`           | `binary()`                    | `undefined`                | Username for `PasswordAuthenticator`.                                     |

## Usage

Start the application, then call the query API:

```erlang
1> marina_app:start().
{ok, [granderl, metal, shackle, foil, marina, ...]}

2> marina:query(<<"SELECT id, name FROM users LIMIT 1">>, #{timeout => 1000}).
{ok, {result, _Metadata, 1, [[<<"…">>, <<"alice">>]]}}

3> marina:query(<<"SELECT * FROM users WHERE id = ?">>,
                #{values      => [Uuid],
                  routing_key => Uuid,
                  timeout     => 1000}).
{ok, {result, _Metadata, 1, [Row]}}

4> marina:reusable_query(<<"SELECT * FROM users WHERE id = ?">>,
                         #{values => [Uuid], timeout => 1000}).
{ok, {result, _Metadata, 1, [Row]}}

5> marina:batch([
        {query, <<"INSERT INTO kv (k, v) VALUES (1, 'a')">>, []},
        {query, <<"INSERT INTO kv (k, v) VALUES (2, 'b')">>, []}
   ], #{batch_type => logged, timeout => 1000}).
{ok, undefined}
```

### API surface

Synchronous:
- `marina:query/2` — raw CQL query.
- `marina:reusable_query/2` — prepares the query once per pool, caches the statement id, executes with bound values thereafter.
- `marina:batch/2` — LOGGED, UNLOGGED, or COUNTER batch mixing raw and prepared statements.

Asynchronous (return a `shackle:request_id()`, consume via `marina:receive_response/1`):
- `marina:async_query/2`
- `marina:async_reusable_query/2`
- `marina:async_batch/2`

### Query options

`query_opts()` is a map; unknown keys are ignored.

| Key                 | Type                               | Default                   |
| ------------------- | ---------------------------------- | ------------------------- |
| `batch_type`        | `logged` \| `unlogged` \| `counter`| `logged`                  |
| `consistency_level` | `?CONSISTENCY_*`                   | `?CONSISTENCY_ONE`        |
| `page_size`         | `pos_integer()`                    | unset                     |
| `paging_state`      | `binary()`                         | unset                     |
| `pid`               | `pid()`                            | `self()`                  |
| `routing_key`       | `integer()` \| `binary()`          | `undefined`               |
| `skip_metadata`     | `boolean()`                        | `false`                   |
| `timeout`           | `pos_integer()`                    | `1000`                    |
| `values`            | `[binary()]`                       | `undefined`               |

## Architecture notes

- **Bootstrap.** On application start, `marina_pool_server` dials each `bootstrap_ip` until one responds, runs `SELECT rpc_address, data_center, tokens FROM system.{local,peers}`, filters by the seed node's datacenter, and starts one [`shackle`](https://github.com/lpgauth/shackle) pool per peer.
- **Token-aware routing.** With `strategy = token_aware`, `marina_ring:build/1` builds a balanced BST keyed on token intervals and `marina_compiler` compiles it to a `marina_ring_utils:lookup/1` function at runtime. Murmur3-hashed routing keys traverse the tree in O(log N).
- **Topology refresh.** `marina_control` owns a dedicated CQL connection that `REGISTER`s for `TOPOLOGY_CHANGE`, `STATUS_CHANGE`, and `SCHEMA_CHANGE`. On any topology event (and on every reconnect, as an anti-missed-event guard) it re-runs `system.peers` and posts `{topology_full_sync, Nodes}` to `marina_pool_server`, which diffs against the current pool set, starts/stops shackle pools as needed, clears prepared-statement caches for removed nodes, and rebuilds the ring.

## Development

```sh
make compile     # rebar3 compile with strict warnings
make xref        # cross-reference analysis
make dialyzer    # success typing
make eunit       # unit + integration tests (expects ScyllaDB at 172.18.0.2:9042)
make test        # xref + eunit + dialyzer
make bench       # ring-lookup micro-benchmark
```

The bundled `Dockerfile` produces the CI image (`lpgauth/erlang-scylla:28.3.1-6.2.3-amd64`) by layering OTP 28.3.1 (from the official `erlang` image) on top of `scylladb/scylla:6.2.3`. For local integration tests, run that image or a plain ScyllaDB container on `172.18.0.2:9042`.

## License

MIT — see [LICENSE](LICENSE).
