# Changelog

## 0.4.5

### Changed

- `error/0` is now `{error, error_reason()}` where `error_reason/0`
  is a documented sum type — was `{error, term()}`, which gave
  dialyzer nothing to check at call sites. The sum covers:

  - `marina_pool_not_started` and `timeout` (marina-level)
  - `cql_error()` — `{Code :: pos_integer(), Msg :: binary()}`, the
    Cassandra/Scylla server-side error tuple
  - `no_server`, `pool_not_started`, `shackle_not_started` (shackle
    errors that propagate through marina)

  `cql_error/0` is exported as a public type so callers can pattern-
  match against it cleanly.

- `marina_body:decode/1` spec corrected: was `{error, atom()}`,
  is actually `{error, cql_error()}` — the `{Code, Msg}` shape was
  documented but the spec contradicted it. Real bug; dialyzer
  was silently accepting `atom()` because no caller was destructuring.

- Four `marina_types` spec corrections (real correctness bugs, not
  just imprecision):

  - `decode_short_bytes/1` and `decode_string/1` can return
    `{null, binary()}` on the 0xFFFF sentinel; specs claimed
    `{binary(), binary()}`.
  - `encode_bytes/1` and `encode_short_bytes/1` accept `null` as
    input (encoded as the sentinel); specs claimed `binary()` only.

  No behavioural change — the functions handled `null` correctly
  already, the specs just lied about it.

No source changes beyond the type tightening; existing callers
continue to work unchanged. Dialyzer now flags `{error, typo}`
at call sites that don't match the sum.

## 0.4.4

### Added

- Two telemetry events at the request boundary:

  | Event | Measurements | Metadata |
  |---|---|---|
  | `[marina, request, sent]`  | `count => 1` | `operation, pool, async` |
  | `[marina, request, error]` | `count => 1` | `operation, reason` |

  `sent` fires at each shackle dispatch — once per query/batch/prepare/
  execute. `reusable_query` with a cache miss fires twice (`prepare`
  then `execute`), giving an accurate count of CQL ops. The `error`
  event fires when `marina_pool:node/1` returns no pool (e.g.
  `marina_pool_not_started`). Attach handlers via `telemetry:attach/4`.

  Per-request shackle lifecycle (queue / send / receive) remains
  observable via shackle's own telemetry — marina's events surface
  the CQL-level intent without duplicating that work.

- `telemetry` (1.4.2) is now a direct dependency (was already
  transitively present via shackle).

- `vsn` in `marina.app.src` is now an explicit string (`"0.4.4"`) —
  was `git`, which only works when built from a checkout.

No source or API changes beyond the instrumentation.

## 0.4.3

### Changed

- Replaced `lz4` git ref (`lpgauth/erlang-lz4`) with the new
  in-house **`lz4_nif`** hex package (0.1.1). `marina_utils:pack/1'
  and `unpack/1' call `lz4_nif:compress/2' and `lz4_nif:uncompress/2'
  respectively — same API, same return shape.
- Replaced `murmur` git ref (`lpgauth/murmur`) with the new in-house
  **`murmur_nif`** hex package (0.1.0). `marina_token:m3p/1' calls
  `murmur_nif:murmur3_cassandra_x64_128/1'. Byte-for-byte compatible
  with the legacy fork (verified across inputs with high-byte
  sequences where the signed/unsigned variant difference matters);
  same token integers, no routing changes.

Both new NIFs follow the same standards as the rest of the
ecosystem: vendored upstream source, raw NIF (no rustler runtime),
dirty CPU scheduler for inputs above 20 KB, `enif_consume_timeslice`
accounting on the inline path, hex publish config, CI matrix
covering OTP 25-28, MIT license for the wrapper plus the upstream
license for the vendored code.

With this swap marina's only remaining non-hex deps are the
plumbing inside its test profile (fprofx git ref).

## 0.4.2

Infrastructure refresh: dependency bumps + docs migration.

### Changed

- Bumped `shackle` from git ref `0.6.20` to hex `0.7.1`. shackle
  0.7.1 replaces granderl with knot (in-house C NIF), so marina's
  transitive granderl dependency is gone — fixing the OTP 27+ build
  break that affected anything depending on `granderl 0.1.5` from
  hex.pm.
- Bumped `foil` from git ref `0.1.3` to hex `0.1.4` (tightened
  `error/0` type, internal DRY refactor; behaviour unchanged).
- `fprofx` test-profile dep moved from the `ransomr/fprofx` fork to
  `lpgauth/fprofx` (`otp_19` branch), matching the rest of the
  ecosystem.
- Documentation migrated from `edown` to `rebar3_ex_doc`.

### Removed

- Direct `granderl` dep entry (was unused by marina source — it sat
  in the dep tree only because the old shackle ref pulled it in).
