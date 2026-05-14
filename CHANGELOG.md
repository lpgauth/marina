# Changelog

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
