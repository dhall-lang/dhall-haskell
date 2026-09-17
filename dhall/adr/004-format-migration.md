# ADR 004: Semantic-hash and cache migration (DAG CBOR)

Status: accepted as the *plan* for a future default-format switch. **This
implementation does not flip defaults.**

## Context

Switching the normative CBOR from a tree to a DAG envelope changes every
semantic hash even though expression semantics are unchanged. Old cache
entries and frozen `sha256:…` imports would otherwise be misread as vNext.

## Decision

Keep shipping:

- Legacy tree CBOR as the only integrity hash (`hashExpression`, freeze, CLI).
- Existing semantic-cache layout and the current semisemantic namespace
  (`dhall-haskell-v2`).

When a future release *does* make DAG CBOR the default, do all of the
following in one coordinated standard major version:

1. Bump the language standard version and rewrite `binary.md` / `imports.md`.
2. Recompute every semantic-hash golden; provide a refreeze procedure
   (`dhall freeze --all` over existing imports) so frozen hashes are rewritten.
3. Put new semantic and semisemantic payloads in a **new** cache directory
   (candidate: `dhall-haskell-v3`) so v2 entries cannot masquerade as vNext.
   Integrity checking in vNext hashes only the new bytes.
4. Update `Dhall.Binary`, `Dhall.Import`, `Dhall.Freeze`, CLI encode/decode/hash,
   the standard executable, and all parser/binary/import/hash goldens together.
5. Cross-check the format with at least one independent implementation before
   release. Internal encode→decode round trips are not enough.

Until that release, experimental DAG CBOR may be used for measurement and
tooling. It must not be accepted as an equivalent integrity hash in the
standard mode.

## Refreeze procedure (when the switch ships)

1. Install the release that emits DAG CBOR by default.
2. For each package or config tree: `dhall freeze --all` (or the equivalent
   recursive freeze) so import integrity checks match vNext hashes.
3. Drop or ignore `~/.cache/dhall-haskell-v2` after confirming v3 is populated;
   do not copy v2 files into v3.
4. Re-run import and semantic-hash test suites; they are expected to change
   bytes, not decoded expressions.
