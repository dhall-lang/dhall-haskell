# ADR 003: Experimental DAG CBOR

Status: accepted as an *experimental* codec. Not the default semantic hash.

## Context

Today the semantic integrity hash is SHA-256 of the **legacy tree** CBOR of an
αβ-normal expression. A hash-consed `CoreGraph` can encode each unique node
once and refer to children by index, shrinking large shared normal forms, but
changing the normative bytes changes every `sha256:…` hash.

## Decision

- Envelope: `["Dhall-DAG", 1, nodes, root]` (CBOR array, version `1`).
- Each `nodes[i]` uses the existing expression CBOR labels. Expression-valued
  fields are node references encoded as `Var "_" n` meaning `nodes[n]`.
- Canonicalization: re-intern with sharing on, then number nodes by
  depth-first postorder over canonical child order. The root is last. Bytes
  must not depend on intern sharing mode, allocation order, or arena ids.
- Child-before-parent is required: a reference `n` in node `i` must satisfy
  `n < i`. The decoder rejects empty tables, trailing data, unknown versions,
  out-of-range roots, forward/self references, and node counts above a bound.
- Default `hashExpression` / freeze / CLI hash paths keep **legacy tree CBOR**.
  `encodeGraph` / `decodeGraph` are opt-in (`Dhall.Core.DAG.CBOR`).
  `hashExpressionGraph` hashes the legacy encoding of `toSharedExpr`.

## Consequences

- Independent of the dhall-lang pin: this is a Haskell prototype plus this
  ADR. A standard `binary.md` / `imports.md` change lands only with the
  coordinated Phase 8 switch.
- Encode/decode tests compare sharing on/off bytes, round-trip denoted trees,
  and re-interned graphs. They are not a second-language oracle.
