# Evaluation benchmark suite

`stack bench evaluation` measures Dhall parse / resolve / typecheck / evaluation
phases on a mix of synthetic and real-world fixtures. The harness lives in
`Main.hs`.

Fixture-specific notes live in each subdirectory (`large3/`, `large4/`, etc.).

## Running

```sh
cd dhall && stack bench evaluation
stack bench evaluation --ba '--pattern large3'
stack bench evaluation --ba '--pattern large4'
stack bench evaluation --ba '--list-tests --pattern large3'
```

Pattern matching is infix (tasty default): `--pattern get_config` matches
`large3.get_config.resolve`.

## CLI timings (`run.sh`)

Each fixture directory has a `run.sh` that times a plain `dhall --file …`
(or, for `k8s/`, `dhall` on the `.mkPod` expression). Run it from that
subdirectory. This is a full CLI evaluate (parse + resolve + typecheck +
normalize), not a phase split.

## Phase benchmarks

Used by: `normalize/*`, `large1`, `k8s`, `large3`, `large3.get_config`, and
`large4`.

| Step | What happens |
|------|----------------|
| **Prep** | Parse; resolve with disk caches on; typecheck once to reject broken fixtures |
| **`resolve` bench** | Re-resolve parsed AST with the semantic cache off (`IgnoreSemanticCache`) |
| **`typecheck` bench** | Type-check the **pre-resolved AST from prep** |
| **`evaluation` bench** | Normalize the **pre-resolved AST from prep** |

Implications:

- `resolve` is not a fully cold import load. Prep may warm the semisemantic
  cache, so re-resolve can be faster than a CLI run with an empty cache.
- `typecheck` / `evaluation` measure only that phase on an already-resolved
  tree. They do not include import-loading cost.

## Fixture index

| Group | Directory | Purpose |
|-------|-----------|---------|
| `normalize.*` | `normalize/` | NbE on small resolved terms |
| `large1` | `large1/` | Medium import tree (includes a parse bench) |
| `large2` | `large2/` | CBOR encode/decode |
| `k8s.*` | `k8s/` | Real k8s schema imports |
| `large3` | `large3/` | Sourcegraph-scale Code pipeline (~193 MB NF) |
| `large3.get_config` | `large3/` | Small projection; still walks the full graph |
| `large4` | `large4/` | Medium customization tree |

## Related docs

- `large3/README.md` — Sourcegraph fixture and get_config notes
- `large4/README.md` — customization tree
