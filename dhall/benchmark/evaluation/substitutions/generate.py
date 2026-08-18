#!/usr/bin/env python3
"""Regenerate package.dhall (nested lets for substitution-shift probing)."""

from pathlib import Path

N = 8000
OUT = Path(__file__).with_name("package.dhall")

lets = "\n".join(f"let x{i} = 0" for i in range(N))

OUT.write_text(
    "-- Nested-let probe for Haskell-API substitutions.\n"
    "-- Binders are named x0..xN so they do not collide with UserType* keys.\n"
    "-- Cost without the shiftSubstitutions identity fast path is rebuilding\n"
    "-- the substitution map at every Let; as Source keeps this AST unnormalized.\n"
    "-- Regenerate with: python3 generate.py\n"
    f"{lets}\n"
    "in  0\n"
)
print(f"wrote {OUT} with {N} lets ({OUT.stat().st_size} bytes)")
