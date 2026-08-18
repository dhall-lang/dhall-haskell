#!/usr/bin/env python3
"""Regenerate slow/walk.dhall (large import-free List Natural walk probe)."""

from pathlib import Path

N = 30000
OUT = Path(__file__).with_name("walk.dhall")

OUT.write_text(
    "-- Import-free walk probe: large List Natural AST.\n"
    "-- Goal: ~0.5s per full structural traverse/denote after the file is parsed.\n"
    "-- Already beta-normal; cost is AST size, not Natural/fold evaluation.\n"
    "-- Regenerate with: python3 generate-walk.py\n"
    "[\n" + ", ".join(str(i) for i in range(N)) + "\n]\n"
)
print(f"wrote {OUT} with {N} elements ({OUT.stat().st_size} bytes)")
