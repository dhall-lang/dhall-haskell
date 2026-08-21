#!/usr/bin/env python3
"""Convert tasty-bench --csv output to github-action-benchmark JSON.

tasty-bench writes:

    Name,Mean (ps),2*Stdev (ps)
    All.large3.evaluation,30000000000000,1200000000000

github-action-benchmark's customSmallerIsBetter tool expects:

    [{"name": "large3.evaluation", "unit": "ms", "value": 30000, "range": "1200"}]
"""

from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

PS_PER_MS = 1e9
ALL_PREFIX = "All."


def row_to_entry(row: Dict[str, str]) -> Optional[Dict[str, Any]]:
    name = (row.get("Name") or "").strip()
    mean = (row.get("Mean (ps)") or "").strip()
    stdev = (row.get("2*Stdev (ps)") or "").strip()
    if not name or not mean:
        return None
    if name.startswith(ALL_PREFIX):
        name = name[len(ALL_PREFIX) :]
    mean_ms = float(mean) / PS_PER_MS
    entry: Dict[str, Any] = {
        "name": name,
        "unit": "ms",
        "value": mean_ms,
    }
    if stdev:
        range_ms = float(stdev) / PS_PER_MS
        entry["range"] = f"{range_ms:.6g}"
        entry["extra"] = f"2*Stdev = {range_ms:.6g} ms"
    return entry


def convert(paths: Iterable[Path]) -> List[Dict[str, Any]]:
    entries: List[Dict[str, Any]] = []
    for path in paths:
        with path.open(newline="") as handle:
            for row in csv.DictReader(handle):
                entry = row_to_entry(row)
                if entry is not None:
                    entries.append(entry)
    return entries


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "csv_files",
        nargs="+",
        type=Path,
        help="tasty-bench CSV files (later files are appended)",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        required=True,
        help="github-action-benchmark JSON output path",
    )
    args = parser.parse_args()

    missing = [path for path in args.csv_files if not path.is_file()]
    if missing:
        for path in missing:
            print(f"error: CSV file not found: {path}", file=sys.stderr)
        return 1

    entries = convert(args.csv_files)
    args.output.write_text(json.dumps(entries, indent=2) + "\n")
    print(f"Wrote {len(entries)} benchmarks to {args.output}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
