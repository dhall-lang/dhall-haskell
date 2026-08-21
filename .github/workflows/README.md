# GitHub Actions

- [`main.yml`](main.yml) — build and test matrix on pull requests
- [`renovatebot.yml`](renovatebot.yml) — scheduled Renovate
- [`bench.yml`](bench.yml) — `dhall` package benchmarks (below)

## Benchmarks

[`bench.yml`](bench.yml) runs the `dhall` tasty-bench suites on GitHub-hosted
Ubuntu (`stack.yaml` only). It is a separate job from the PR test matrix and is
not a merge gate.

### When it runs

| Trigger | What happens |
|---------|----------------|
| Push to `main` | Full suite (`dhall-parser`, `deep-nested-large-record`, `evaluation`). Results are appended to the chart history. |
| **Actions → benchmarks → Run workflow** | Same job. Optional `pattern` input is passed to tasty-bench (e.g. `large3`). Charts update only if the selected branch is `main`. |

A manual run on a feature branch still produces logs, a job summary, and a
downloadable artifact. It does **not** write to `gh-pages`, so experiment
commits do not pollute the time series.

Until this workflow file exists on `main`, use the branch selector in the
Actions UI to dispatch it from this branch.

### Where results live

Durable history is the **`gh-pages`** branch, not Actions artifacts.

Each published run converts tasty-bench CSV to JSON and
[github-action-benchmark](https://github.com/benchmark-action/github-action-benchmark)
commits one data point per benchmark (tagged with the git SHA) under
`dev/bench/`. Hover a chart point for commit, date, and value; click it to open
the commit.

After the first successful `main` run (which creates `gh-pages` if needed):

1. Repo **Settings → Pages → Deploy from a branch → `gh-pages` / (root)**
2. Charts: `https://<owner>.github.io/<repo>/dev/bench/`

The workflow also uploads `parser.csv` / `evaluation.csv` / `bench.json` as an
artifact named `dhall-benchmarks-<sha>` for 14 days. That is a debug dump, not
the archive.

### CSV → JSON

tasty-bench has no native github-action-benchmark parser. The workflow writes
`--csv` from each suite, then
[`tasty-bench-csv-to-json.py`](../scripts/tasty-bench-csv-to-json.py) maps

```text
Name,Mean (ps),2*Stdev (ps)
All.large3.evaluation,30000000000000,1200000000000
```

to `customSmallerIsBetter` JSON (picoseconds → milliseconds, strip a leading
`All.`):

```json
[{"name": "large3.evaluation", "unit": "ms", "value": 30000, "range": "1200"}]
```

### Local equivalent

```sh
stack bench dhall:evaluation --benchmark-arguments '--csv evaluation.csv'
python3 .github/scripts/tasty-bench-csv-to-json.py evaluation.csv -o bench.json
```

See [`dhall/benchmark/evaluation/README.md`](../../dhall/benchmark/evaluation/README.md)
for the evaluation harness, `--pattern`, and how prep vs timed phases differ
from a cold `dhall --file` run.

### Caveats

- GitHub-hosted runners are noisy. Treat small deltas as uninteresting; the
  workflow does not fail the job on apparent regressions (`fail-on-alert: false`).
- Each individual tasty-bench item is capped at 300 seconds (`--timeout 300`);
  the job is capped at 180 minutes.
- Numbers are the harness phase timings (see the evaluation README), not wall
  time of `run.sh`.
