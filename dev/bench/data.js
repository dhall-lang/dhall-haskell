window.BENCHMARK_DATA = {
  "lastUpdate": 1790785354236,
  "repoUrl": "https://github.com/dhall-lang/dhall-haskell",
  "entries": {
    "dhall": [
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ecf223aabe247aa50b2b7b9e61739f7e9c7150c0",
          "message": "fix laziness in VHLam vApp (#2831)",
          "timestamp": "2026-09-18T13:16:00Z",
          "tree_id": "dd2d8903a4d0f8215fe63d54e6fa5aaf5d25a6dd",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/ecf223aabe247aa50b2b7b9e61739f7e9c7150c0"
        },
        "date": 1789738581012,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052829018,
            "range": "0.00454682",
            "unit": "ms",
            "extra": "2*Stdev = 0.00454682 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053325665,
            "range": "0.00397024",
            "unit": "ms",
            "extra": "2*Stdev = 0.00397024 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.03258121,
            "range": "0.0016828",
            "unit": "ms",
            "extra": "2*Stdev = 0.0016828 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 354.1736894,
            "range": "7.01974",
            "unit": "ms",
            "extra": "2*Stdev = 7.01974 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017209586,
            "range": "0.00103622",
            "unit": "ms",
            "extra": "2*Stdev = 0.00103622 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 236.6829312,
            "range": "4.49323",
            "unit": "ms",
            "extra": "2*Stdev = 4.49323 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.04183915,
            "range": "0.00286672",
            "unit": "ms",
            "extra": "2*Stdev = 0.00286672 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 82.8821124,
            "range": "4.95204",
            "unit": "ms",
            "extra": "2*Stdev = 4.95204 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020719231,
            "range": "0.00144856",
            "unit": "ms",
            "extra": "2*Stdev = 0.00144856 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 207.9199398,
            "range": "3.06787",
            "unit": "ms",
            "extra": "2*Stdev = 3.06787 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.018264301,
            "range": "0.00100757",
            "unit": "ms",
            "extra": "2*Stdev = 0.00100757 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 174.7698082,
            "range": "7.53605",
            "unit": "ms",
            "extra": "2*Stdev = 7.53605 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.055580629,
            "range": "0.00553923",
            "unit": "ms",
            "extra": "2*Stdev = 0.00553923 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 162.6855566,
            "range": "3.17418",
            "unit": "ms",
            "extra": "2*Stdev = 3.17418 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.04894076,
            "range": "0.00271035",
            "unit": "ms",
            "extra": "2*Stdev = 0.00271035 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 194.4369788,
            "range": "7.78778",
            "unit": "ms",
            "extra": "2*Stdev = 7.78778 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000476291,
            "range": "3.3024e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.3024e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010148484,
            "range": "0.000801932",
            "unit": "ms",
            "extra": "2*Stdev = 0.000801932 ms"
          },
          {
            "name": "large1.parse",
            "value": 331.468897,
            "range": "11.6568",
            "unit": "ms",
            "extra": "2*Stdev = 11.6568 ms"
          },
          {
            "name": "large1.resolve",
            "value": 101.631182,
            "range": "3.80891",
            "unit": "ms",
            "extra": "2*Stdev = 3.80891 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 73.526753,
            "range": "3.15709",
            "unit": "ms",
            "extra": "2*Stdev = 3.15709 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 132.000498,
            "range": "8.23296",
            "unit": "ms",
            "extra": "2*Stdev = 8.23296 ms"
          },
          {
            "name": "large2.normalize",
            "value": 168.3771706,
            "range": "5.26958",
            "unit": "ms",
            "extra": "2*Stdev = 5.26958 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 332.5989386,
            "range": "22.2373",
            "unit": "ms",
            "extra": "2*Stdev = 22.2373 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 833.6395622,
            "range": "9.8952",
            "unit": "ms",
            "extra": "2*Stdev = 9.8952 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2755.10427055,
            "range": "126.408",
            "unit": "ms",
            "extra": "2*Stdev = 126.408 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 413.5776908,
            "range": "4.38524",
            "unit": "ms",
            "extra": "2*Stdev = 4.38524 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.648219498,
            "range": "0.0308523",
            "unit": "ms",
            "extra": "2*Stdev = 0.0308523 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1825.2608848,
            "range": "5.81182",
            "unit": "ms",
            "extra": "2*Stdev = 5.81182 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.8927992,
            "range": "8.4816",
            "unit": "ms",
            "extra": "2*Stdev = 8.4816 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.403153965,
            "range": "0.0271433",
            "unit": "ms",
            "extra": "2*Stdev = 0.0271433 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2778.4332666,
            "range": "217.709",
            "unit": "ms",
            "extra": "2*Stdev = 217.709 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5238.5301298,
            "range": "156.055",
            "unit": "ms",
            "extra": "2*Stdev = 156.055 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 1047.937893,
            "range": "81.3834",
            "unit": "ms",
            "extra": "2*Stdev = 81.3834 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2883.64734815,
            "range": "132.737",
            "unit": "ms",
            "extra": "2*Stdev = 132.737 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5533.0617372,
            "range": "136.484",
            "unit": "ms",
            "extra": "2*Stdev = 136.484 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.264513137,
            "range": "0.0252952",
            "unit": "ms",
            "extra": "2*Stdev = 0.0252952 ms"
          },
          {
            "name": "large4.resolve",
            "value": 551.2391234,
            "range": "12.2262",
            "unit": "ms",
            "extra": "2*Stdev = 12.2262 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 435.4659956,
            "range": "5.28796",
            "unit": "ms",
            "extra": "2*Stdev = 5.28796 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.466414337,
            "range": "0.124112",
            "unit": "ms",
            "extra": "2*Stdev = 0.124112 ms"
          },
          {
            "name": "large5.resolve",
            "value": 224.657112,
            "range": "9.06527",
            "unit": "ms",
            "extra": "2*Stdev = 9.06527 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 67.620404,
            "range": "4.33551",
            "unit": "ms",
            "extra": "2*Stdev = 4.33551 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.1871708,
            "range": "5.61788",
            "unit": "ms",
            "extra": "2*Stdev = 5.61788 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 844.3834358,
            "range": "34.9055",
            "unit": "ms",
            "extra": "2*Stdev = 34.9055 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000211714,
            "range": "1.1556e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.1556e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000196941,
            "range": "1.127e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.127e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 973.647633,
            "range": "32.568",
            "unit": "ms",
            "extra": "2*Stdev = 32.568 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.155608431,
            "range": "0.108244",
            "unit": "ms",
            "extra": "2*Stdev = 0.108244 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.21742855,
            "range": "0.215866",
            "unit": "ms",
            "extra": "2*Stdev = 0.215866 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.779188181,
            "range": "0.101727",
            "unit": "ms",
            "extra": "2*Stdev = 0.101727 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 487.9805026,
            "range": "14.9012",
            "unit": "ms",
            "extra": "2*Stdev = 14.9012 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 403.8684104,
            "range": "3.57115",
            "unit": "ms",
            "extra": "2*Stdev = 3.57115 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 328.7220342,
            "range": "4.45075",
            "unit": "ms",
            "extra": "2*Stdev = 4.45075 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 477.038225,
            "range": "5.31031",
            "unit": "ms",
            "extra": "2*Stdev = 5.31031 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 372.1247482,
            "range": "2.95434",
            "unit": "ms",
            "extra": "2*Stdev = 2.95434 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 92.8867379,
            "range": "2.94887",
            "unit": "ms",
            "extra": "2*Stdev = 2.94887 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1501.955064,
            "range": "11.067",
            "unit": "ms",
            "extra": "2*Stdev = 11.067 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 616.6509244,
            "range": "21.3394",
            "unit": "ms",
            "extra": "2*Stdev = 21.3394 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 219.0256124,
            "range": "4.72447",
            "unit": "ms",
            "extra": "2*Stdev = 4.72447 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 22.2867522,
            "range": "1.35185",
            "unit": "ms",
            "extra": "2*Stdev = 1.35185 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.790289231,
            "range": "0.136989",
            "unit": "ms",
            "extra": "2*Stdev = 0.136989 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 48.9215311,
            "range": "3.34235",
            "unit": "ms",
            "extra": "2*Stdev = 3.34235 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.531212212,
            "range": "0.109016",
            "unit": "ms",
            "extra": "2*Stdev = 0.109016 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 12.684045937,
            "range": "1.09444",
            "unit": "ms",
            "extra": "2*Stdev = 1.09444 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.143116996,
            "range": "0.0110686",
            "unit": "ms",
            "extra": "2*Stdev = 0.0110686 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 305.1578121,
            "range": "6.82414",
            "unit": "ms",
            "extra": "2*Stdev = 6.82414 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.2504072,
            "range": "1.12104",
            "unit": "ms",
            "extra": "2*Stdev = 1.12104 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 77.814297,
            "range": "4.45617",
            "unit": "ms",
            "extra": "2*Stdev = 4.45617 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 24.31857965,
            "range": "1.03988",
            "unit": "ms",
            "extra": "2*Stdev = 1.03988 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.004763722,
            "range": "0.00020289",
            "unit": "ms",
            "extra": "2*Stdev = 0.00020289 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 221.7700195,
            "range": "1.38291",
            "unit": "ms",
            "extra": "2*Stdev = 1.38291 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 209.1437398,
            "range": "7.09594",
            "unit": "ms",
            "extra": "2*Stdev = 7.09594 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 215.98553,
            "range": "5.90821",
            "unit": "ms",
            "extra": "2*Stdev = 5.90821 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.4407006,
            "range": "0.60552",
            "unit": "ms",
            "extra": "2*Stdev = 0.60552 ms"
          },
          {
            "name": "Line comment",
            "value": 404.731187,
            "range": "24.9434",
            "unit": "ms",
            "extra": "2*Stdev = 24.9434 ms"
          },
          {
            "name": "Block comment",
            "value": 368.8411908,
            "range": "34.9255",
            "unit": "ms",
            "extra": "2*Stdev = 34.9255 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.478065185,
            "range": "0.0257153",
            "unit": "ms",
            "extra": "2*Stdev = 0.0257153 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2220.7086984,
            "range": "59.797",
            "unit": "ms",
            "extra": "2*Stdev = 59.797 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dab41d6e81654ce14caea43d6c8a1af1b3c63a79",
          "message": "make VLam lazy when types are not bounded (#2832)\n\n* make VLam lazy when types are not bounded\n\n* Force VLam arguments at bounded binder types.\n\nKeep ChurchEval-style Natural/Bool loops CBV so lazy user-lambda\napplication does not build a thunk per step, while List and function\nbinders stay lazy for Church cons and Iterate.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* fix the laziness in VLam\n\n* Bang every Natural/fold accumulator to WHNF.\n\nA lazy fold on list/record accumulators was an overcorrection: Iterate\nand ListBench paid a thunk per step. List spines are Seq concatenations\nand do not force elements; the conv shortcut stays boundedType-only.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Force Natural/fold record fields to WHNF, not just the constructor.\n\nVRecordLit did not bang Dhall.Map inner maps, so IterateAlt and ListBench\nstill built thunk towers in next/rest records. Walk fields including list\nspines, without forcing list elements or lambda bodies.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Skip list-typed record fields in Natural/fold forcing.\n\nListBench still forces next : Natural. IterateAlt next : List _ stays a\nthunk so List/length of the outer spine does not build lists of length 1..n.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Keep forceAccWHNF out of line from eval.\n\nInlining the record-field walker into eval bloated the Natural/fold\nbranch and is a plausible cause of the large5 regression.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Classify record binders only one field-level deep.\n\nUnbounded whnfCheapType walks on VRecord made large4 pay a schema walk\non every apply. Nested records stay not cheap; flat products of\nprimitives and function types stay cheap.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-18T17:27:44Z",
          "tree_id": "48b76cbe0a31aebaa98fc0c7c351159bf0160ded",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/dab41d6e81654ce14caea43d6c8a1af1b3c63a79"
        },
        "date": 1789753094391,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052339334,
            "range": "0.00272681",
            "unit": "ms",
            "extra": "2*Stdev = 0.00272681 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053206362,
            "range": "0.00320201",
            "unit": "ms",
            "extra": "2*Stdev = 0.00320201 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033051603,
            "range": "0.00237225",
            "unit": "ms",
            "extra": "2*Stdev = 0.00237225 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 395.9126592,
            "range": "24.6534",
            "unit": "ms",
            "extra": "2*Stdev = 24.6534 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017216766,
            "range": "0.0014013",
            "unit": "ms",
            "extra": "2*Stdev = 0.0014013 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 305.8548872,
            "range": "4.65952",
            "unit": "ms",
            "extra": "2*Stdev = 4.65952 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045977852,
            "range": "0.00354964",
            "unit": "ms",
            "extra": "2*Stdev = 0.00354964 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 83.925685,
            "range": "4.33323",
            "unit": "ms",
            "extra": "2*Stdev = 4.33323 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.02065067,
            "range": "0.00149323",
            "unit": "ms",
            "extra": "2*Stdev = 0.00149323 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 262.501567,
            "range": "6.2765",
            "unit": "ms",
            "extra": "2*Stdev = 6.2765 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.018087898,
            "range": "0.00107538",
            "unit": "ms",
            "extra": "2*Stdev = 0.00107538 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 205.4711916,
            "range": "3.01061",
            "unit": "ms",
            "extra": "2*Stdev = 3.01061 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.055149343,
            "range": "0.00287186",
            "unit": "ms",
            "extra": "2*Stdev = 0.00287186 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 192.9995672,
            "range": "15.474",
            "unit": "ms",
            "extra": "2*Stdev = 15.474 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.04895631,
            "range": "0.00308411",
            "unit": "ms",
            "extra": "2*Stdev = 0.00308411 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 246.0367388,
            "range": "15.9485",
            "unit": "ms",
            "extra": "2*Stdev = 15.9485 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000499515,
            "range": "4.525e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.525e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010758309,
            "range": "0.000784478",
            "unit": "ms",
            "extra": "2*Stdev = 0.000784478 ms"
          },
          {
            "name": "large1.parse",
            "value": 338.426532,
            "range": "12.5103",
            "unit": "ms",
            "extra": "2*Stdev = 12.5103 ms"
          },
          {
            "name": "large1.resolve",
            "value": 99.4054273,
            "range": "2.06663",
            "unit": "ms",
            "extra": "2*Stdev = 2.06663 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 72.5007908,
            "range": "2.74161",
            "unit": "ms",
            "extra": "2*Stdev = 2.74161 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.1629964,
            "range": "3.00402",
            "unit": "ms",
            "extra": "2*Stdev = 3.00402 ms"
          },
          {
            "name": "large2.normalize",
            "value": 172.3830044,
            "range": "2.92568",
            "unit": "ms",
            "extra": "2*Stdev = 2.92568 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 336.6450634,
            "range": "12.8461",
            "unit": "ms",
            "extra": "2*Stdev = 12.8461 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 841.3843996,
            "range": "15.2568",
            "unit": "ms",
            "extra": "2*Stdev = 15.2568 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2763.41662265,
            "range": "172.1",
            "unit": "ms",
            "extra": "2*Stdev = 172.1 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 406.4850162,
            "range": "6.82154",
            "unit": "ms",
            "extra": "2*Stdev = 6.82154 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.668979106,
            "range": "0.0525666",
            "unit": "ms",
            "extra": "2*Stdev = 0.0525666 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1859.8792038,
            "range": "31.4528",
            "unit": "ms",
            "extra": "2*Stdev = 31.4528 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.6481746,
            "range": "3.58483",
            "unit": "ms",
            "extra": "2*Stdev = 3.58483 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.403183503,
            "range": "0.0244587",
            "unit": "ms",
            "extra": "2*Stdev = 0.0244587 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2866.11779085,
            "range": "129.976",
            "unit": "ms",
            "extra": "2*Stdev = 129.976 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4997.2670176,
            "range": "38.1138",
            "unit": "ms",
            "extra": "2*Stdev = 38.1138 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 711.4336234,
            "range": "15.2123",
            "unit": "ms",
            "extra": "2*Stdev = 15.2123 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2882.62994425,
            "range": "196.933",
            "unit": "ms",
            "extra": "2*Stdev = 196.933 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5368.898426,
            "range": "361.854",
            "unit": "ms",
            "extra": "2*Stdev = 361.854 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.258809073,
            "range": "0.0235496",
            "unit": "ms",
            "extra": "2*Stdev = 0.0235496 ms"
          },
          {
            "name": "large4.resolve",
            "value": 556.5875402,
            "range": "21.6384",
            "unit": "ms",
            "extra": "2*Stdev = 21.6384 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 438.1879382,
            "range": "8.30479",
            "unit": "ms",
            "extra": "2*Stdev = 8.30479 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.927032921,
            "range": "0.0766242",
            "unit": "ms",
            "extra": "2*Stdev = 0.0766242 ms"
          },
          {
            "name": "large5.resolve",
            "value": 222.4287198,
            "range": "5.47586",
            "unit": "ms",
            "extra": "2*Stdev = 5.47586 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.0300858,
            "range": "3.00689",
            "unit": "ms",
            "extra": "2*Stdev = 3.00689 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.4591842,
            "range": "2.89323",
            "unit": "ms",
            "extra": "2*Stdev = 2.89323 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 843.93308,
            "range": "36.2744",
            "unit": "ms",
            "extra": "2*Stdev = 36.2744 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000210531,
            "range": "2.0868e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.0868e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000208252,
            "range": "1.8826e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.8826e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 985.2593352,
            "range": "27.2887",
            "unit": "ms",
            "extra": "2*Stdev = 27.2887 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.210955762,
            "range": "0.117943",
            "unit": "ms",
            "extra": "2*Stdev = 0.117943 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.288586375,
            "range": "0.204959",
            "unit": "ms",
            "extra": "2*Stdev = 0.204959 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.772955462,
            "range": "0.106516",
            "unit": "ms",
            "extra": "2*Stdev = 0.106516 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 527.3763336,
            "range": "20.7131",
            "unit": "ms",
            "extra": "2*Stdev = 20.7131 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 482.809721,
            "range": "2.8052",
            "unit": "ms",
            "extra": "2*Stdev = 2.8052 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 393.003744,
            "range": "2.95903",
            "unit": "ms",
            "extra": "2*Stdev = 2.95903 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 484.432603,
            "range": "9.27219",
            "unit": "ms",
            "extra": "2*Stdev = 9.27219 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 374.1476594,
            "range": "3.00071",
            "unit": "ms",
            "extra": "2*Stdev = 3.00071 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 92.9803516,
            "range": "1.83967",
            "unit": "ms",
            "extra": "2*Stdev = 1.83967 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1527.057396,
            "range": "58.5542",
            "unit": "ms",
            "extra": "2*Stdev = 58.5542 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 612.9250926,
            "range": "9.46639",
            "unit": "ms",
            "extra": "2*Stdev = 9.46639 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 220.2901124,
            "range": "3.59516",
            "unit": "ms",
            "extra": "2*Stdev = 3.59516 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 22.19862,
            "range": "1.58151",
            "unit": "ms",
            "extra": "2*Stdev = 1.58151 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.834169318,
            "range": "0.158585",
            "unit": "ms",
            "extra": "2*Stdev = 0.158585 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 47.2279144,
            "range": "3.98218",
            "unit": "ms",
            "extra": "2*Stdev = 3.98218 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.517635312,
            "range": "0.114271",
            "unit": "ms",
            "extra": "2*Stdev = 0.114271 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 12.54531395,
            "range": "0.999563",
            "unit": "ms",
            "extra": "2*Stdev = 0.999563 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.144398188,
            "range": "0.00598494",
            "unit": "ms",
            "extra": "2*Stdev = 0.00598494 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 308.9303747,
            "range": "2.945",
            "unit": "ms",
            "extra": "2*Stdev = 2.945 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.4305215,
            "range": "2.94086",
            "unit": "ms",
            "extra": "2*Stdev = 2.94086 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 75.1137862,
            "range": "4.54695",
            "unit": "ms",
            "extra": "2*Stdev = 4.54695 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 24.4487862,
            "range": "1.73043",
            "unit": "ms",
            "extra": "2*Stdev = 1.73043 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.004927725,
            "range": "0.000472288",
            "unit": "ms",
            "extra": "2*Stdev = 0.000472288 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 219.63549555,
            "range": "3.53272",
            "unit": "ms",
            "extra": "2*Stdev = 3.53272 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 207.562385,
            "range": "11.0405",
            "unit": "ms",
            "extra": "2*Stdev = 11.0405 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 216.430887,
            "range": "6.72521",
            "unit": "ms",
            "extra": "2*Stdev = 6.72521 ms"
          },
          {
            "name": "Whitespace",
            "value": 21.957372075,
            "range": "1.39506",
            "unit": "ms",
            "extra": "2*Stdev = 1.39506 ms"
          },
          {
            "name": "Line comment",
            "value": 429.234276,
            "range": "22.0804",
            "unit": "ms",
            "extra": "2*Stdev = 22.0804 ms"
          },
          {
            "name": "Block comment",
            "value": 371.324782,
            "range": "29.0564",
            "unit": "ms",
            "extra": "2*Stdev = 29.0564 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.496432685,
            "range": "0.0274232",
            "unit": "ms",
            "extra": "2*Stdev = 0.0274232 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2223.8875306,
            "range": "35.8245",
            "unit": "ms",
            "extra": "2*Stdev = 35.8245 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d4d2e0f2e5da5b60d27afb16c45cfb83991780cc",
          "message": "update benchmark running scripts for dhall cli alone (#2835)",
          "timestamp": "2026-09-20T09:28:28Z",
          "tree_id": "8912d9d564125f3a585727cc85470609a40fdf71",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/d4d2e0f2e5da5b60d27afb16c45cfb83991780cc"
        },
        "date": 1789897795517,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052038761,
            "range": "0.0033177",
            "unit": "ms",
            "extra": "2*Stdev = 0.0033177 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053276472,
            "range": "0.00402273",
            "unit": "ms",
            "extra": "2*Stdev = 0.00402273 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.032986577,
            "range": "0.00198708",
            "unit": "ms",
            "extra": "2*Stdev = 0.00198708 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 411.8915284,
            "range": "4.37174",
            "unit": "ms",
            "extra": "2*Stdev = 4.37174 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017232098,
            "range": "0.0014954",
            "unit": "ms",
            "extra": "2*Stdev = 0.0014954 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 333.7312498,
            "range": "2.71802",
            "unit": "ms",
            "extra": "2*Stdev = 2.71802 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.04523912,
            "range": "0.00413166",
            "unit": "ms",
            "extra": "2*Stdev = 0.00413166 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 84.1890628,
            "range": "5.37039",
            "unit": "ms",
            "extra": "2*Stdev = 5.37039 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020114878,
            "range": "0.00173237",
            "unit": "ms",
            "extra": "2*Stdev = 0.00173237 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 261.428207,
            "range": "13.6619",
            "unit": "ms",
            "extra": "2*Stdev = 13.6619 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017782485,
            "range": "0.00150274",
            "unit": "ms",
            "extra": "2*Stdev = 0.00150274 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 207.3402702,
            "range": "12.2419",
            "unit": "ms",
            "extra": "2*Stdev = 12.2419 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.054012317,
            "range": "0.00379151",
            "unit": "ms",
            "extra": "2*Stdev = 0.00379151 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 189.7493438,
            "range": "8.04093",
            "unit": "ms",
            "extra": "2*Stdev = 8.04093 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047537928,
            "range": "0.00299286",
            "unit": "ms",
            "extra": "2*Stdev = 0.00299286 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 252.5631176,
            "range": "5.61824",
            "unit": "ms",
            "extra": "2*Stdev = 5.61824 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000501845,
            "range": "2.7676e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.7676e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010988776,
            "range": "0.0010643",
            "unit": "ms",
            "extra": "2*Stdev = 0.0010643 ms"
          },
          {
            "name": "large1.parse",
            "value": 337.7750988,
            "range": "4.02559",
            "unit": "ms",
            "extra": "2*Stdev = 4.02559 ms"
          },
          {
            "name": "large1.resolve",
            "value": 101.562626,
            "range": "7.63191",
            "unit": "ms",
            "extra": "2*Stdev = 7.63191 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 74.6471132,
            "range": "4.05961",
            "unit": "ms",
            "extra": "2*Stdev = 4.05961 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.8137944,
            "range": "5.40164",
            "unit": "ms",
            "extra": "2*Stdev = 5.40164 ms"
          },
          {
            "name": "large2.normalize",
            "value": 174.5000293,
            "range": "4.5141",
            "unit": "ms",
            "extra": "2*Stdev = 4.5141 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 340.2942172,
            "range": "12.6787",
            "unit": "ms",
            "extra": "2*Stdev = 12.6787 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 831.600551,
            "range": "56.6621",
            "unit": "ms",
            "extra": "2*Stdev = 56.6621 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2872.62709825,
            "range": "137.437",
            "unit": "ms",
            "extra": "2*Stdev = 137.437 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 414.931526,
            "range": "4.02772",
            "unit": "ms",
            "extra": "2*Stdev = 4.02772 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.67653551,
            "range": "0.0454453",
            "unit": "ms",
            "extra": "2*Stdev = 0.0454453 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1866.9793456,
            "range": "39.831",
            "unit": "ms",
            "extra": "2*Stdev = 39.831 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 95.9971164,
            "range": "3.32607",
            "unit": "ms",
            "extra": "2*Stdev = 3.32607 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.406285406,
            "range": "0.036735",
            "unit": "ms",
            "extra": "2*Stdev = 0.036735 ms"
          },
          {
            "name": "large3.resolve",
            "value": 3098.42626405,
            "range": "253.934",
            "unit": "ms",
            "extra": "2*Stdev = 253.934 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5428.2990214,
            "range": "294.49",
            "unit": "ms",
            "extra": "2*Stdev = 294.49 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 739.3314718,
            "range": "53.5096",
            "unit": "ms",
            "extra": "2*Stdev = 53.5096 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 3185.1755025,
            "range": "104.445",
            "unit": "ms",
            "extra": "2*Stdev = 104.445 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 6281.246939,
            "range": "270.057",
            "unit": "ms",
            "extra": "2*Stdev = 270.057 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.260308503,
            "range": "0.0153396",
            "unit": "ms",
            "extra": "2*Stdev = 0.0153396 ms"
          },
          {
            "name": "large4.resolve",
            "value": 555.8161612,
            "range": "8.43598",
            "unit": "ms",
            "extra": "2*Stdev = 8.43598 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 436.6214406,
            "range": "3.96098",
            "unit": "ms",
            "extra": "2*Stdev = 3.96098 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.896260159,
            "range": "0.060131",
            "unit": "ms",
            "extra": "2*Stdev = 0.060131 ms"
          },
          {
            "name": "large5.resolve",
            "value": 222.8429716,
            "range": "8.31121",
            "unit": "ms",
            "extra": "2*Stdev = 8.31121 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.6981312,
            "range": "5.14904",
            "unit": "ms",
            "extra": "2*Stdev = 5.14904 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.4717974,
            "range": "5.56634",
            "unit": "ms",
            "extra": "2*Stdev = 5.56634 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 830.3669926,
            "range": "13.4366",
            "unit": "ms",
            "extra": "2*Stdev = 13.4366 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000211404,
            "range": "2.0268e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.0268e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000204452,
            "range": "1.4734e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.4734e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 964.6660326,
            "range": "25.9074",
            "unit": "ms",
            "extra": "2*Stdev = 25.9074 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.21680959,
            "range": "0.100227",
            "unit": "ms",
            "extra": "2*Stdev = 0.100227 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.322208093,
            "range": "0.115857",
            "unit": "ms",
            "extra": "2*Stdev = 0.115857 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.771636131,
            "range": "0.109739",
            "unit": "ms",
            "extra": "2*Stdev = 0.109739 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 527.0276518,
            "range": "32.6151",
            "unit": "ms",
            "extra": "2*Stdev = 32.6151 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 492.3841416,
            "range": "5.21293",
            "unit": "ms",
            "extra": "2*Stdev = 5.21293 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 398.444256,
            "range": "7.72855",
            "unit": "ms",
            "extra": "2*Stdev = 7.72855 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 478.320242,
            "range": "4.09357",
            "unit": "ms",
            "extra": "2*Stdev = 4.09357 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 373.335127,
            "range": "14.1229",
            "unit": "ms",
            "extra": "2*Stdev = 14.1229 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 93.3364147,
            "range": "1.61643",
            "unit": "ms",
            "extra": "2*Stdev = 1.61643 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1483.8438446,
            "range": "40.8058",
            "unit": "ms",
            "extra": "2*Stdev = 40.8058 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 618.8884574,
            "range": "10.9822",
            "unit": "ms",
            "extra": "2*Stdev = 10.9822 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 220.3318624,
            "range": "2.81816",
            "unit": "ms",
            "extra": "2*Stdev = 2.81816 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 22.0537506,
            "range": "1.69007",
            "unit": "ms",
            "extra": "2*Stdev = 1.69007 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.790730393,
            "range": "0.130295",
            "unit": "ms",
            "extra": "2*Stdev = 0.130295 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 48.6360064,
            "range": "3.32799",
            "unit": "ms",
            "extra": "2*Stdev = 3.32799 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.533348787,
            "range": "0.107581",
            "unit": "ms",
            "extra": "2*Stdev = 0.107581 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 13.129799125,
            "range": "1.27055",
            "unit": "ms",
            "extra": "2*Stdev = 1.27055 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.143483832,
            "range": "0.0116987",
            "unit": "ms",
            "extra": "2*Stdev = 0.0116987 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 306.3059676,
            "range": "4.4342",
            "unit": "ms",
            "extra": "2*Stdev = 4.4342 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.5994069,
            "range": "1.6259",
            "unit": "ms",
            "extra": "2*Stdev = 1.6259 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 77.0704464,
            "range": "6.23772",
            "unit": "ms",
            "extra": "2*Stdev = 6.23772 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 23.5172393,
            "range": "2.29848",
            "unit": "ms",
            "extra": "2*Stdev = 2.29848 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.004643935,
            "range": "0.000284998",
            "unit": "ms",
            "extra": "2*Stdev = 0.000284998 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 225.1273463,
            "range": "12.2551",
            "unit": "ms",
            "extra": "2*Stdev = 12.2551 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 211.5239543,
            "range": "5.41382",
            "unit": "ms",
            "extra": "2*Stdev = 5.41382 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 203.798251,
            "range": "6.33612",
            "unit": "ms",
            "extra": "2*Stdev = 6.33612 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.0701143,
            "range": "1.94327",
            "unit": "ms",
            "extra": "2*Stdev = 1.94327 ms"
          },
          {
            "name": "Line comment",
            "value": 401.739738,
            "range": "9.79637",
            "unit": "ms",
            "extra": "2*Stdev = 9.79637 ms"
          },
          {
            "name": "Block comment",
            "value": 376.0867546,
            "range": "4.12029",
            "unit": "ms",
            "extra": "2*Stdev = 4.12029 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.47543227,
            "range": "0.0260284",
            "unit": "ms",
            "extra": "2*Stdev = 0.0260284 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2232.0701832,
            "range": "72.0862",
            "unit": "ms",
            "extra": "2*Stdev = 72.0862 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8a07699b6bfbf194c47968719991206496d73dc2",
          "message": "Improve parse error locations by committing after a cheap peek (#2836)\n\n* Improve parse error locations by committing after a cheap peek\n\nAvoid wrapping whole application arguments, list elements, and record\nfields in try so inner mistakes (missing space after :, bad escapes)\nare reported at the actual site instead of the outer bracket.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* fix golden test regression\n\n* fix stach.ghc-8.10\n\n* Fix list parsing and quoted Some binders\n\nConsume whitespace after list commas so multi-element lists parse again, and reject quoted Some as an identifier to match the language tests.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Revert dhall-lang pin and quoted Some language change\n\nThe parser-error-messages work should not change the language standard; keep only the list-whitespace parse fix.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Distinguish extra commas from missing commas in records\n\nReport an extra ',' in a record or list instead of the misleading \"Missing ','\" message from #2265.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Explain that Some cannot be annotated like a type\n\nWhen `Some` is followed by `:`, report that it is a constructor rather than a generic \"expecting argument\" parse error (#1654).\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Hint to quote keywords used as record labels\n\nBare keywords such as assert or if in a record field now suggest quoting them with backticks.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Report only an extra-comma error, not a missing-comma error as well\n\nMegaparsec was combining both alternatives when a record ended with ',,'.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Point EOF parse errors at the last real line\n\nAvoid reporting a fake empty line when input ends with a newline after an incomplete expression.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Give ParseError rewriting an explicit type for older GHCs\n\nGHC 8.10 and 9.2 inferred an illegal Token s1 ~ Token s2 constraint when reconstructing Megaparsec errors.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Keep the merge missing-argument hint at EOF\n\nLabel the whitespace after the first merge argument so both CLI and REPL report that a second argument is required.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Report leading zeros as invalid Naturals\n\nConsume the extra digits so the dedicated leading-zero message is not hidden by a later time/double parse error at the following space.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Cover more parse-error examples in unit tests\n\nAdd cases for nested escapes, list commas, lambda record patterns, keyword let, empty lists, bare Some, merge {=}, and record completion.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* fix Some EOF parsing\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-20T19:42:36Z",
          "tree_id": "8553daf511446a04221dcdd153de29a35298f68a",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/8a07699b6bfbf194c47968719991206496d73dc2"
        },
        "date": 1789933894650,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.038964381,
            "range": "0.00345609",
            "unit": "ms",
            "extra": "2*Stdev = 0.00345609 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.039414965,
            "range": "0.00384589",
            "unit": "ms",
            "extra": "2*Stdev = 0.00384589 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.022394728,
            "range": "0.00150111",
            "unit": "ms",
            "extra": "2*Stdev = 0.00150111 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 310.0735812,
            "range": "2.87656",
            "unit": "ms",
            "extra": "2*Stdev = 2.87656 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.010791775,
            "range": "0.000701932",
            "unit": "ms",
            "extra": "2*Stdev = 0.000701932 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 250.4683422,
            "range": "16.3515",
            "unit": "ms",
            "extra": "2*Stdev = 16.3515 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.022840976,
            "range": "0.00172254",
            "unit": "ms",
            "extra": "2*Stdev = 0.00172254 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 66.7090054,
            "range": "4.26678",
            "unit": "ms",
            "extra": "2*Stdev = 4.26678 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.00961075,
            "range": "0.00070405",
            "unit": "ms",
            "extra": "2*Stdev = 0.00070405 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 230.5794722,
            "range": "5.5175",
            "unit": "ms",
            "extra": "2*Stdev = 5.5175 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.008379816,
            "range": "0.00077842",
            "unit": "ms",
            "extra": "2*Stdev = 0.00077842 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 178.1858868,
            "range": "5.67428",
            "unit": "ms",
            "extra": "2*Stdev = 5.67428 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.027004136,
            "range": "0.00177067",
            "unit": "ms",
            "extra": "2*Stdev = 0.00177067 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 156.165862,
            "range": "6.50673",
            "unit": "ms",
            "extra": "2*Stdev = 6.50673 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.02289821,
            "range": "0.00181077",
            "unit": "ms",
            "extra": "2*Stdev = 0.00181077 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 197.8695524,
            "range": "11.7632",
            "unit": "ms",
            "extra": "2*Stdev = 11.7632 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000335782,
            "range": "2.8226e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.8226e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.008289581,
            "range": "0.0007423",
            "unit": "ms",
            "extra": "2*Stdev = 0.0007423 ms"
          },
          {
            "name": "large1.parse",
            "value": 202.1887438,
            "range": "4.52151",
            "unit": "ms",
            "extra": "2*Stdev = 4.52151 ms"
          },
          {
            "name": "large1.resolve",
            "value": 65.561099,
            "range": "2.4246",
            "unit": "ms",
            "extra": "2*Stdev = 2.4246 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 50.8470516,
            "range": "2.50248",
            "unit": "ms",
            "extra": "2*Stdev = 2.50248 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 100.0398186,
            "range": "3.66871",
            "unit": "ms",
            "extra": "2*Stdev = 3.66871 ms"
          },
          {
            "name": "large2.normalize",
            "value": 122.7284186,
            "range": "3.52201",
            "unit": "ms",
            "extra": "2*Stdev = 3.52201 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 263.2382116,
            "range": "2.76762",
            "unit": "ms",
            "extra": "2*Stdev = 2.76762 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 611.8610176,
            "range": "7.97617",
            "unit": "ms",
            "extra": "2*Stdev = 7.97617 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2103.4285005,
            "range": "105.806",
            "unit": "ms",
            "extra": "2*Stdev = 105.806 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 328.1850594,
            "range": "7.17405",
            "unit": "ms",
            "extra": "2*Stdev = 7.17405 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.505554015,
            "range": "0.0255427",
            "unit": "ms",
            "extra": "2*Stdev = 0.0255427 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1354.3538044,
            "range": "15.0196",
            "unit": "ms",
            "extra": "2*Stdev = 15.0196 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 78.6309524,
            "range": "6.95212",
            "unit": "ms",
            "extra": "2*Stdev = 6.95212 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.305114473,
            "range": "0.0222366",
            "unit": "ms",
            "extra": "2*Stdev = 0.0222366 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2180.64946515,
            "range": "114.249",
            "unit": "ms",
            "extra": "2*Stdev = 114.249 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3943.778712,
            "range": "8.50446",
            "unit": "ms",
            "extra": "2*Stdev = 8.50446 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 598.500493,
            "range": "10.4684",
            "unit": "ms",
            "extra": "2*Stdev = 10.4684 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2179.4503427,
            "range": "108.755",
            "unit": "ms",
            "extra": "2*Stdev = 108.755 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 4144.4315172,
            "range": "121.24",
            "unit": "ms",
            "extra": "2*Stdev = 121.24 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.189887323,
            "range": "0.0136536",
            "unit": "ms",
            "extra": "2*Stdev = 0.0136536 ms"
          },
          {
            "name": "large4.resolve",
            "value": 337.1950212,
            "range": "18.3831",
            "unit": "ms",
            "extra": "2*Stdev = 18.3831 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 318.4302236,
            "range": "11.7125",
            "unit": "ms",
            "extra": "2*Stdev = 11.7125 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.378787596,
            "range": "0.0629824",
            "unit": "ms",
            "extra": "2*Stdev = 0.0629824 ms"
          },
          {
            "name": "large5.resolve",
            "value": 139.9370682,
            "range": "5.29906",
            "unit": "ms",
            "extra": "2*Stdev = 5.29906 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 50.968771,
            "range": "4.44785",
            "unit": "ms",
            "extra": "2*Stdev = 4.44785 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 63.3993173,
            "range": "2.09877",
            "unit": "ms",
            "extra": "2*Stdev = 2.09877 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 628.6989664,
            "range": "5.05982",
            "unit": "ms",
            "extra": "2*Stdev = 5.05982 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000156112,
            "range": "1.4454e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.4454e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000147674,
            "range": "1.2478e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.2478e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 546.9330938,
            "range": "8.12994",
            "unit": "ms",
            "extra": "2*Stdev = 8.12994 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.875151684,
            "range": "0.0480584",
            "unit": "ms",
            "extra": "2*Stdev = 0.0480584 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.742214518,
            "range": "0.102028",
            "unit": "ms",
            "extra": "2*Stdev = 0.102028 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.299915125,
            "range": "0.0992037",
            "unit": "ms",
            "extra": "2*Stdev = 0.0992037 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 423.2610388,
            "range": "6.99056",
            "unit": "ms",
            "extra": "2*Stdev = 6.99056 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 380.996213,
            "range": "10.7439",
            "unit": "ms",
            "extra": "2*Stdev = 10.7439 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 313.8039108,
            "range": "4.50149",
            "unit": "ms",
            "extra": "2*Stdev = 4.50149 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 321.7496988,
            "range": "11.411",
            "unit": "ms",
            "extra": "2*Stdev = 11.411 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 179.254376,
            "range": "14.013",
            "unit": "ms",
            "extra": "2*Stdev = 14.013 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 66.30435,
            "range": "1.70626",
            "unit": "ms",
            "extra": "2*Stdev = 1.70626 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 850.8682502,
            "range": "3.51014",
            "unit": "ms",
            "extra": "2*Stdev = 3.51014 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 393.5707662,
            "range": "12.1803",
            "unit": "ms",
            "extra": "2*Stdev = 12.1803 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 162.1507584,
            "range": "5.97943",
            "unit": "ms",
            "extra": "2*Stdev = 5.97943 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 15.930756,
            "range": "0.990379",
            "unit": "ms",
            "extra": "2*Stdev = 0.990379 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.386575943,
            "range": "0.110418",
            "unit": "ms",
            "extra": "2*Stdev = 0.110418 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 34.7731812,
            "range": "3.40087",
            "unit": "ms",
            "extra": "2*Stdev = 3.40087 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 0.956861557,
            "range": "0.0504887",
            "unit": "ms",
            "extra": "2*Stdev = 0.0504887 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 7.1952348,
            "range": "0.363697",
            "unit": "ms",
            "extra": "2*Stdev = 0.363697 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.096399996,
            "range": "0.00589194",
            "unit": "ms",
            "extra": "2*Stdev = 0.00589194 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 242.0978665,
            "range": "2.59994",
            "unit": "ms",
            "extra": "2*Stdev = 2.59994 ms"
          },
          {
            "name": "Long variable names",
            "value": 21.6790812,
            "range": "1.47813",
            "unit": "ms",
            "extra": "2*Stdev = 1.47813 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 52.5215688,
            "range": "3.10585",
            "unit": "ms",
            "extra": "2*Stdev = 3.10585 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 16.353395775,
            "range": "1.3205",
            "unit": "ms",
            "extra": "2*Stdev = 1.3205 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.003173848,
            "range": "0.000188362",
            "unit": "ms",
            "extra": "2*Stdev = 0.000188362 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 181.53479525,
            "range": "3.52108",
            "unit": "ms",
            "extra": "2*Stdev = 3.52108 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 164.0035034,
            "range": "12.0567",
            "unit": "ms",
            "extra": "2*Stdev = 12.0567 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 170.1131172,
            "range": "13.7029",
            "unit": "ms",
            "extra": "2*Stdev = 13.7029 ms"
          },
          {
            "name": "Whitespace",
            "value": 17.16301065,
            "range": "0.948937",
            "unit": "ms",
            "extra": "2*Stdev = 0.948937 ms"
          },
          {
            "name": "Line comment",
            "value": 294.9001034,
            "range": "16.2007",
            "unit": "ms",
            "extra": "2*Stdev = 16.2007 ms"
          },
          {
            "name": "Block comment",
            "value": 269.0289724,
            "range": "20.1851",
            "unit": "ms",
            "extra": "2*Stdev = 20.1851 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.313142998,
            "range": "0.0240451",
            "unit": "ms",
            "extra": "2*Stdev = 0.0240451 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 1373.984524,
            "range": "27.2569",
            "unit": "ms",
            "extra": "2*Stdev = 27.2569 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "49699333+dependabot[bot]@users.noreply.github.com",
            "name": "dependabot[bot]",
            "username": "dependabot[bot]"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "98a64897f5f4c1fd7e849ef32dc5ab37af375db5",
          "message": "Bump renovatebot/github-action from 46.3.0 to 46.3.1 (#2837)\n\nBumps [renovatebot/github-action](https://github.com/renovatebot/github-action) from 46.3.0 to 46.3.1.\n- [Release notes](https://github.com/renovatebot/github-action/releases)\n- [Changelog](https://github.com/renovatebot/github-action/blob/main/CHANGELOG.md)\n- [Commits](https://github.com/renovatebot/github-action/compare/v46.3.0...v46.3.1)\n\n---\nupdated-dependencies:\n- dependency-name: renovatebot/github-action\n  dependency-version: 46.3.1\n  dependency-type: direct:production\n  update-type: version-update:semver-patch\n...\n\nSigned-off-by: dependabot[bot] <support@github.com>\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
          "timestamp": "2026-09-21T13:06:27+02:00",
          "tree_id": "2ab92ee61a651e8d69af18bfca9557040e7c819e",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/98a64897f5f4c1fd7e849ef32dc5ab37af375db5"
        },
        "date": 1789989294369,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.036659868,
            "range": "0.00189802",
            "unit": "ms",
            "extra": "2*Stdev = 0.00189802 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.036761572,
            "range": "0.00322041",
            "unit": "ms",
            "extra": "2*Stdev = 0.00322041 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.020977813,
            "range": "0.00158783",
            "unit": "ms",
            "extra": "2*Stdev = 0.00158783 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 265.0997322,
            "range": "14.4669",
            "unit": "ms",
            "extra": "2*Stdev = 14.4669 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.010444971,
            "range": "0.00068716",
            "unit": "ms",
            "extra": "2*Stdev = 0.00068716 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 235.7221258,
            "range": "4.19234",
            "unit": "ms",
            "extra": "2*Stdev = 4.19234 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.03694355,
            "range": "0.00281546",
            "unit": "ms",
            "extra": "2*Stdev = 0.00281546 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 72.243524,
            "range": "4.63102",
            "unit": "ms",
            "extra": "2*Stdev = 4.63102 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.013015935,
            "range": "0.000732854",
            "unit": "ms",
            "extra": "2*Stdev = 0.000732854 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 207.8784162,
            "range": "3.3865",
            "unit": "ms",
            "extra": "2*Stdev = 3.3865 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.010956288,
            "range": "0.000975384",
            "unit": "ms",
            "extra": "2*Stdev = 0.000975384 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 166.6479354,
            "range": "12.8083",
            "unit": "ms",
            "extra": "2*Stdev = 12.8083 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.039094232,
            "range": "0.00335807",
            "unit": "ms",
            "extra": "2*Stdev = 0.00335807 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 143.8939706,
            "range": "5.01612",
            "unit": "ms",
            "extra": "2*Stdev = 5.01612 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.033730045,
            "range": "0.00325183",
            "unit": "ms",
            "extra": "2*Stdev = 0.00325183 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 178.8401948,
            "range": "7.74906",
            "unit": "ms",
            "extra": "2*Stdev = 7.74906 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000322452,
            "range": "2.1082e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.1082e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.007365036,
            "range": "0.000199382",
            "unit": "ms",
            "extra": "2*Stdev = 0.000199382 ms"
          },
          {
            "name": "large1.parse",
            "value": 214.7256802,
            "range": "5.11149",
            "unit": "ms",
            "extra": "2*Stdev = 5.11149 ms"
          },
          {
            "name": "large1.resolve",
            "value": 66.6802387,
            "range": "2.82302",
            "unit": "ms",
            "extra": "2*Stdev = 2.82302 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 55.284806,
            "range": "3.51067",
            "unit": "ms",
            "extra": "2*Stdev = 3.51067 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 110.730619,
            "range": "4.36367",
            "unit": "ms",
            "extra": "2*Stdev = 4.36367 ms"
          },
          {
            "name": "large2.normalize",
            "value": 113.099554,
            "range": "4.06042",
            "unit": "ms",
            "extra": "2*Stdev = 4.06042 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 255.435504,
            "range": "6.15432",
            "unit": "ms",
            "extra": "2*Stdev = 6.15432 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 641.9669782,
            "range": "3.4193",
            "unit": "ms",
            "extra": "2*Stdev = 3.4193 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2028.7996932,
            "range": "126.618",
            "unit": "ms",
            "extra": "2*Stdev = 126.618 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 288.122767,
            "range": "3.23985",
            "unit": "ms",
            "extra": "2*Stdev = 3.23985 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.520530215,
            "range": "0.0216223",
            "unit": "ms",
            "extra": "2*Stdev = 0.0216223 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1301.5652398,
            "range": "9.18707",
            "unit": "ms",
            "extra": "2*Stdev = 9.18707 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 63.97266825,
            "range": "5.60564",
            "unit": "ms",
            "extra": "2*Stdev = 5.60564 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.282628931,
            "range": "0.0265087",
            "unit": "ms",
            "extra": "2*Stdev = 0.0265087 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2108.3795296,
            "range": "121.037",
            "unit": "ms",
            "extra": "2*Stdev = 121.037 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3865.864235,
            "range": "15.3541",
            "unit": "ms",
            "extra": "2*Stdev = 15.3541 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 547.5920094,
            "range": "51.5593",
            "unit": "ms",
            "extra": "2*Stdev = 51.5593 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2068.8703921,
            "range": "99.7756",
            "unit": "ms",
            "extra": "2*Stdev = 99.7756 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 4056.570162,
            "range": "250.989",
            "unit": "ms",
            "extra": "2*Stdev = 250.989 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.164307005,
            "range": "0.0121986",
            "unit": "ms",
            "extra": "2*Stdev = 0.0121986 ms"
          },
          {
            "name": "large4.resolve",
            "value": 347.8422052,
            "range": "17.2105",
            "unit": "ms",
            "extra": "2*Stdev = 17.2105 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 323.7475014,
            "range": "2.98553",
            "unit": "ms",
            "extra": "2*Stdev = 2.98553 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.479355862,
            "range": "0.0665632",
            "unit": "ms",
            "extra": "2*Stdev = 0.0665632 ms"
          },
          {
            "name": "large5.resolve",
            "value": 138.4668282,
            "range": "2.71124",
            "unit": "ms",
            "extra": "2*Stdev = 2.71124 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 46.6792475,
            "range": "1.36415",
            "unit": "ms",
            "extra": "2*Stdev = 1.36415 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 57.9516515,
            "range": "1.37954",
            "unit": "ms",
            "extra": "2*Stdev = 1.37954 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 573.8427358,
            "range": "4.61586",
            "unit": "ms",
            "extra": "2*Stdev = 4.61586 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000149833,
            "range": "7.258e-06",
            "unit": "ms",
            "extra": "2*Stdev = 7.258e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000155669,
            "range": "1.3256e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.3256e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 582.9806476,
            "range": "7.45246",
            "unit": "ms",
            "extra": "2*Stdev = 7.45246 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.760380068,
            "range": "0.0709087",
            "unit": "ms",
            "extra": "2*Stdev = 0.0709087 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.314976356,
            "range": "0.0780911",
            "unit": "ms",
            "extra": "2*Stdev = 0.0780911 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.160657306,
            "range": "0.085811",
            "unit": "ms",
            "extra": "2*Stdev = 0.085811 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 397.996698,
            "range": "23.7802",
            "unit": "ms",
            "extra": "2*Stdev = 23.7802 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 378.9530438,
            "range": "15.1513",
            "unit": "ms",
            "extra": "2*Stdev = 15.1513 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 310.311058,
            "range": "8.96924",
            "unit": "ms",
            "extra": "2*Stdev = 8.96924 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 325.2914936,
            "range": "16.1918",
            "unit": "ms",
            "extra": "2*Stdev = 16.1918 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 191.3644504,
            "range": "5.01353",
            "unit": "ms",
            "extra": "2*Stdev = 5.01353 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 56.3665134,
            "range": "3.33428",
            "unit": "ms",
            "extra": "2*Stdev = 3.33428 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 890.2926706,
            "range": "10.281",
            "unit": "ms",
            "extra": "2*Stdev = 10.281 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 406.7887572,
            "range": "10.8336",
            "unit": "ms",
            "extra": "2*Stdev = 10.8336 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 167.8378146,
            "range": "3.81429",
            "unit": "ms",
            "extra": "2*Stdev = 3.81429 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 18.4520122,
            "range": "1.11001",
            "unit": "ms",
            "extra": "2*Stdev = 1.11001 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.377349806,
            "range": "0.0925159",
            "unit": "ms",
            "extra": "2*Stdev = 0.0925159 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 36.4241209,
            "range": "2.03154",
            "unit": "ms",
            "extra": "2*Stdev = 2.03154 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.095251331,
            "range": "0.0884743",
            "unit": "ms",
            "extra": "2*Stdev = 0.0884743 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 7.9611096,
            "range": "0.410423",
            "unit": "ms",
            "extra": "2*Stdev = 0.410423 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.09857491,
            "range": "0.00546678",
            "unit": "ms",
            "extra": "2*Stdev = 0.00546678 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 227.9313668,
            "range": "12.8782",
            "unit": "ms",
            "extra": "2*Stdev = 12.8782 ms"
          },
          {
            "name": "Long variable names",
            "value": 20.2600457,
            "range": "1.41518",
            "unit": "ms",
            "extra": "2*Stdev = 1.41518 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 50.2223002,
            "range": "4.46617",
            "unit": "ms",
            "extra": "2*Stdev = 4.46617 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 15.1327549,
            "range": "1.50276",
            "unit": "ms",
            "extra": "2*Stdev = 1.50276 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.003094796,
            "range": "0.000169696",
            "unit": "ms",
            "extra": "2*Stdev = 0.000169696 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 176.0465617,
            "range": "2.25573",
            "unit": "ms",
            "extra": "2*Stdev = 2.25573 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 146.7657058,
            "range": "12.2559",
            "unit": "ms",
            "extra": "2*Stdev = 12.2559 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 150.239796,
            "range": "7.53561",
            "unit": "ms",
            "extra": "2*Stdev = 7.53561 ms"
          },
          {
            "name": "Whitespace",
            "value": 13.8974974,
            "range": "0.904188",
            "unit": "ms",
            "extra": "2*Stdev = 0.904188 ms"
          },
          {
            "name": "Line comment",
            "value": 253.7269616,
            "range": "4.39669",
            "unit": "ms",
            "extra": "2*Stdev = 4.39669 ms"
          },
          {
            "name": "Block comment",
            "value": 223.4646868,
            "range": "12.2604",
            "unit": "ms",
            "extra": "2*Stdev = 12.2604 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.324907067,
            "range": "0.0221498",
            "unit": "ms",
            "extra": "2*Stdev = 0.0221498 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 1416.6831528,
            "range": "11.0197",
            "unit": "ms",
            "extra": "2*Stdev = 11.0197 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d180f603ab30566d6c487dfbb16a91195c056219",
          "message": "Fix/1657 explain imported type errors (#2839)\n\n* Apply --explain to type errors nested in MissingImports.\n\nRecursively upgrade Imported TypeErrors to DetailedTypeError when\n--explain is set, including failures wrapped in MissingImports or\nSourcedException, so imported-file errors match direct stdin errors.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Add changelog entry for --explain on imported type errors.\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-22T18:54:11Z",
          "tree_id": "a3d2763df9d0c6259cf123ab5dc8247e514c8d4f",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/d180f603ab30566d6c487dfbb16a91195c056219"
        },
        "date": 1790104177675,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.026817767,
            "range": "0.00249499",
            "unit": "ms",
            "extra": "2*Stdev = 0.00249499 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.027226993,
            "range": "0.00221218",
            "unit": "ms",
            "extra": "2*Stdev = 0.00221218 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.015833254,
            "range": "0.000827284",
            "unit": "ms",
            "extra": "2*Stdev = 0.000827284 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 208.5700386,
            "range": "18.7482",
            "unit": "ms",
            "extra": "2*Stdev = 18.7482 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.007212221,
            "range": "0.000402172",
            "unit": "ms",
            "extra": "2*Stdev = 0.000402172 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 193.5652626,
            "range": "8.01438",
            "unit": "ms",
            "extra": "2*Stdev = 8.01438 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.02220048,
            "range": "0.00181162",
            "unit": "ms",
            "extra": "2*Stdev = 0.00181162 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 48.6036539,
            "range": "1.47174",
            "unit": "ms",
            "extra": "2*Stdev = 1.47174 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.007366697,
            "range": "0.0004842",
            "unit": "ms",
            "extra": "2*Stdev = 0.0004842 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 179.8825006,
            "range": "2.93351",
            "unit": "ms",
            "extra": "2*Stdev = 2.93351 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.006183039,
            "range": "0.000340148",
            "unit": "ms",
            "extra": "2*Stdev = 0.000340148 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 139.42397,
            "range": "6.56973",
            "unit": "ms",
            "extra": "2*Stdev = 6.56973 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.025958295,
            "range": "0.00200925",
            "unit": "ms",
            "extra": "2*Stdev = 0.00200925 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 118.3488016,
            "range": "2.84241",
            "unit": "ms",
            "extra": "2*Stdev = 2.84241 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.021064813,
            "range": "0.00156215",
            "unit": "ms",
            "extra": "2*Stdev = 0.00156215 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 163.2125924,
            "range": "15.3758",
            "unit": "ms",
            "extra": "2*Stdev = 15.3758 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000208963,
            "range": "1.8864e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.8864e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.005190745,
            "range": "0.000128916",
            "unit": "ms",
            "extra": "2*Stdev = 0.000128916 ms"
          },
          {
            "name": "large1.parse",
            "value": 156.0041396,
            "range": "4.67371",
            "unit": "ms",
            "extra": "2*Stdev = 4.67371 ms"
          },
          {
            "name": "large1.resolve",
            "value": 49.225856875,
            "range": "4.69813",
            "unit": "ms",
            "extra": "2*Stdev = 4.69813 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 40.28038955,
            "range": "3.37088",
            "unit": "ms",
            "extra": "2*Stdev = 3.37088 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 77.8138394,
            "range": "4.46413",
            "unit": "ms",
            "extra": "2*Stdev = 4.46413 ms"
          },
          {
            "name": "large2.normalize",
            "value": 82.0695478,
            "range": "3.10347",
            "unit": "ms",
            "extra": "2*Stdev = 3.10347 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 193.2707656,
            "range": "6.42279",
            "unit": "ms",
            "extra": "2*Stdev = 6.42279 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 446.2633418,
            "range": "11.4179",
            "unit": "ms",
            "extra": "2*Stdev = 11.4179 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 1641.78662785,
            "range": "136.192",
            "unit": "ms",
            "extra": "2*Stdev = 136.192 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 306.7823996,
            "range": "17.5564",
            "unit": "ms",
            "extra": "2*Stdev = 17.5564 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.351178753,
            "range": "0.0307312",
            "unit": "ms",
            "extra": "2*Stdev = 0.0307312 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1010.9064374,
            "range": "16.4018",
            "unit": "ms",
            "extra": "2*Stdev = 16.4018 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 73.2518386,
            "range": "4.03559",
            "unit": "ms",
            "extra": "2*Stdev = 4.03559 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.205304744,
            "range": "0.0171271",
            "unit": "ms",
            "extra": "2*Stdev = 0.0171271 ms"
          },
          {
            "name": "large3.resolve",
            "value": 1700.45828515,
            "range": "34.4302",
            "unit": "ms",
            "extra": "2*Stdev = 34.4302 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3550.9974008,
            "range": "23.9025",
            "unit": "ms",
            "extra": "2*Stdev = 23.9025 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 705.369850525,
            "range": "62.7778",
            "unit": "ms",
            "extra": "2*Stdev = 62.7778 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 1599.7733219,
            "range": "59.5227",
            "unit": "ms",
            "extra": "2*Stdev = 59.5227 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 3768.5266344,
            "range": "112.893",
            "unit": "ms",
            "extra": "2*Stdev = 112.893 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.116806635,
            "range": "0.00515377",
            "unit": "ms",
            "extra": "2*Stdev = 0.00515377 ms"
          },
          {
            "name": "large4.resolve",
            "value": 239.122835,
            "range": "18.4571",
            "unit": "ms",
            "extra": "2*Stdev = 18.4571 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 247.1645396,
            "range": "9.37226",
            "unit": "ms",
            "extra": "2*Stdev = 9.37226 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.028924884,
            "range": "0.0445998",
            "unit": "ms",
            "extra": "2*Stdev = 0.0445998 ms"
          },
          {
            "name": "large5.resolve",
            "value": 96.7451958,
            "range": "5.66747",
            "unit": "ms",
            "extra": "2*Stdev = 5.66747 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 37.2957638,
            "range": "0.92195",
            "unit": "ms",
            "extra": "2*Stdev = 0.92195 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 42.9128227,
            "range": "2.83705",
            "unit": "ms",
            "extra": "2*Stdev = 2.83705 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 414.8784984,
            "range": "3.63188",
            "unit": "ms",
            "extra": "2*Stdev = 3.63188 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000090715,
            "range": "7.128e-06",
            "unit": "ms",
            "extra": "2*Stdev = 7.128e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000093343,
            "range": "8.062e-06",
            "unit": "ms",
            "extra": "2*Stdev = 8.062e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 396.9652672,
            "range": "19.0801",
            "unit": "ms",
            "extra": "2*Stdev = 19.0801 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.592471956,
            "range": "0.0466971",
            "unit": "ms",
            "extra": "2*Stdev = 0.0466971 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.068839915,
            "range": "0.05858",
            "unit": "ms",
            "extra": "2*Stdev = 0.05858 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.017807928,
            "range": "0.0540811",
            "unit": "ms",
            "extra": "2*Stdev = 0.0540811 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 250.052956,
            "range": "7.1811",
            "unit": "ms",
            "extra": "2*Stdev = 7.1811 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 226.9916618,
            "range": "6.06474",
            "unit": "ms",
            "extra": "2*Stdev = 6.06474 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 184.6972192,
            "range": "4.23495",
            "unit": "ms",
            "extra": "2*Stdev = 4.23495 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 236.0064108,
            "range": "6.83626",
            "unit": "ms",
            "extra": "2*Stdev = 6.83626 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 126.073509,
            "range": "6.61118",
            "unit": "ms",
            "extra": "2*Stdev = 6.61118 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 50.0499787,
            "range": "2.73374",
            "unit": "ms",
            "extra": "2*Stdev = 2.73374 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 629.8673368,
            "range": "14.73",
            "unit": "ms",
            "extra": "2*Stdev = 14.73 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 282.5027555,
            "range": "13.3214",
            "unit": "ms",
            "extra": "2*Stdev = 13.3214 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 115.6237568,
            "range": "2.90988",
            "unit": "ms",
            "extra": "2*Stdev = 2.90988 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 11.03281815,
            "range": "1.01274",
            "unit": "ms",
            "extra": "2*Stdev = 1.01274 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 0.83833659,
            "range": "0.0333324",
            "unit": "ms",
            "extra": "2*Stdev = 0.0333324 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 24.4559296,
            "range": "1.49958",
            "unit": "ms",
            "extra": "2*Stdev = 1.49958 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 0.712250468,
            "range": "0.0532309",
            "unit": "ms",
            "extra": "2*Stdev = 0.0532309 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.655832025,
            "range": "0.519028",
            "unit": "ms",
            "extra": "2*Stdev = 0.519028 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.069130551,
            "range": "0.00662841",
            "unit": "ms",
            "extra": "2*Stdev = 0.00662841 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 198.1458564,
            "range": "2.5867",
            "unit": "ms",
            "extra": "2*Stdev = 2.5867 ms"
          },
          {
            "name": "Long variable names",
            "value": 14.95885185,
            "range": "0.367435",
            "unit": "ms",
            "extra": "2*Stdev = 0.367435 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 39.4095472,
            "range": "2.88198",
            "unit": "ms",
            "extra": "2*Stdev = 2.88198 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 11.14121065,
            "range": "1.02129",
            "unit": "ms",
            "extra": "2*Stdev = 1.02129 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.002309684,
            "range": "0.0001945",
            "unit": "ms",
            "extra": "2*Stdev = 0.0001945 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 147.81904305,
            "range": "2.15741",
            "unit": "ms",
            "extra": "2*Stdev = 2.15741 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 110.3477482,
            "range": "10.0883",
            "unit": "ms",
            "extra": "2*Stdev = 10.0883 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 103.6103822,
            "range": "3.03027",
            "unit": "ms",
            "extra": "2*Stdev = 3.03027 ms"
          },
          {
            "name": "Whitespace",
            "value": 10.3130801,
            "range": "0.662045",
            "unit": "ms",
            "extra": "2*Stdev = 0.662045 ms"
          },
          {
            "name": "Line comment",
            "value": 173.6804042,
            "range": "3.94953",
            "unit": "ms",
            "extra": "2*Stdev = 3.94953 ms"
          },
          {
            "name": "Block comment",
            "value": 176.251408875,
            "range": "5.87506",
            "unit": "ms",
            "extra": "2*Stdev = 5.87506 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.249827828,
            "range": "0.0117928",
            "unit": "ms",
            "extra": "2*Stdev = 0.0117928 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 1092.02268,
            "range": "5.7888",
            "unit": "ms",
            "extra": "2*Stdev = 5.7888 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "12d87a9f391600a76ccf54a41be1815042bbc838",
          "message": "Fix/1218 bool literal normalize (#2838)\n\n* Reduce BoolEQ/BoolNE on boolean literals before conv.\n\nHandle all VBoolLit pairs in eval and mirror the shortcut in\nnormalizeWithM and isNormalized so literal comparisons avoid\njudgmentallyEqual when both sides are already literals.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Add changelog entry for boolean literal ==/!= optimization.\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-22T19:49:52Z",
          "tree_id": "daf3fe6e24a0834fe56a7e8bc7a5f2342c140fed",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/12d87a9f391600a76ccf54a41be1815042bbc838"
        },
        "date": 1790107278448,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.053321309,
            "range": "0.00254947",
            "unit": "ms",
            "extra": "2*Stdev = 0.00254947 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053328386,
            "range": "0.00388277",
            "unit": "ms",
            "extra": "2*Stdev = 0.00388277 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.034246586,
            "range": "0.00306084",
            "unit": "ms",
            "extra": "2*Stdev = 0.00306084 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 412.9800958,
            "range": "39.1038",
            "unit": "ms",
            "extra": "2*Stdev = 39.1038 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017756311,
            "range": "0.00094264",
            "unit": "ms",
            "extra": "2*Stdev = 0.00094264 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 330.766718,
            "range": "5.20802",
            "unit": "ms",
            "extra": "2*Stdev = 5.20802 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.046003228,
            "range": "0.00283569",
            "unit": "ms",
            "extra": "2*Stdev = 0.00283569 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 84.7053531,
            "range": "4.18225",
            "unit": "ms",
            "extra": "2*Stdev = 4.18225 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020749022,
            "range": "0.00185635",
            "unit": "ms",
            "extra": "2*Stdev = 0.00185635 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 448.3970405,
            "range": "36.7451",
            "unit": "ms",
            "extra": "2*Stdev = 36.7451 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017439557,
            "range": "0.000680342",
            "unit": "ms",
            "extra": "2*Stdev = 0.000680342 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 215.0310654,
            "range": "6.75062",
            "unit": "ms",
            "extra": "2*Stdev = 6.75062 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053587655,
            "range": "0.0026417",
            "unit": "ms",
            "extra": "2*Stdev = 0.0026417 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 198.1180184,
            "range": "16.4774",
            "unit": "ms",
            "extra": "2*Stdev = 16.4774 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.048788776,
            "range": "0.00411776",
            "unit": "ms",
            "extra": "2*Stdev = 0.00411776 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 253.420405,
            "range": "7.41468",
            "unit": "ms",
            "extra": "2*Stdev = 7.41468 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000490545,
            "range": "3.756e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.756e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010954071,
            "range": "0.000907954",
            "unit": "ms",
            "extra": "2*Stdev = 0.000907954 ms"
          },
          {
            "name": "large1.parse",
            "value": 313.68509,
            "range": "4.89273",
            "unit": "ms",
            "extra": "2*Stdev = 4.89273 ms"
          },
          {
            "name": "large1.resolve",
            "value": 98.9910946,
            "range": "7.2749",
            "unit": "ms",
            "extra": "2*Stdev = 7.2749 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 75.7697047,
            "range": "3.01372",
            "unit": "ms",
            "extra": "2*Stdev = 3.01372 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.541934,
            "range": "5.82868",
            "unit": "ms",
            "extra": "2*Stdev = 5.82868 ms"
          },
          {
            "name": "large2.normalize",
            "value": 165.1797306,
            "range": "8.99415",
            "unit": "ms",
            "extra": "2*Stdev = 8.99415 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 335.3765766,
            "range": "7.98111",
            "unit": "ms",
            "extra": "2*Stdev = 7.98111 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 821.2343976,
            "range": "7.28712",
            "unit": "ms",
            "extra": "2*Stdev = 7.28712 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2776.18455515,
            "range": "94.3468",
            "unit": "ms",
            "extra": "2*Stdev = 94.3468 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 409.5072512,
            "range": "11.4368",
            "unit": "ms",
            "extra": "2*Stdev = 11.4368 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.674110475,
            "range": "0.0670777",
            "unit": "ms",
            "extra": "2*Stdev = 0.0670777 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1805.6793822,
            "range": "12.01",
            "unit": "ms",
            "extra": "2*Stdev = 12.01 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.0140738,
            "range": "8.03504",
            "unit": "ms",
            "extra": "2*Stdev = 8.03504 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.411801243,
            "range": "0.03389",
            "unit": "ms",
            "extra": "2*Stdev = 0.03389 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2772.23762,
            "range": "187.954",
            "unit": "ms",
            "extra": "2*Stdev = 187.954 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5417.6871348,
            "range": "240.853",
            "unit": "ms",
            "extra": "2*Stdev = 240.853 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 743.0163762,
            "range": "61.4019",
            "unit": "ms",
            "extra": "2*Stdev = 61.4019 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2943.56190955,
            "range": "238.648",
            "unit": "ms",
            "extra": "2*Stdev = 238.648 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 6054.8599216,
            "range": "233.644",
            "unit": "ms",
            "extra": "2*Stdev = 233.644 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.260037774,
            "range": "0.0193018",
            "unit": "ms",
            "extra": "2*Stdev = 0.0193018 ms"
          },
          {
            "name": "large4.resolve",
            "value": 521.5288516,
            "range": "12.0001",
            "unit": "ms",
            "extra": "2*Stdev = 12.0001 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 440.9511596,
            "range": "7.07406",
            "unit": "ms",
            "extra": "2*Stdev = 7.07406 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.939068677,
            "range": "0.0318596",
            "unit": "ms",
            "extra": "2*Stdev = 0.0318596 ms"
          },
          {
            "name": "large5.resolve",
            "value": 209.9803844,
            "range": "12.9811",
            "unit": "ms",
            "extra": "2*Stdev = 12.9811 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.7271356,
            "range": "3.30519",
            "unit": "ms",
            "extra": "2*Stdev = 3.30519 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.789637,
            "range": "5.9715",
            "unit": "ms",
            "extra": "2*Stdev = 5.9715 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 830.9005964,
            "range": "4.07877",
            "unit": "ms",
            "extra": "2*Stdev = 4.07877 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000213679,
            "range": "1.0814e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.0814e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000214827,
            "range": "1.282e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.282e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 959.3587142,
            "range": "36.6482",
            "unit": "ms",
            "extra": "2*Stdev = 36.6482 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.257675918,
            "range": "0.092463",
            "unit": "ms",
            "extra": "2*Stdev = 0.092463 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.37143645,
            "range": "0.216754",
            "unit": "ms",
            "extra": "2*Stdev = 0.216754 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.73240635,
            "range": "0.085344",
            "unit": "ms",
            "extra": "2*Stdev = 0.085344 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 553.6930914,
            "range": "8.80663",
            "unit": "ms",
            "extra": "2*Stdev = 8.80663 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 511.4940974,
            "range": "6.52563",
            "unit": "ms",
            "extra": "2*Stdev = 6.52563 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 413.6342674,
            "range": "2.82758",
            "unit": "ms",
            "extra": "2*Stdev = 2.82758 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 472.9942186,
            "range": "43.5384",
            "unit": "ms",
            "extra": "2*Stdev = 43.5384 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 311.7547146,
            "range": "15.2755",
            "unit": "ms",
            "extra": "2*Stdev = 15.2755 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 90.6807349,
            "range": "4.26202",
            "unit": "ms",
            "extra": "2*Stdev = 4.26202 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1398.7757212,
            "range": "117.38",
            "unit": "ms",
            "extra": "2*Stdev = 117.38 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 627.144915,
            "range": "7.86511",
            "unit": "ms",
            "extra": "2*Stdev = 7.86511 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 226.0883016,
            "range": "4.27792",
            "unit": "ms",
            "extra": "2*Stdev = 4.27792 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 21.9562425,
            "range": "1.44729",
            "unit": "ms",
            "extra": "2*Stdev = 1.44729 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.849068193,
            "range": "0.123587",
            "unit": "ms",
            "extra": "2*Stdev = 0.123587 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 49.6291088,
            "range": "4.74703",
            "unit": "ms",
            "extra": "2*Stdev = 4.74703 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.493755175,
            "range": "0.134791",
            "unit": "ms",
            "extra": "2*Stdev = 0.134791 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 12.165231475,
            "range": "1.21636",
            "unit": "ms",
            "extra": "2*Stdev = 1.21636 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.142836018,
            "range": "0.0115288",
            "unit": "ms",
            "extra": "2*Stdev = 0.0115288 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 340.9067125,
            "range": "5.08114",
            "unit": "ms",
            "extra": "2*Stdev = 5.08114 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.8661074,
            "range": "3.07031",
            "unit": "ms",
            "extra": "2*Stdev = 3.07031 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 89.227942,
            "range": "5.23194",
            "unit": "ms",
            "extra": "2*Stdev = 5.23194 ms"
          },
          {
            "name": "Long double-quoted strings",
            "value": 23.5737046,
            "range": "2.01738",
            "unit": "ms",
            "extra": "2*Stdev = 2.01738 ms"
          },
          {
            "name": "Long single-quoted strings",
            "value": 0.004789463,
            "range": "0.000294374",
            "unit": "ms",
            "extra": "2*Stdev = 0.000294374 ms"
          },
          {
            "name": "Large natural number literal (1M digits)",
            "value": 355.9429637,
            "range": "1.36104",
            "unit": "ms",
            "extra": "2*Stdev = 1.36104 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 246.7820894,
            "range": "3.94293",
            "unit": "ms",
            "extra": "2*Stdev = 3.94293 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 235.5804314,
            "range": "20.3566",
            "unit": "ms",
            "extra": "2*Stdev = 20.3566 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.300502,
            "range": "1.98106",
            "unit": "ms",
            "extra": "2*Stdev = 1.98106 ms"
          },
          {
            "name": "Line comment",
            "value": 435.5048678,
            "range": "18.8635",
            "unit": "ms",
            "extra": "2*Stdev = 18.8635 ms"
          },
          {
            "name": "Block comment",
            "value": 376.3696078,
            "range": "2.99974",
            "unit": "ms",
            "extra": "2*Stdev = 2.99974 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 0.500440237,
            "range": "0.0485987",
            "unit": "ms",
            "extra": "2*Stdev = 0.0485987 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2165.498838,
            "range": "25.022",
            "unit": "ms",
            "extra": "2*Stdev = 25.022 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e51191bf100539685e5481d2b62ddaa6b4fa406a",
          "message": "Faster megaparsec decimal (#2841)\n\n* Parse unsigned decimals once for naturals and doubles\n\nShare a single digit run between natural and double literals so long\ndecimals are not scanned twice when double parsing fails without a\nfraction or exponent.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* wip fix\n\n* Defer Natural conversion with Lexer.decimal so large decimal literals stay cheap to parse.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* simplify code\n\n* simplify code further\n\n* simplify code further\n\n* add forcing naturals to benchmarks\n\n* optimized algorithms for long literal numbers\n\n* make benchmarks consistent\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-23T15:02:34Z",
          "tree_id": "cd7f7f094875e36da6f7981109343df985ff3c0f",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/e51191bf100539685e5481d2b62ddaa6b4fa406a"
        },
        "date": 1790176378023,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.051025947,
            "range": "0.00264462",
            "unit": "ms",
            "extra": "2*Stdev = 0.00264462 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.050859848,
            "range": "0.00187196",
            "unit": "ms",
            "extra": "2*Stdev = 0.00187196 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.032557798,
            "range": "0.00245271",
            "unit": "ms",
            "extra": "2*Stdev = 0.00245271 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 409.1007358,
            "range": "18.7194",
            "unit": "ms",
            "extra": "2*Stdev = 18.7194 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017469781,
            "range": "0.00110378",
            "unit": "ms",
            "extra": "2*Stdev = 0.00110378 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 302.9438784,
            "range": "9.78102",
            "unit": "ms",
            "extra": "2*Stdev = 9.78102 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045511944,
            "range": "0.00392713",
            "unit": "ms",
            "extra": "2*Stdev = 0.00392713 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 84.0909424,
            "range": "3.9009",
            "unit": "ms",
            "extra": "2*Stdev = 3.9009 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.019962396,
            "range": "0.00117814",
            "unit": "ms",
            "extra": "2*Stdev = 0.00117814 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 264.0855944,
            "range": "4.87544",
            "unit": "ms",
            "extra": "2*Stdev = 4.87544 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017450228,
            "range": "0.00169778",
            "unit": "ms",
            "extra": "2*Stdev = 0.00169778 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 206.0471004,
            "range": "5.12238",
            "unit": "ms",
            "extra": "2*Stdev = 5.12238 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053556494,
            "range": "0.00390452",
            "unit": "ms",
            "extra": "2*Stdev = 0.00390452 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 185.3774492,
            "range": "7.76255",
            "unit": "ms",
            "extra": "2*Stdev = 7.76255 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047532947,
            "range": "0.00310798",
            "unit": "ms",
            "extra": "2*Stdev = 0.00310798 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 236.9796294,
            "range": "6.94415",
            "unit": "ms",
            "extra": "2*Stdev = 6.94415 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000468091,
            "range": "4.166e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.166e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010811889,
            "range": "0.00061879",
            "unit": "ms",
            "extra": "2*Stdev = 0.00061879 ms"
          },
          {
            "name": "large1.parse",
            "value": 307.7362696,
            "range": "9.14923",
            "unit": "ms",
            "extra": "2*Stdev = 9.14923 ms"
          },
          {
            "name": "large1.resolve",
            "value": 91.367796,
            "range": "2.5352",
            "unit": "ms",
            "extra": "2*Stdev = 2.5352 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 70.8783324,
            "range": "5.63876",
            "unit": "ms",
            "extra": "2*Stdev = 5.63876 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 140.6714562,
            "range": "4.24693",
            "unit": "ms",
            "extra": "2*Stdev = 4.24693 ms"
          },
          {
            "name": "large2.normalize",
            "value": 164.986079,
            "range": "6.55112",
            "unit": "ms",
            "extra": "2*Stdev = 6.55112 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 342.639001,
            "range": "10.301",
            "unit": "ms",
            "extra": "2*Stdev = 10.301 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 796.5264056,
            "range": "21.1359",
            "unit": "ms",
            "extra": "2*Stdev = 21.1359 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2625.1058363,
            "range": "162.504",
            "unit": "ms",
            "extra": "2*Stdev = 162.504 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 404.6167948,
            "range": "19.8415",
            "unit": "ms",
            "extra": "2*Stdev = 19.8415 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.668461293,
            "range": "0.0333299",
            "unit": "ms",
            "extra": "2*Stdev = 0.0333299 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1751.6780422,
            "range": "15.5164",
            "unit": "ms",
            "extra": "2*Stdev = 15.5164 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 92.55404,
            "range": "6.89587",
            "unit": "ms",
            "extra": "2*Stdev = 6.89587 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.401936982,
            "range": "0.0220227",
            "unit": "ms",
            "extra": "2*Stdev = 0.0220227 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2651.385,
            "range": "248.763",
            "unit": "ms",
            "extra": "2*Stdev = 248.763 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4903.5776408,
            "range": "31.5776",
            "unit": "ms",
            "extra": "2*Stdev = 31.5776 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 696.1447212,
            "range": "25.2407",
            "unit": "ms",
            "extra": "2*Stdev = 25.2407 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2749.49023805,
            "range": "154.484",
            "unit": "ms",
            "extra": "2*Stdev = 154.484 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5080.4167494,
            "range": "75.5498",
            "unit": "ms",
            "extra": "2*Stdev = 75.5498 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.26127204,
            "range": "0.0220109",
            "unit": "ms",
            "extra": "2*Stdev = 0.0220109 ms"
          },
          {
            "name": "large4.resolve",
            "value": 504.254401,
            "range": "4.99728",
            "unit": "ms",
            "extra": "2*Stdev = 4.99728 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 435.7894406,
            "range": "4.57156",
            "unit": "ms",
            "extra": "2*Stdev = 4.57156 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.930762187,
            "range": "0.0954926",
            "unit": "ms",
            "extra": "2*Stdev = 0.0954926 ms"
          },
          {
            "name": "large5.resolve",
            "value": 201.4422036,
            "range": "7.98139",
            "unit": "ms",
            "extra": "2*Stdev = 7.98139 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.39113,
            "range": "6.29446",
            "unit": "ms",
            "extra": "2*Stdev = 6.29446 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.3071553,
            "range": "3.99699",
            "unit": "ms",
            "extra": "2*Stdev = 3.99699 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 840.4464202,
            "range": "19.9813",
            "unit": "ms",
            "extra": "2*Stdev = 19.9813 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000215067,
            "range": "1.6936e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.6936e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000215954,
            "range": "1.1174e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.1174e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 870.0262174,
            "range": "21.8896",
            "unit": "ms",
            "extra": "2*Stdev = 21.8896 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.233723743,
            "range": "0.0739609",
            "unit": "ms",
            "extra": "2*Stdev = 0.0739609 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.272726112,
            "range": "0.219406",
            "unit": "ms",
            "extra": "2*Stdev = 0.219406 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.6684575,
            "range": "0.106947",
            "unit": "ms",
            "extra": "2*Stdev = 0.106947 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 548.0824286,
            "range": "11.6105",
            "unit": "ms",
            "extra": "2*Stdev = 11.6105 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 484.9561916,
            "range": "14.4743",
            "unit": "ms",
            "extra": "2*Stdev = 14.4743 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 388.6483156,
            "range": "2.80703",
            "unit": "ms",
            "extra": "2*Stdev = 2.80703 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 444.7082146,
            "range": "14.3369",
            "unit": "ms",
            "extra": "2*Stdev = 14.3369 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 306.1880428,
            "range": "6.75827",
            "unit": "ms",
            "extra": "2*Stdev = 6.75827 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 86.8503112,
            "range": "1.55045",
            "unit": "ms",
            "extra": "2*Stdev = 1.55045 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1337.3072616,
            "range": "3.09539",
            "unit": "ms",
            "extra": "2*Stdev = 3.09539 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 565.2318362,
            "range": "3.4221",
            "unit": "ms",
            "extra": "2*Stdev = 3.4221 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 214.2610898,
            "range": "2.81789",
            "unit": "ms",
            "extra": "2*Stdev = 2.81789 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.81951755,
            "range": "1.31518",
            "unit": "ms",
            "extra": "2*Stdev = 1.31518 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.838869843,
            "range": "0.0949225",
            "unit": "ms",
            "extra": "2*Stdev = 0.0949225 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 47.4986068,
            "range": "4.55765",
            "unit": "ms",
            "extra": "2*Stdev = 4.55765 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.489648159,
            "range": "0.0889727",
            "unit": "ms",
            "extra": "2*Stdev = 0.0889727 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 11.24106435,
            "range": "1.0216",
            "unit": "ms",
            "extra": "2*Stdev = 1.0216 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.142342924,
            "range": "0.0104872",
            "unit": "ms",
            "extra": "2*Stdev = 0.0104872 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 300.5754299,
            "range": "4.11448",
            "unit": "ms",
            "extra": "2*Stdev = 4.11448 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 43.5014142,
            "range": "4.14094",
            "unit": "ms",
            "extra": "2*Stdev = 4.14094 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 34.6776554,
            "range": "1.45886",
            "unit": "ms",
            "extra": "2*Stdev = 1.45886 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.4556578,
            "range": "0.717697",
            "unit": "ms",
            "extra": "2*Stdev = 0.717697 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 84.608709,
            "range": "3.95951",
            "unit": "ms",
            "extra": "2*Stdev = 3.95951 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 221.9779411,
            "range": "10.1725",
            "unit": "ms",
            "extra": "2*Stdev = 10.1725 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 202.6337096,
            "range": "3.55289",
            "unit": "ms",
            "extra": "2*Stdev = 3.55289 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 199.8972588,
            "range": "14.7942",
            "unit": "ms",
            "extra": "2*Stdev = 14.7942 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 220.2581262,
            "range": "8.75438",
            "unit": "ms",
            "extra": "2*Stdev = 8.75438 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 207.008028,
            "range": "11.2921",
            "unit": "ms",
            "extra": "2*Stdev = 11.2921 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1629.1492014,
            "range": "40.9591",
            "unit": "ms",
            "extra": "2*Stdev = 40.9591 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 383.0572084,
            "range": "22.4425",
            "unit": "ms",
            "extra": "2*Stdev = 22.4425 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 304.606046,
            "range": "7.24909",
            "unit": "ms",
            "extra": "2*Stdev = 7.24909 ms"
          },
          {
            "name": "Whitespace",
            "value": 23.7650351,
            "range": "0.96557",
            "unit": "ms",
            "extra": "2*Stdev = 0.96557 ms"
          },
          {
            "name": "Line comment",
            "value": 404.9046798,
            "range": "8.0081",
            "unit": "ms",
            "extra": "2*Stdev = 8.0081 ms"
          },
          {
            "name": "Block comment",
            "value": 354.755967475,
            "range": "12.9618",
            "unit": "ms",
            "extra": "2*Stdev = 12.9618 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2089.382506,
            "range": "59.9573",
            "unit": "ms",
            "extra": "2*Stdev = 59.9573 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "47cc941346b7369a36a772555c9bc56f30d45764",
          "message": "Parenthesize function calls used as Nix field selections. (#2842)\n\nNix binds `.` more tightly than application, so `f x.a` reads a field of the argument rather than of the call result.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-25T21:22:23Z",
          "tree_id": "adeedda90f16e36e4126ab4be0bea653e3ed953f",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/47cc941346b7369a36a772555c9bc56f30d45764"
        },
        "date": 1790371983916,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.04905713,
            "range": "0.00472873",
            "unit": "ms",
            "extra": "2*Stdev = 0.00472873 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.049480581,
            "range": "0.00399563",
            "unit": "ms",
            "extra": "2*Stdev = 0.00399563 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033340436,
            "range": "0.00279671",
            "unit": "ms",
            "extra": "2*Stdev = 0.00279671 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 413.4138302,
            "range": "33.8351",
            "unit": "ms",
            "extra": "2*Stdev = 33.8351 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017822515,
            "range": "0.00151283",
            "unit": "ms",
            "extra": "2*Stdev = 0.00151283 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 320.2673256,
            "range": "6.52198",
            "unit": "ms",
            "extra": "2*Stdev = 6.52198 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.046051687,
            "range": "0.00394348",
            "unit": "ms",
            "extra": "2*Stdev = 0.00394348 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 87.692665,
            "range": "5.00613",
            "unit": "ms",
            "extra": "2*Stdev = 5.00613 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020013358,
            "range": "0.00148068",
            "unit": "ms",
            "extra": "2*Stdev = 0.00148068 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 282.1996408,
            "range": "3.93219",
            "unit": "ms",
            "extra": "2*Stdev = 3.93219 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.01739053,
            "range": "0.00149775",
            "unit": "ms",
            "extra": "2*Stdev = 0.00149775 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 218.1114016,
            "range": "7.02216",
            "unit": "ms",
            "extra": "2*Stdev = 7.02216 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053273742,
            "range": "0.00318829",
            "unit": "ms",
            "extra": "2*Stdev = 0.00318829 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 197.1606824,
            "range": "5.44147",
            "unit": "ms",
            "extra": "2*Stdev = 5.44147 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047712232,
            "range": "0.00202419",
            "unit": "ms",
            "extra": "2*Stdev = 0.00202419 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 236.953378,
            "range": "11.77",
            "unit": "ms",
            "extra": "2*Stdev = 11.77 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000486513,
            "range": "2.671e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.671e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011105089,
            "range": "0.000744994",
            "unit": "ms",
            "extra": "2*Stdev = 0.000744994 ms"
          },
          {
            "name": "large1.parse",
            "value": 309.8912743,
            "range": "1.55079",
            "unit": "ms",
            "extra": "2*Stdev = 1.55079 ms"
          },
          {
            "name": "large1.resolve",
            "value": 94.3719156,
            "range": "8.98733",
            "unit": "ms",
            "extra": "2*Stdev = 8.98733 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 72.954225,
            "range": "5.68234",
            "unit": "ms",
            "extra": "2*Stdev = 5.68234 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 142.9763394,
            "range": "4.26855",
            "unit": "ms",
            "extra": "2*Stdev = 4.26855 ms"
          },
          {
            "name": "large2.normalize",
            "value": 165.7034206,
            "range": "3.42178",
            "unit": "ms",
            "extra": "2*Stdev = 3.42178 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 339.4926734,
            "range": "16.6682",
            "unit": "ms",
            "extra": "2*Stdev = 16.6682 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 803.4915792,
            "range": "9.3204",
            "unit": "ms",
            "extra": "2*Stdev = 9.3204 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2679.90093985,
            "range": "157.465",
            "unit": "ms",
            "extra": "2*Stdev = 157.465 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 409.7647388,
            "range": "9.46665",
            "unit": "ms",
            "extra": "2*Stdev = 9.46665 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.677044831,
            "range": "0.042605",
            "unit": "ms",
            "extra": "2*Stdev = 0.042605 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1796.371827,
            "range": "5.72369",
            "unit": "ms",
            "extra": "2*Stdev = 5.72369 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 91.3540613,
            "range": "1.80586",
            "unit": "ms",
            "extra": "2*Stdev = 1.80586 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.41476597,
            "range": "0.0260855",
            "unit": "ms",
            "extra": "2*Stdev = 0.0260855 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2785.57069825,
            "range": "159.723",
            "unit": "ms",
            "extra": "2*Stdev = 159.723 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4896.0267654,
            "range": "42.6759",
            "unit": "ms",
            "extra": "2*Stdev = 42.6759 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 699.5560226,
            "range": "19.5524",
            "unit": "ms",
            "extra": "2*Stdev = 19.5524 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2791.449228,
            "range": "157.328",
            "unit": "ms",
            "extra": "2*Stdev = 157.328 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5373.091004,
            "range": "146.729",
            "unit": "ms",
            "extra": "2*Stdev = 146.729 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.26436671,
            "range": "0.0212086",
            "unit": "ms",
            "extra": "2*Stdev = 0.0212086 ms"
          },
          {
            "name": "large4.resolve",
            "value": 510.5179626,
            "range": "4.24806",
            "unit": "ms",
            "extra": "2*Stdev = 4.24806 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 443.5445576,
            "range": "11.9605",
            "unit": "ms",
            "extra": "2*Stdev = 11.9605 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.943326531,
            "range": "0.0654814",
            "unit": "ms",
            "extra": "2*Stdev = 0.0654814 ms"
          },
          {
            "name": "large5.resolve",
            "value": 205.8208348,
            "range": "4.49811",
            "unit": "ms",
            "extra": "2*Stdev = 4.49811 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.6319574,
            "range": "5.58556",
            "unit": "ms",
            "extra": "2*Stdev = 5.58556 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.5596434,
            "range": "2.76565",
            "unit": "ms",
            "extra": "2*Stdev = 2.76565 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 868.008564,
            "range": "63.5724",
            "unit": "ms",
            "extra": "2*Stdev = 63.5724 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000215135,
            "range": "1.51e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.51e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000223358,
            "range": "1.2398e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.2398e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 900.4092864,
            "range": "16.9049",
            "unit": "ms",
            "extra": "2*Stdev = 16.9049 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.276467937,
            "range": "0.0804601",
            "unit": "ms",
            "extra": "2*Stdev = 0.0804601 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.308906343,
            "range": "0.141556",
            "unit": "ms",
            "extra": "2*Stdev = 0.141556 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.681409618,
            "range": "0.119707",
            "unit": "ms",
            "extra": "2*Stdev = 0.119707 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 560.2477392,
            "range": "5.53206",
            "unit": "ms",
            "extra": "2*Stdev = 5.53206 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 486.9039092,
            "range": "13.3985",
            "unit": "ms",
            "extra": "2*Stdev = 13.3985 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 390.659834,
            "range": "16.1527",
            "unit": "ms",
            "extra": "2*Stdev = 16.1527 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 453.446529,
            "range": "3.95875",
            "unit": "ms",
            "extra": "2*Stdev = 3.95875 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 310.0896348,
            "range": "12.0629",
            "unit": "ms",
            "extra": "2*Stdev = 12.0629 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 88.3470066,
            "range": "1.64681",
            "unit": "ms",
            "extra": "2*Stdev = 1.64681 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1361.8400306,
            "range": "16.2115",
            "unit": "ms",
            "extra": "2*Stdev = 16.2115 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 581.4696596,
            "range": "14.9935",
            "unit": "ms",
            "extra": "2*Stdev = 14.9935 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 216.8757116,
            "range": "3.17322",
            "unit": "ms",
            "extra": "2*Stdev = 3.17322 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.6754316,
            "range": "1.97408",
            "unit": "ms",
            "extra": "2*Stdev = 1.97408 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.875881606,
            "range": "0.107824",
            "unit": "ms",
            "extra": "2*Stdev = 0.107824 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 49.681191,
            "range": "4.93295",
            "unit": "ms",
            "extra": "2*Stdev = 4.93295 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.454436118,
            "range": "0.142727",
            "unit": "ms",
            "extra": "2*Stdev = 0.142727 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 11.4434266,
            "range": "0.843386",
            "unit": "ms",
            "extra": "2*Stdev = 0.843386 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.143977347,
            "range": "0.0034848",
            "unit": "ms",
            "extra": "2*Stdev = 0.0034848 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 299.6784096,
            "range": "3.27155",
            "unit": "ms",
            "extra": "2*Stdev = 3.27155 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 43.6604269,
            "range": "2.08106",
            "unit": "ms",
            "extra": "2*Stdev = 2.08106 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 35.4955068,
            "range": "1.63787",
            "unit": "ms",
            "extra": "2*Stdev = 1.63787 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.0722032,
            "range": "2.87324",
            "unit": "ms",
            "extra": "2*Stdev = 2.87324 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 85.7939626,
            "range": "4.84096",
            "unit": "ms",
            "extra": "2*Stdev = 4.84096 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 253.3095558,
            "range": "18.2619",
            "unit": "ms",
            "extra": "2*Stdev = 18.2619 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 210.133755,
            "range": "4.4887",
            "unit": "ms",
            "extra": "2*Stdev = 4.4887 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 205.5170835,
            "range": "7.37237",
            "unit": "ms",
            "extra": "2*Stdev = 7.37237 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 216.5561999,
            "range": "3.25174",
            "unit": "ms",
            "extra": "2*Stdev = 3.25174 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 216.28901015,
            "range": "6.90975",
            "unit": "ms",
            "extra": "2*Stdev = 6.90975 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1631.351113,
            "range": "20.7199",
            "unit": "ms",
            "extra": "2*Stdev = 20.7199 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 387.2199594,
            "range": "6.36336",
            "unit": "ms",
            "extra": "2*Stdev = 6.36336 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 303.5653574,
            "range": "15.9863",
            "unit": "ms",
            "extra": "2*Stdev = 15.9863 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.3091111,
            "range": "1.70138",
            "unit": "ms",
            "extra": "2*Stdev = 1.70138 ms"
          },
          {
            "name": "Line comment",
            "value": 410.144523,
            "range": "20.012",
            "unit": "ms",
            "extra": "2*Stdev = 20.012 ms"
          },
          {
            "name": "Block comment",
            "value": 358.0426084,
            "range": "27.1075",
            "unit": "ms",
            "extra": "2*Stdev = 27.1075 ms"
          },
          {
            "name": "CPkg/Text",
            "value": 2080.7348714,
            "range": "17.5549",
            "unit": "ms",
            "extra": "2*Stdev = 17.5549 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fb8cd0445c029ebdcc17a89da323bbeb3df62775",
          "message": "Split the CPkg parser bench into parse versus nf. (#2843)\n\nMeasure a root-level exprFromText separately from a full deepseq so source-note cost can be subtracted without a lexer stage.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-26T10:48:34Z",
          "tree_id": "9b5daa1e5bc19eed3217254ac7295c2586f8795f",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/fb8cd0445c029ebdcc17a89da323bbeb3df62775"
        },
        "date": 1790420226221,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.025338601,
            "range": "0.00162499",
            "unit": "ms",
            "extra": "2*Stdev = 0.00162499 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.025857016,
            "range": "0.00240953",
            "unit": "ms",
            "extra": "2*Stdev = 0.00240953 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.014664285,
            "range": "0.00132277",
            "unit": "ms",
            "extra": "2*Stdev = 0.00132277 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 200.0450754,
            "range": "8.91912",
            "unit": "ms",
            "extra": "2*Stdev = 8.91912 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.006770245,
            "range": "0.000269162",
            "unit": "ms",
            "extra": "2*Stdev = 0.000269162 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 174.6116316,
            "range": "6.81582",
            "unit": "ms",
            "extra": "2*Stdev = 6.81582 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.020945764,
            "range": "0.00147812",
            "unit": "ms",
            "extra": "2*Stdev = 0.00147812 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 47.8928478,
            "range": "3.25004",
            "unit": "ms",
            "extra": "2*Stdev = 3.25004 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.007208787,
            "range": "0.00067509",
            "unit": "ms",
            "extra": "2*Stdev = 0.00067509 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 175.947757,
            "range": "10.3459",
            "unit": "ms",
            "extra": "2*Stdev = 10.3459 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.006126361,
            "range": "0.00048801",
            "unit": "ms",
            "extra": "2*Stdev = 0.00048801 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 136.0347998,
            "range": "3.1806",
            "unit": "ms",
            "extra": "2*Stdev = 3.1806 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.024578235,
            "range": "0.00118052",
            "unit": "ms",
            "extra": "2*Stdev = 0.00118052 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 115.0642316,
            "range": "9.95269",
            "unit": "ms",
            "extra": "2*Stdev = 9.95269 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.021047704,
            "range": "0.0017123",
            "unit": "ms",
            "extra": "2*Stdev = 0.0017123 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 149.3136626,
            "range": "7.87944",
            "unit": "ms",
            "extra": "2*Stdev = 7.87944 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000215973,
            "range": "1.3466e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.3466e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.005536617,
            "range": "0.000496386",
            "unit": "ms",
            "extra": "2*Stdev = 0.000496386 ms"
          },
          {
            "name": "large1.parse",
            "value": 151.2063544,
            "range": "9.45933",
            "unit": "ms",
            "extra": "2*Stdev = 9.45933 ms"
          },
          {
            "name": "large1.resolve",
            "value": 46.904992875,
            "range": "1.72764",
            "unit": "ms",
            "extra": "2*Stdev = 1.72764 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 39.8562649,
            "range": "2.00803",
            "unit": "ms",
            "extra": "2*Stdev = 2.00803 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 76.79204,
            "range": "6.70804",
            "unit": "ms",
            "extra": "2*Stdev = 6.70804 ms"
          },
          {
            "name": "large2.normalize",
            "value": 83.060015,
            "range": "4.88771",
            "unit": "ms",
            "extra": "2*Stdev = 4.88771 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 189.9789912,
            "range": "7.46699",
            "unit": "ms",
            "extra": "2*Stdev = 7.46699 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 434.1149646,
            "range": "3.96438",
            "unit": "ms",
            "extra": "2*Stdev = 3.96438 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 1624.53032425,
            "range": "83.7481",
            "unit": "ms",
            "extra": "2*Stdev = 83.7481 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 292.4859876,
            "range": "3.61312",
            "unit": "ms",
            "extra": "2*Stdev = 3.61312 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.363172292,
            "range": "0.0212456",
            "unit": "ms",
            "extra": "2*Stdev = 0.0212456 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1001.1074264,
            "range": "16.393",
            "unit": "ms",
            "extra": "2*Stdev = 16.393 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 72.1259176,
            "range": "6.44222",
            "unit": "ms",
            "extra": "2*Stdev = 6.44222 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.21433436,
            "range": "0.0115869",
            "unit": "ms",
            "extra": "2*Stdev = 0.0115869 ms"
          },
          {
            "name": "large3.resolve",
            "value": 1643.13417895,
            "range": "138.476",
            "unit": "ms",
            "extra": "2*Stdev = 138.476 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3261.5231772,
            "range": "110.317",
            "unit": "ms",
            "extra": "2*Stdev = 110.317 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 668.4150705,
            "range": "16.964",
            "unit": "ms",
            "extra": "2*Stdev = 16.964 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 1654.0624216,
            "range": "31.3496",
            "unit": "ms",
            "extra": "2*Stdev = 31.3496 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 3266.9473806,
            "range": "34.7124",
            "unit": "ms",
            "extra": "2*Stdev = 34.7124 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.120460358,
            "range": "0.0066693",
            "unit": "ms",
            "extra": "2*Stdev = 0.0066693 ms"
          },
          {
            "name": "large4.resolve",
            "value": 242.0487322,
            "range": "4.53439",
            "unit": "ms",
            "extra": "2*Stdev = 4.53439 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 248.0599319,
            "range": "4.9972",
            "unit": "ms",
            "extra": "2*Stdev = 4.9972 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.0042033,
            "range": "0.0331562",
            "unit": "ms",
            "extra": "2*Stdev = 0.0331562 ms"
          },
          {
            "name": "large5.resolve",
            "value": 94.9473558,
            "range": "3.05133",
            "unit": "ms",
            "extra": "2*Stdev = 3.05133 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 38.8722642,
            "range": "3.75052",
            "unit": "ms",
            "extra": "2*Stdev = 3.75052 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 43.1325225,
            "range": "3.1792",
            "unit": "ms",
            "extra": "2*Stdev = 3.1792 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 426.9371492,
            "range": "13.2317",
            "unit": "ms",
            "extra": "2*Stdev = 13.2317 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000091393,
            "range": "8.228e-06",
            "unit": "ms",
            "extra": "2*Stdev = 8.228e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000093529,
            "range": "6.516e-06",
            "unit": "ms",
            "extra": "2*Stdev = 6.516e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 378.3987138,
            "range": "4.78012",
            "unit": "ms",
            "extra": "2*Stdev = 4.78012 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.589973015,
            "range": "0.0228406",
            "unit": "ms",
            "extra": "2*Stdev = 0.0228406 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.02919534,
            "range": "0.0597249",
            "unit": "ms",
            "extra": "2*Stdev = 0.0597249 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.038402125,
            "range": "0.0856264",
            "unit": "ms",
            "extra": "2*Stdev = 0.0856264 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 244.5318726,
            "range": "6.86572",
            "unit": "ms",
            "extra": "2*Stdev = 6.86572 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 209.947441,
            "range": "4.99093",
            "unit": "ms",
            "extra": "2*Stdev = 4.99093 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 176.3925224,
            "range": "3.4158",
            "unit": "ms",
            "extra": "2*Stdev = 3.4158 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 241.454084,
            "range": "5.15639",
            "unit": "ms",
            "extra": "2*Stdev = 5.15639 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 125.2697874,
            "range": "4.17255",
            "unit": "ms",
            "extra": "2*Stdev = 4.17255 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 46.8776445,
            "range": "4.04293",
            "unit": "ms",
            "extra": "2*Stdev = 4.04293 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 633.5831578,
            "range": "26.0603",
            "unit": "ms",
            "extra": "2*Stdev = 26.0603 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 289.31715305,
            "range": "17.9778",
            "unit": "ms",
            "extra": "2*Stdev = 17.9778 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 111.5719172,
            "range": "7.24347",
            "unit": "ms",
            "extra": "2*Stdev = 7.24347 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 10.4024795,
            "range": "0.711631",
            "unit": "ms",
            "extra": "2*Stdev = 0.711631 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 0.85271364,
            "range": "0.0556312",
            "unit": "ms",
            "extra": "2*Stdev = 0.0556312 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 24.9482117,
            "range": "1.39926",
            "unit": "ms",
            "extra": "2*Stdev = 1.39926 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 0.683375818,
            "range": "0.0597017",
            "unit": "ms",
            "extra": "2*Stdev = 0.0597017 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.41360465,
            "range": "0.335883",
            "unit": "ms",
            "extra": "2*Stdev = 0.335883 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.068504663,
            "range": "0.00469091",
            "unit": "ms",
            "extra": "2*Stdev = 0.00469091 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 187.6679883,
            "range": "5.39488",
            "unit": "ms",
            "extra": "2*Stdev = 5.39488 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 19.37586125,
            "range": "0.950446",
            "unit": "ms",
            "extra": "2*Stdev = 0.950446 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 17.861541925,
            "range": "1.28005",
            "unit": "ms",
            "extra": "2*Stdev = 1.28005 ms"
          },
          {
            "name": "Long variable names",
            "value": 12.7478325,
            "range": "1.20204",
            "unit": "ms",
            "extra": "2*Stdev = 1.20204 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 38.101663,
            "range": "2.14798",
            "unit": "ms",
            "extra": "2*Stdev = 2.14798 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 96.4503374,
            "range": "3.57297",
            "unit": "ms",
            "extra": "2*Stdev = 3.57297 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 93.8123484,
            "range": "2.65114",
            "unit": "ms",
            "extra": "2*Stdev = 2.65114 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 91.1737232,
            "range": "4.97593",
            "unit": "ms",
            "extra": "2*Stdev = 4.97593 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 89.1192316,
            "range": "8.06331",
            "unit": "ms",
            "extra": "2*Stdev = 8.06331 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 89.0416564,
            "range": "4.58366",
            "unit": "ms",
            "extra": "2*Stdev = 4.58366 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 866.7071454,
            "range": "34.5617",
            "unit": "ms",
            "extra": "2*Stdev = 34.5617 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 175.2057984,
            "range": "13.8806",
            "unit": "ms",
            "extra": "2*Stdev = 13.8806 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 125.5378478,
            "range": "6.0195",
            "unit": "ms",
            "extra": "2*Stdev = 6.0195 ms"
          },
          {
            "name": "Whitespace",
            "value": 10.382955656,
            "range": "0.354556",
            "unit": "ms",
            "extra": "2*Stdev = 0.354556 ms"
          },
          {
            "name": "Line comment",
            "value": 184.2562066,
            "range": "6.42132",
            "unit": "ms",
            "extra": "2*Stdev = 6.42132 ms"
          },
          {
            "name": "Block comment",
            "value": 157.5989843,
            "range": "11.1889",
            "unit": "ms",
            "extra": "2*Stdev = 11.1889 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 993.1459934,
            "range": "30.4183",
            "unit": "ms",
            "extra": "2*Stdev = 30.4183 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1026.3239544,
            "range": "14.5727",
            "unit": "ms",
            "extra": "2*Stdev = 14.5727 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e5bfb86da186800fa1208cf642f056de0223bd89",
          "message": "Add tests for the dhall repl (#2845)\n\nCover commands, paste mode, quit, and the history file so the line editor can be replaced without changing behavior.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-26T20:43:52Z",
          "tree_id": "8a5be03f10525e56f6ac8418886f3ac9efe16f7e",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/e5bfb86da186800fa1208cf642f056de0223bd89"
        },
        "date": 1790456061932,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.050701829,
            "range": "0.00331153",
            "unit": "ms",
            "extra": "2*Stdev = 0.00331153 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.051221998,
            "range": "0.00274602",
            "unit": "ms",
            "extra": "2*Stdev = 0.00274602 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033298506,
            "range": "0.00325277",
            "unit": "ms",
            "extra": "2*Stdev = 0.00325277 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 414.9021556,
            "range": "15.3133",
            "unit": "ms",
            "extra": "2*Stdev = 15.3133 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017273452,
            "range": "0.00165127",
            "unit": "ms",
            "extra": "2*Stdev = 0.00165127 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 300.854565,
            "range": "6.10648",
            "unit": "ms",
            "extra": "2*Stdev = 6.10648 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045944138,
            "range": "0.00356884",
            "unit": "ms",
            "extra": "2*Stdev = 0.00356884 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 85.7370754,
            "range": "3.30185",
            "unit": "ms",
            "extra": "2*Stdev = 3.30185 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.02022826,
            "range": "0.00201313",
            "unit": "ms",
            "extra": "2*Stdev = 0.00201313 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 261.0755246,
            "range": "5.27319",
            "unit": "ms",
            "extra": "2*Stdev = 5.27319 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017333356,
            "range": "0.00136366",
            "unit": "ms",
            "extra": "2*Stdev = 0.00136366 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 205.3228818,
            "range": "3.03732",
            "unit": "ms",
            "extra": "2*Stdev = 3.03732 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053679622,
            "range": "0.00333994",
            "unit": "ms",
            "extra": "2*Stdev = 0.00333994 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 185.4476862,
            "range": "3.99919",
            "unit": "ms",
            "extra": "2*Stdev = 3.99919 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047463829,
            "range": "0.00269055",
            "unit": "ms",
            "extra": "2*Stdev = 0.00269055 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 232.031329,
            "range": "6.52489",
            "unit": "ms",
            "extra": "2*Stdev = 6.52489 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.00048761,
            "range": "4.5756e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.5756e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010722323,
            "range": "0.000874168",
            "unit": "ms",
            "extra": "2*Stdev = 0.000874168 ms"
          },
          {
            "name": "large1.parse",
            "value": 308.7570386,
            "range": "11.9333",
            "unit": "ms",
            "extra": "2*Stdev = 11.9333 ms"
          },
          {
            "name": "large1.resolve",
            "value": 92.4683761,
            "range": "1.87305",
            "unit": "ms",
            "extra": "2*Stdev = 1.87305 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 72.4070934,
            "range": "2.87487",
            "unit": "ms",
            "extra": "2*Stdev = 2.87487 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 142.1062478,
            "range": "7.81029",
            "unit": "ms",
            "extra": "2*Stdev = 7.81029 ms"
          },
          {
            "name": "large2.normalize",
            "value": 162.2680632,
            "range": "3.22898",
            "unit": "ms",
            "extra": "2*Stdev = 3.22898 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 342.221111,
            "range": "5.04962",
            "unit": "ms",
            "extra": "2*Stdev = 5.04962 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 816.4145918,
            "range": "4.28225",
            "unit": "ms",
            "extra": "2*Stdev = 4.28225 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2675.01448205,
            "range": "180.629",
            "unit": "ms",
            "extra": "2*Stdev = 180.629 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 403.9973866,
            "range": "8.34921",
            "unit": "ms",
            "extra": "2*Stdev = 8.34921 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.67745725,
            "range": "0.0618137",
            "unit": "ms",
            "extra": "2*Stdev = 0.0618137 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1796.5563656,
            "range": "21.0272",
            "unit": "ms",
            "extra": "2*Stdev = 21.0272 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 92.7641076,
            "range": "4.63683",
            "unit": "ms",
            "extra": "2*Stdev = 4.63683 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.404200075,
            "range": "0.0222931",
            "unit": "ms",
            "extra": "2*Stdev = 0.0222931 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2814.64897705,
            "range": "160.561",
            "unit": "ms",
            "extra": "2*Stdev = 160.561 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5005.6418984,
            "range": "27.4782",
            "unit": "ms",
            "extra": "2*Stdev = 27.4782 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 710.85363,
            "range": "43.4109",
            "unit": "ms",
            "extra": "2*Stdev = 43.4109 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2782.0419336,
            "range": "123.578",
            "unit": "ms",
            "extra": "2*Stdev = 123.578 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5277.692559,
            "range": "14.4752",
            "unit": "ms",
            "extra": "2*Stdev = 14.4752 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.263007826,
            "range": "0.0229242",
            "unit": "ms",
            "extra": "2*Stdev = 0.0229242 ms"
          },
          {
            "name": "large4.resolve",
            "value": 511.3823372,
            "range": "7.72166",
            "unit": "ms",
            "extra": "2*Stdev = 7.72166 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 437.377132,
            "range": "3.61921",
            "unit": "ms",
            "extra": "2*Stdev = 3.61921 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.929952834,
            "range": "0.0789126",
            "unit": "ms",
            "extra": "2*Stdev = 0.0789126 ms"
          },
          {
            "name": "large5.resolve",
            "value": 205.2625834,
            "range": "4.65695",
            "unit": "ms",
            "extra": "2*Stdev = 4.65695 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.6209078,
            "range": "5.33003",
            "unit": "ms",
            "extra": "2*Stdev = 5.33003 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.0891528,
            "range": "5.22672",
            "unit": "ms",
            "extra": "2*Stdev = 5.22672 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 853.1137374,
            "range": "4.93149",
            "unit": "ms",
            "extra": "2*Stdev = 4.93149 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000212645,
            "range": "1.403e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.403e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000226081,
            "range": "1.7498e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.7498e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 879.8695432,
            "range": "21.7291",
            "unit": "ms",
            "extra": "2*Stdev = 21.7291 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.248679743,
            "range": "0.086617",
            "unit": "ms",
            "extra": "2*Stdev = 0.086617 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.3002338,
            "range": "0.126714",
            "unit": "ms",
            "extra": "2*Stdev = 0.126714 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.726751943,
            "range": "0.149253",
            "unit": "ms",
            "extra": "2*Stdev = 0.149253 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 551.8111506,
            "range": "9.13934",
            "unit": "ms",
            "extra": "2*Stdev = 9.13934 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 494.451463,
            "range": "46.1713",
            "unit": "ms",
            "extra": "2*Stdev = 46.1713 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 391.0625836,
            "range": "4.77971",
            "unit": "ms",
            "extra": "2*Stdev = 4.77971 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 457.2298782,
            "range": "4.58612",
            "unit": "ms",
            "extra": "2*Stdev = 4.58612 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 310.764608,
            "range": "15.1542",
            "unit": "ms",
            "extra": "2*Stdev = 15.1542 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 88.4309614,
            "range": "1.62325",
            "unit": "ms",
            "extra": "2*Stdev = 1.62325 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1386.6291648,
            "range": "64.0249",
            "unit": "ms",
            "extra": "2*Stdev = 64.0249 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 586.2790082,
            "range": "5.42438",
            "unit": "ms",
            "extra": "2*Stdev = 5.42438 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 216.997907,
            "range": "3.11789",
            "unit": "ms",
            "extra": "2*Stdev = 3.11789 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 21.2091123,
            "range": "1.52634",
            "unit": "ms",
            "extra": "2*Stdev = 1.52634 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.895202587,
            "range": "0.169039",
            "unit": "ms",
            "extra": "2*Stdev = 0.169039 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 52.91539285,
            "range": "1.24804",
            "unit": "ms",
            "extra": "2*Stdev = 1.24804 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.559624675,
            "range": "0.141699",
            "unit": "ms",
            "extra": "2*Stdev = 0.141699 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 11.466531225,
            "range": "0.66862",
            "unit": "ms",
            "extra": "2*Stdev = 0.66862 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.138027034,
            "range": "0.0105321",
            "unit": "ms",
            "extra": "2*Stdev = 0.0105321 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 297.6318608,
            "range": "4.70715",
            "unit": "ms",
            "extra": "2*Stdev = 4.70715 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 42.37172325,
            "range": "1.77138",
            "unit": "ms",
            "extra": "2*Stdev = 1.77138 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 34.5785062,
            "range": "2.75306",
            "unit": "ms",
            "extra": "2*Stdev = 2.75306 ms"
          },
          {
            "name": "Long variable names",
            "value": 27.0709698,
            "range": "2.00718",
            "unit": "ms",
            "extra": "2*Stdev = 2.00718 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 86.5883124,
            "range": "6.75597",
            "unit": "ms",
            "extra": "2*Stdev = 6.75597 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 202.0801584,
            "range": "15.267",
            "unit": "ms",
            "extra": "2*Stdev = 15.267 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 191.2109042,
            "range": "16.2017",
            "unit": "ms",
            "extra": "2*Stdev = 16.2017 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 184.8068738,
            "range": "12.318",
            "unit": "ms",
            "extra": "2*Stdev = 12.318 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 197.3907568,
            "range": "15.6005",
            "unit": "ms",
            "extra": "2*Stdev = 15.6005 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 190.7063442,
            "range": "10.8363",
            "unit": "ms",
            "extra": "2*Stdev = 10.8363 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1617.8052438,
            "range": "39.7751",
            "unit": "ms",
            "extra": "2*Stdev = 39.7751 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 366.2385788,
            "range": "32.6835",
            "unit": "ms",
            "extra": "2*Stdev = 32.6835 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 286.2067238,
            "range": "11.6992",
            "unit": "ms",
            "extra": "2*Stdev = 11.6992 ms"
          },
          {
            "name": "Whitespace",
            "value": 19.14377,
            "range": "1.17671",
            "unit": "ms",
            "extra": "2*Stdev = 1.17671 ms"
          },
          {
            "name": "Line comment",
            "value": 362.4503016,
            "range": "16.3003",
            "unit": "ms",
            "extra": "2*Stdev = 16.3003 ms"
          },
          {
            "name": "Block comment",
            "value": 315.3567894,
            "range": "26.8494",
            "unit": "ms",
            "extra": "2*Stdev = 26.8494 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1970.1129358,
            "range": "3.95667",
            "unit": "ms",
            "extra": "2*Stdev = 3.95667 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 2008.0214892,
            "range": "46.512",
            "unit": "ms",
            "extra": "2*Stdev = 46.512 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dd98c876b12b8ba36a5fa501c361216a95048991",
          "message": "Stop depending on unmaintained repline (#2643) (#2846)\n\n* Stop depending on unmaintained repline (#2643)\n\nrepline has not had a release since 2022. Keep dhall repl behavior by moving the Haskeline loop it used into Dhall.Repl.Line.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* update changelog\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-27T18:17:12+02:00",
          "tree_id": "1bf887eb65e31d6a5b72f95e780cb2315fb620db",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/dd98c876b12b8ba36a5fa501c361216a95048991"
        },
        "date": 1790526465125,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.05035189,
            "range": "0.00346391",
            "unit": "ms",
            "extra": "2*Stdev = 0.00346391 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.051008034,
            "range": "0.00296248",
            "unit": "ms",
            "extra": "2*Stdev = 0.00296248 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.032803121,
            "range": "0.00121359",
            "unit": "ms",
            "extra": "2*Stdev = 0.00121359 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 416.4309552,
            "range": "24.5723",
            "unit": "ms",
            "extra": "2*Stdev = 24.5723 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017447102,
            "range": "0.000957158",
            "unit": "ms",
            "extra": "2*Stdev = 0.000957158 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 309.1126008,
            "range": "20.6562",
            "unit": "ms",
            "extra": "2*Stdev = 20.6562 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045460187,
            "range": "0.00368353",
            "unit": "ms",
            "extra": "2*Stdev = 0.00368353 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 83.8166831,
            "range": "3.5584",
            "unit": "ms",
            "extra": "2*Stdev = 3.5584 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.01978995,
            "range": "0.00155138",
            "unit": "ms",
            "extra": "2*Stdev = 0.00155138 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 267.8069382,
            "range": "12.0089",
            "unit": "ms",
            "extra": "2*Stdev = 12.0089 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017045945,
            "range": "0.00135064",
            "unit": "ms",
            "extra": "2*Stdev = 0.00135064 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 208.4405152,
            "range": "11.964",
            "unit": "ms",
            "extra": "2*Stdev = 11.964 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.052739791,
            "range": "0.00328209",
            "unit": "ms",
            "extra": "2*Stdev = 0.00328209 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 187.3719348,
            "range": "5.16089",
            "unit": "ms",
            "extra": "2*Stdev = 5.16089 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047248156,
            "range": "0.00350104",
            "unit": "ms",
            "extra": "2*Stdev = 0.00350104 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 235.2611756,
            "range": "13.6671",
            "unit": "ms",
            "extra": "2*Stdev = 13.6671 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000482117,
            "range": "2.3242e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.3242e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010768414,
            "range": "0.000755366",
            "unit": "ms",
            "extra": "2*Stdev = 0.000755366 ms"
          },
          {
            "name": "large1.parse",
            "value": 313.4004068,
            "range": "8.54333",
            "unit": "ms",
            "extra": "2*Stdev = 8.54333 ms"
          },
          {
            "name": "large1.resolve",
            "value": 96.4965664,
            "range": "5.18906",
            "unit": "ms",
            "extra": "2*Stdev = 5.18906 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 73.612062,
            "range": "5.51909",
            "unit": "ms",
            "extra": "2*Stdev = 5.51909 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.4543708,
            "range": "4.67426",
            "unit": "ms",
            "extra": "2*Stdev = 4.67426 ms"
          },
          {
            "name": "large2.normalize",
            "value": 164.843044,
            "range": "4.18479",
            "unit": "ms",
            "extra": "2*Stdev = 4.18479 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 340.373851,
            "range": "3.89166",
            "unit": "ms",
            "extra": "2*Stdev = 3.89166 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 822.4007114,
            "range": "6.15288",
            "unit": "ms",
            "extra": "2*Stdev = 6.15288 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2686.5912366,
            "range": "139.31",
            "unit": "ms",
            "extra": "2*Stdev = 139.31 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 401.3733666,
            "range": "11.6249",
            "unit": "ms",
            "extra": "2*Stdev = 11.6249 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.676744525,
            "range": "0.027459",
            "unit": "ms",
            "extra": "2*Stdev = 0.027459 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1786.2279744,
            "range": "20.5513",
            "unit": "ms",
            "extra": "2*Stdev = 20.5513 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.4158284,
            "range": "4.6897",
            "unit": "ms",
            "extra": "2*Stdev = 4.6897 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.410411423,
            "range": "0.0232266",
            "unit": "ms",
            "extra": "2*Stdev = 0.0232266 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2703.6424921,
            "range": "268.128",
            "unit": "ms",
            "extra": "2*Stdev = 268.128 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4992.9444236,
            "range": "38.2832",
            "unit": "ms",
            "extra": "2*Stdev = 38.2832 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 712.2641304,
            "range": "15.6361",
            "unit": "ms",
            "extra": "2*Stdev = 15.6361 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2834.02430425,
            "range": "223.07",
            "unit": "ms",
            "extra": "2*Stdev = 223.07 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5459.061933,
            "range": "6.53828",
            "unit": "ms",
            "extra": "2*Stdev = 6.53828 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.265061733,
            "range": "0.0204609",
            "unit": "ms",
            "extra": "2*Stdev = 0.0204609 ms"
          },
          {
            "name": "large4.resolve",
            "value": 529.3552622,
            "range": "10.4107",
            "unit": "ms",
            "extra": "2*Stdev = 10.4107 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 448.6072812,
            "range": "8.00987",
            "unit": "ms",
            "extra": "2*Stdev = 8.00987 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 2.002216225,
            "range": "0.0609534",
            "unit": "ms",
            "extra": "2*Stdev = 0.0609534 ms"
          },
          {
            "name": "large5.resolve",
            "value": 209.7218924,
            "range": "8.32192",
            "unit": "ms",
            "extra": "2*Stdev = 8.32192 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 70.0599362,
            "range": "3.54488",
            "unit": "ms",
            "extra": "2*Stdev = 3.54488 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 74.649385,
            "range": "4.15509",
            "unit": "ms",
            "extra": "2*Stdev = 4.15509 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 860.4361772,
            "range": "14.9005",
            "unit": "ms",
            "extra": "2*Stdev = 14.9005 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000216036,
            "range": "1.3004e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.3004e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000224424,
            "range": "1.14e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.14e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 903.9871956,
            "range": "13.6917",
            "unit": "ms",
            "extra": "2*Stdev = 13.6917 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.273427481,
            "range": "0.0978504",
            "unit": "ms",
            "extra": "2*Stdev = 0.0978504 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.263274837,
            "range": "0.223006",
            "unit": "ms",
            "extra": "2*Stdev = 0.223006 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.73966995,
            "range": "0.111031",
            "unit": "ms",
            "extra": "2*Stdev = 0.111031 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 555.3476074,
            "range": "10.9465",
            "unit": "ms",
            "extra": "2*Stdev = 10.9465 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 481.1561416,
            "range": "22.748",
            "unit": "ms",
            "extra": "2*Stdev = 22.748 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 394.2983068,
            "range": "3.90296",
            "unit": "ms",
            "extra": "2*Stdev = 3.90296 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 458.3798902,
            "range": "13.8015",
            "unit": "ms",
            "extra": "2*Stdev = 13.8015 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 317.9286952,
            "range": "10.2849",
            "unit": "ms",
            "extra": "2*Stdev = 10.2849 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 89.0118141,
            "range": "4.94981",
            "unit": "ms",
            "extra": "2*Stdev = 4.94981 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 1370.6550016,
            "range": "7.85453",
            "unit": "ms",
            "extra": "2*Stdev = 7.85453 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 595.2434174,
            "range": "10.8776",
            "unit": "ms",
            "extra": "2*Stdev = 10.8776 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 222.5654888,
            "range": "5.09517",
            "unit": "ms",
            "extra": "2*Stdev = 5.09517 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.6649485,
            "range": "1.53987",
            "unit": "ms",
            "extra": "2*Stdev = 1.53987 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.892005737,
            "range": "0.172888",
            "unit": "ms",
            "extra": "2*Stdev = 0.172888 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 49.1271411,
            "range": "3.31627",
            "unit": "ms",
            "extra": "2*Stdev = 3.31627 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.588798487,
            "range": "0.0729641",
            "unit": "ms",
            "extra": "2*Stdev = 0.0729641 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 11.498382125,
            "range": "0.42255",
            "unit": "ms",
            "extra": "2*Stdev = 0.42255 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.14002214,
            "range": "0.00854894",
            "unit": "ms",
            "extra": "2*Stdev = 0.00854894 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 300.3420901,
            "range": "3.22268",
            "unit": "ms",
            "extra": "2*Stdev = 3.22268 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 42.8672454,
            "range": "2.56272",
            "unit": "ms",
            "extra": "2*Stdev = 2.56272 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 35.7213642,
            "range": "2.55442",
            "unit": "ms",
            "extra": "2*Stdev = 2.55442 ms"
          },
          {
            "name": "Long variable names",
            "value": 28.8689613,
            "range": "1.21134",
            "unit": "ms",
            "extra": "2*Stdev = 1.21134 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 87.9046076,
            "range": "6.68186",
            "unit": "ms",
            "extra": "2*Stdev = 6.68186 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 204.08123305,
            "range": "9.04703",
            "unit": "ms",
            "extra": "2*Stdev = 9.04703 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 193.216724,
            "range": "7.2576",
            "unit": "ms",
            "extra": "2*Stdev = 7.2576 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 182.7337618,
            "range": "11.0526",
            "unit": "ms",
            "extra": "2*Stdev = 11.0526 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 202.35828555,
            "range": "3.56309",
            "unit": "ms",
            "extra": "2*Stdev = 3.56309 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 200.85988805,
            "range": "10.9888",
            "unit": "ms",
            "extra": "2*Stdev = 10.9888 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1613.3920626,
            "range": "40.8789",
            "unit": "ms",
            "extra": "2*Stdev = 40.8789 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 376.6328666,
            "range": "8.35398",
            "unit": "ms",
            "extra": "2*Stdev = 8.35398 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 290.446164,
            "range": "17.1722",
            "unit": "ms",
            "extra": "2*Stdev = 17.1722 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.14056265,
            "range": "0.709552",
            "unit": "ms",
            "extra": "2*Stdev = 0.709552 ms"
          },
          {
            "name": "Line comment",
            "value": 371.5685102,
            "range": "3.45187",
            "unit": "ms",
            "extra": "2*Stdev = 3.45187 ms"
          },
          {
            "name": "Block comment",
            "value": 344.1270816,
            "range": "10.0009",
            "unit": "ms",
            "extra": "2*Stdev = 10.0009 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1981.1017306,
            "range": "11.3205",
            "unit": "ms",
            "extra": "2*Stdev = 11.3205 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 2035.6009772,
            "range": "7.3904",
            "unit": "ms",
            "extra": "2*Stdev = 7.3904 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "49699333+dependabot[bot]@users.noreply.github.com",
            "name": "dependabot[bot]",
            "username": "dependabot[bot]"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "aa7d8bdecefe488218aa0e188bf7960de3aa0cd9",
          "message": "Bump renovatebot/github-action from 46.3.1 to 46.3.4 (#2847)\n\nBumps [renovatebot/github-action](https://github.com/renovatebot/github-action) from 46.3.1 to 46.3.4.\n- [Release notes](https://github.com/renovatebot/github-action/releases)\n- [Changelog](https://github.com/renovatebot/github-action/blob/main/CHANGELOG.md)\n- [Commits](https://github.com/renovatebot/github-action/compare/v46.3.1...v46.3.4)\n\n---\nupdated-dependencies:\n- dependency-name: renovatebot/github-action\n  dependency-version: 46.3.4\n  dependency-type: direct:production\n  update-type: version-update:semver-patch\n...\n\nSigned-off-by: dependabot[bot] <support@github.com>\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
          "timestamp": "2026-09-28T16:34:51+02:00",
          "tree_id": "28753e1b3593b6e89c631f7020b0dec7e95fff42",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/aa7d8bdecefe488218aa0e188bf7960de3aa0cd9"
        },
        "date": 1790606619808,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.03561678,
            "range": "0.00316528",
            "unit": "ms",
            "extra": "2*Stdev = 0.00316528 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.035813438,
            "range": "0.00280228",
            "unit": "ms",
            "extra": "2*Stdev = 0.00280228 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.020500759,
            "range": "0.00165734",
            "unit": "ms",
            "extra": "2*Stdev = 0.00165734 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 254.8711122,
            "range": "17.5301",
            "unit": "ms",
            "extra": "2*Stdev = 17.5301 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.010224039,
            "range": "0.000669762",
            "unit": "ms",
            "extra": "2*Stdev = 0.000669762 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 231.2099028,
            "range": "4.51955",
            "unit": "ms",
            "extra": "2*Stdev = 4.51955 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.036617552,
            "range": "0.0031615",
            "unit": "ms",
            "extra": "2*Stdev = 0.0031615 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 73.8150126,
            "range": "4.84717",
            "unit": "ms",
            "extra": "2*Stdev = 4.84717 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.01281333,
            "range": "0.000757222",
            "unit": "ms",
            "extra": "2*Stdev = 0.000757222 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 209.3288048,
            "range": "2.71535",
            "unit": "ms",
            "extra": "2*Stdev = 2.71535 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.01071647,
            "range": "0.000806886",
            "unit": "ms",
            "extra": "2*Stdev = 0.000806886 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 169.326232,
            "range": "4.79845",
            "unit": "ms",
            "extra": "2*Stdev = 4.79845 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.038120335,
            "range": "0.00345656",
            "unit": "ms",
            "extra": "2*Stdev = 0.00345656 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 144.9216538,
            "range": "5.59288",
            "unit": "ms",
            "extra": "2*Stdev = 5.59288 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.033959943,
            "range": "0.00317668",
            "unit": "ms",
            "extra": "2*Stdev = 0.00317668 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 171.1603708,
            "range": "3.50991",
            "unit": "ms",
            "extra": "2*Stdev = 3.50991 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000317621,
            "range": "2.5934e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.5934e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.00735696,
            "range": "0.000711646",
            "unit": "ms",
            "extra": "2*Stdev = 0.000711646 ms"
          },
          {
            "name": "large1.parse",
            "value": 215.0532496,
            "range": "2.88162",
            "unit": "ms",
            "extra": "2*Stdev = 2.88162 ms"
          },
          {
            "name": "large1.resolve",
            "value": 66.88859,
            "range": "1.52191",
            "unit": "ms",
            "extra": "2*Stdev = 1.52191 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 55.3949124,
            "range": "2.83266",
            "unit": "ms",
            "extra": "2*Stdev = 2.83266 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 111.5282046,
            "range": "5.57501",
            "unit": "ms",
            "extra": "2*Stdev = 5.57501 ms"
          },
          {
            "name": "large2.normalize",
            "value": 119.9464916,
            "range": "4.12172",
            "unit": "ms",
            "extra": "2*Stdev = 4.12172 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 256.4322156,
            "range": "5.20952",
            "unit": "ms",
            "extra": "2*Stdev = 5.20952 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 659.5028894,
            "range": "18.3386",
            "unit": "ms",
            "extra": "2*Stdev = 18.3386 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2026.3742259,
            "range": "108.613",
            "unit": "ms",
            "extra": "2*Stdev = 108.613 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 289.5791086,
            "range": "4.5552",
            "unit": "ms",
            "extra": "2*Stdev = 4.5552 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.526243793,
            "range": "0.0218894",
            "unit": "ms",
            "extra": "2*Stdev = 0.0218894 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1318.800129,
            "range": "6.22351",
            "unit": "ms",
            "extra": "2*Stdev = 6.22351 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 59.0415011,
            "range": "4.2855",
            "unit": "ms",
            "extra": "2*Stdev = 4.2855 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.290133254,
            "range": "0.0289735",
            "unit": "ms",
            "extra": "2*Stdev = 0.0289735 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2109.6721719,
            "range": "136.507",
            "unit": "ms",
            "extra": "2*Stdev = 136.507 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3848.389931,
            "range": "11.7534",
            "unit": "ms",
            "extra": "2*Stdev = 11.7534 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 548.7730538,
            "range": "19.2713",
            "unit": "ms",
            "extra": "2*Stdev = 19.2713 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2093.1206527,
            "range": "150.015",
            "unit": "ms",
            "extra": "2*Stdev = 150.015 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 3874.3099408,
            "range": "17.2152",
            "unit": "ms",
            "extra": "2*Stdev = 17.2152 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.16468074,
            "range": "0.0147213",
            "unit": "ms",
            "extra": "2*Stdev = 0.0147213 ms"
          },
          {
            "name": "large4.resolve",
            "value": 343.2341398,
            "range": "2.9947",
            "unit": "ms",
            "extra": "2*Stdev = 2.9947 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 324.7472474,
            "range": "3.72102",
            "unit": "ms",
            "extra": "2*Stdev = 3.72102 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.489188131,
            "range": "0.0452033",
            "unit": "ms",
            "extra": "2*Stdev = 0.0452033 ms"
          },
          {
            "name": "large5.resolve",
            "value": 135.6531434,
            "range": "4.6271",
            "unit": "ms",
            "extra": "2*Stdev = 4.6271 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 46.8740087,
            "range": "1.38334",
            "unit": "ms",
            "extra": "2*Stdev = 1.38334 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 57.1432952,
            "range": "3.60797",
            "unit": "ms",
            "extra": "2*Stdev = 3.60797 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 573.226118,
            "range": "6.46282",
            "unit": "ms",
            "extra": "2*Stdev = 6.46282 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000147602,
            "range": "7.138e-06",
            "unit": "ms",
            "extra": "2*Stdev = 7.138e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000162869,
            "range": "1.0852e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.0852e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 541.5874992,
            "range": "5.3811",
            "unit": "ms",
            "extra": "2*Stdev = 5.3811 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.759847184,
            "range": "0.0644548",
            "unit": "ms",
            "extra": "2*Stdev = 0.0644548 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.295633281,
            "range": "0.109579",
            "unit": "ms",
            "extra": "2*Stdev = 0.109579 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.122068243,
            "range": "0.110642",
            "unit": "ms",
            "extra": "2*Stdev = 0.110642 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 391.2942216,
            "range": "10.2634",
            "unit": "ms",
            "extra": "2*Stdev = 10.2634 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 356.2321192,
            "range": "3.48561",
            "unit": "ms",
            "extra": "2*Stdev = 3.48561 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 291.9903972,
            "range": "4.43914",
            "unit": "ms",
            "extra": "2*Stdev = 4.43914 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 319.6695588,
            "range": "4.16102",
            "unit": "ms",
            "extra": "2*Stdev = 4.16102 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 190.0581512,
            "range": "5.66841",
            "unit": "ms",
            "extra": "2*Stdev = 5.66841 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 56.2962512,
            "range": "1.36142",
            "unit": "ms",
            "extra": "2*Stdev = 1.36142 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 884.5277254,
            "range": "18.7058",
            "unit": "ms",
            "extra": "2*Stdev = 18.7058 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 414.5733504,
            "range": "5.82628",
            "unit": "ms",
            "extra": "2*Stdev = 5.82628 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 166.3201466,
            "range": "4.67513",
            "unit": "ms",
            "extra": "2*Stdev = 4.67513 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 18.1079631,
            "range": "0.945382",
            "unit": "ms",
            "extra": "2*Stdev = 0.945382 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.3494291,
            "range": "0.119625",
            "unit": "ms",
            "extra": "2*Stdev = 0.119625 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 36.4093408,
            "range": "3.30878",
            "unit": "ms",
            "extra": "2*Stdev = 3.30878 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.081308712,
            "range": "0.0910645",
            "unit": "ms",
            "extra": "2*Stdev = 0.0910645 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 8.00103125,
            "range": "0.782429",
            "unit": "ms",
            "extra": "2*Stdev = 0.782429 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.101827785,
            "range": "0.00936743",
            "unit": "ms",
            "extra": "2*Stdev = 0.00936743 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 228.4915461,
            "range": "9.01505",
            "unit": "ms",
            "extra": "2*Stdev = 9.01505 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 27.4510603,
            "range": "2.67517",
            "unit": "ms",
            "extra": "2*Stdev = 2.67517 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 23.6907973,
            "range": "1.55877",
            "unit": "ms",
            "extra": "2*Stdev = 1.55877 ms"
          },
          {
            "name": "Long variable names",
            "value": 18.5133498,
            "range": "1.733",
            "unit": "ms",
            "extra": "2*Stdev = 1.733 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 51.9490406,
            "range": "4.04528",
            "unit": "ms",
            "extra": "2*Stdev = 4.04528 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 129.1214091,
            "range": "2.74533",
            "unit": "ms",
            "extra": "2*Stdev = 2.74533 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 124.0184746,
            "range": "11.2566",
            "unit": "ms",
            "extra": "2*Stdev = 11.2566 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 122.6733082,
            "range": "5.78939",
            "unit": "ms",
            "extra": "2*Stdev = 5.78939 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 130.0752068,
            "range": "10.6255",
            "unit": "ms",
            "extra": "2*Stdev = 10.6255 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 126.361164,
            "range": "10.2667",
            "unit": "ms",
            "extra": "2*Stdev = 10.2667 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1199.0138402,
            "range": "28.5035",
            "unit": "ms",
            "extra": "2*Stdev = 28.5035 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 236.3190874,
            "range": "10.0438",
            "unit": "ms",
            "extra": "2*Stdev = 10.0438 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 174.8009514,
            "range": "5.23353",
            "unit": "ms",
            "extra": "2*Stdev = 5.23353 ms"
          },
          {
            "name": "Whitespace",
            "value": 12.5131884,
            "range": "0.705947",
            "unit": "ms",
            "extra": "2*Stdev = 0.705947 ms"
          },
          {
            "name": "Line comment",
            "value": 231.1438838,
            "range": "4.17238",
            "unit": "ms",
            "extra": "2*Stdev = 4.17238 ms"
          },
          {
            "name": "Block comment",
            "value": 203.8045094,
            "range": "4.53675",
            "unit": "ms",
            "extra": "2*Stdev = 4.53675 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1349.297681,
            "range": "21.6988",
            "unit": "ms",
            "extra": "2*Stdev = 21.6988 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1380.261371,
            "range": "28.1584",
            "unit": "ms",
            "extra": "2*Stdev = 28.1584 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "janosch.kindl@natuvion.com",
            "name": "kindlnatuvion",
            "username": "kindlnatuvion"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d3cbce36f3ceb437e242ff1be8972465302558dd",
          "message": "Demonstration benchmarks for performance regression introduced with (#2808) (#2848)\n\n* Create benchmark for diamond pattern imports\n\nCleanup\n\n* Add benchmark for transitive diamond imports\n\nCleanup2\n\n---------\n\nCo-authored-by: Sergei Winitzki <winitzki@users.noreply.github.com>",
          "timestamp": "2026-09-28T17:26:40+02:00",
          "tree_id": "b7a96e2ae203af109d7bae3579ca1943695ca11e",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/d3cbce36f3ceb437e242ff1be8972465302558dd"
        },
        "date": 1790609727794,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.025280625,
            "range": "0.00174727",
            "unit": "ms",
            "extra": "2*Stdev = 0.00174727 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.026074858,
            "range": "0.00195357",
            "unit": "ms",
            "extra": "2*Stdev = 0.00195357 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.014608539,
            "range": "0.00131206",
            "unit": "ms",
            "extra": "2*Stdev = 0.00131206 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 200.9155204,
            "range": "7.1125",
            "unit": "ms",
            "extra": "2*Stdev = 7.1125 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.006875343,
            "range": "0.000655276",
            "unit": "ms",
            "extra": "2*Stdev = 0.000655276 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 186.5364952,
            "range": "10.4575",
            "unit": "ms",
            "extra": "2*Stdev = 10.4575 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.021372068,
            "range": "0.000980918",
            "unit": "ms",
            "extra": "2*Stdev = 0.000980918 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 47.9874009,
            "range": "1.44218",
            "unit": "ms",
            "extra": "2*Stdev = 1.44218 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.007458441,
            "range": "0.000542394",
            "unit": "ms",
            "extra": "2*Stdev = 0.000542394 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 191.4866602,
            "range": "11.5438",
            "unit": "ms",
            "extra": "2*Stdev = 11.5438 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.00620787,
            "range": "0.000378192",
            "unit": "ms",
            "extra": "2*Stdev = 0.000378192 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 146.4492364,
            "range": "6.00047",
            "unit": "ms",
            "extra": "2*Stdev = 6.00047 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.025356216,
            "range": "0.00186417",
            "unit": "ms",
            "extra": "2*Stdev = 0.00186417 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 129.9021792,
            "range": "8.72529",
            "unit": "ms",
            "extra": "2*Stdev = 8.72529 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.021000348,
            "range": "0.00151752",
            "unit": "ms",
            "extra": "2*Stdev = 0.00151752 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 170.7056486,
            "range": "16.1857",
            "unit": "ms",
            "extra": "2*Stdev = 16.1857 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000207957,
            "range": "1.0268e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.0268e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.005377634,
            "range": "0.000340768",
            "unit": "ms",
            "extra": "2*Stdev = 0.000340768 ms"
          },
          {
            "name": "large1.parse",
            "value": 150.8024969,
            "range": "1.38784",
            "unit": "ms",
            "extra": "2*Stdev = 1.38784 ms"
          },
          {
            "name": "large1.resolve",
            "value": 53.38428935,
            "range": "1.1618",
            "unit": "ms",
            "extra": "2*Stdev = 1.1618 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 43.5070998,
            "range": "4.23813",
            "unit": "ms",
            "extra": "2*Stdev = 4.23813 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 78.4281862,
            "range": "2.8178",
            "unit": "ms",
            "extra": "2*Stdev = 2.8178 ms"
          },
          {
            "name": "large2.normalize",
            "value": 82.2750548,
            "range": "3.55845",
            "unit": "ms",
            "extra": "2*Stdev = 3.55845 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 196.8989652,
            "range": "3.06198",
            "unit": "ms",
            "extra": "2*Stdev = 3.06198 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 438.9865282,
            "range": "12.116",
            "unit": "ms",
            "extra": "2*Stdev = 12.116 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 1701.9035441,
            "range": "69.166",
            "unit": "ms",
            "extra": "2*Stdev = 69.166 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 300.142037,
            "range": "5.52403",
            "unit": "ms",
            "extra": "2*Stdev = 5.52403 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.359968514,
            "range": "0.0314841",
            "unit": "ms",
            "extra": "2*Stdev = 0.0314841 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1028.4992366,
            "range": "40.7446",
            "unit": "ms",
            "extra": "2*Stdev = 40.7446 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 80.2515315,
            "range": "5.40722",
            "unit": "ms",
            "extra": "2*Stdev = 5.40722 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.202837292,
            "range": "0.0149952",
            "unit": "ms",
            "extra": "2*Stdev = 0.0149952 ms"
          },
          {
            "name": "large3.resolve",
            "value": 1710.4247091,
            "range": "156.936",
            "unit": "ms",
            "extra": "2*Stdev = 156.936 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 3782.9777416,
            "range": "14.48",
            "unit": "ms",
            "extra": "2*Stdev = 14.48 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 697.9740181,
            "range": "14.0711",
            "unit": "ms",
            "extra": "2*Stdev = 14.0711 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 1619.7647856,
            "range": "79.8017",
            "unit": "ms",
            "extra": "2*Stdev = 79.8017 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 3297.1453342,
            "range": "75.3786",
            "unit": "ms",
            "extra": "2*Stdev = 75.3786 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.124233173,
            "range": "0.010814",
            "unit": "ms",
            "extra": "2*Stdev = 0.010814 ms"
          },
          {
            "name": "large4.resolve",
            "value": 236.2747962,
            "range": "7.8595",
            "unit": "ms",
            "extra": "2*Stdev = 7.8595 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 245.8579044,
            "range": "9.06763",
            "unit": "ms",
            "extra": "2*Stdev = 9.06763 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.008465359,
            "range": "0.0808645",
            "unit": "ms",
            "extra": "2*Stdev = 0.0808645 ms"
          },
          {
            "name": "large5.resolve",
            "value": 93.1908468,
            "range": "5.41018",
            "unit": "ms",
            "extra": "2*Stdev = 5.41018 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 38.286934,
            "range": "2.86258",
            "unit": "ms",
            "extra": "2*Stdev = 2.86258 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 41.6562858,
            "range": "3.65143",
            "unit": "ms",
            "extra": "2*Stdev = 3.65143 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 427.3533728,
            "range": "6.83765",
            "unit": "ms",
            "extra": "2*Stdev = 6.83765 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000094393,
            "range": "8.334e-06",
            "unit": "ms",
            "extra": "2*Stdev = 8.334e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000099355,
            "range": "8.648e-06",
            "unit": "ms",
            "extra": "2*Stdev = 8.648e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 378.0839212,
            "range": "31.8758",
            "unit": "ms",
            "extra": "2*Stdev = 31.8758 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.586709918,
            "range": "0.0443655",
            "unit": "ms",
            "extra": "2*Stdev = 0.0443655 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.056944693,
            "range": "0.0858801",
            "unit": "ms",
            "extra": "2*Stdev = 0.0858801 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 0.990208312,
            "range": "0.0657007",
            "unit": "ms",
            "extra": "2*Stdev = 0.0657007 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 260.416947,
            "range": "16.7356",
            "unit": "ms",
            "extra": "2*Stdev = 16.7356 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 222.3927746,
            "range": "19.232",
            "unit": "ms",
            "extra": "2*Stdev = 19.232 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 174.6018616,
            "range": "3.96006",
            "unit": "ms",
            "extra": "2*Stdev = 3.96006 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 234.9986195,
            "range": "2.1589",
            "unit": "ms",
            "extra": "2*Stdev = 2.1589 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 1287.6727808,
            "range": "122.115",
            "unit": "ms",
            "extra": "2*Stdev = 122.115 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 1323.0897286,
            "range": "77.8594",
            "unit": "ms",
            "extra": "2*Stdev = 77.8594 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 117.7655866,
            "range": "6.27739",
            "unit": "ms",
            "extra": "2*Stdev = 6.27739 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 47.193412,
            "range": "3.30331",
            "unit": "ms",
            "extra": "2*Stdev = 3.30331 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 592.9250956,
            "range": "18.2536",
            "unit": "ms",
            "extra": "2*Stdev = 18.2536 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 277.5749786,
            "range": "17.5796",
            "unit": "ms",
            "extra": "2*Stdev = 17.5796 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 106.7096296,
            "range": "5.06454",
            "unit": "ms",
            "extra": "2*Stdev = 5.06454 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 9.70982715,
            "range": "0.75289",
            "unit": "ms",
            "extra": "2*Stdev = 0.75289 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 0.831937406,
            "range": "0.0413776",
            "unit": "ms",
            "extra": "2*Stdev = 0.0413776 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 23.89398915,
            "range": "0.984746",
            "unit": "ms",
            "extra": "2*Stdev = 0.984746 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 0.715478737,
            "range": "0.0702079",
            "unit": "ms",
            "extra": "2*Stdev = 0.0702079 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.664262062,
            "range": "0.538465",
            "unit": "ms",
            "extra": "2*Stdev = 0.538465 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.066728555,
            "range": "0.00555269",
            "unit": "ms",
            "extra": "2*Stdev = 0.00555269 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 192.2341247,
            "range": "6.50019",
            "unit": "ms",
            "extra": "2*Stdev = 6.50019 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 20.0570703,
            "range": "1.49035",
            "unit": "ms",
            "extra": "2*Stdev = 1.49035 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 18.304332625,
            "range": "1.43065",
            "unit": "ms",
            "extra": "2*Stdev = 1.43065 ms"
          },
          {
            "name": "Long variable names",
            "value": 14.09822665,
            "range": "0.747691",
            "unit": "ms",
            "extra": "2*Stdev = 0.747691 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 39.8825393,
            "range": "1.53546",
            "unit": "ms",
            "extra": "2*Stdev = 1.53546 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 95.5664845,
            "range": "2.56781",
            "unit": "ms",
            "extra": "2*Stdev = 2.56781 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 95.0986694,
            "range": "9.11523",
            "unit": "ms",
            "extra": "2*Stdev = 9.11523 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 94.4888938,
            "range": "5.15742",
            "unit": "ms",
            "extra": "2*Stdev = 5.15742 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 98.2921761,
            "range": "2.69082",
            "unit": "ms",
            "extra": "2*Stdev = 2.69082 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 91.99411375,
            "range": "3.36905",
            "unit": "ms",
            "extra": "2*Stdev = 3.36905 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 883.6885084,
            "range": "36.6426",
            "unit": "ms",
            "extra": "2*Stdev = 36.6426 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 174.8547104,
            "range": "8.4165",
            "unit": "ms",
            "extra": "2*Stdev = 8.4165 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 125.6381471,
            "range": "11.2502",
            "unit": "ms",
            "extra": "2*Stdev = 11.2502 ms"
          },
          {
            "name": "Whitespace",
            "value": 9.51922005,
            "range": "0.511032",
            "unit": "ms",
            "extra": "2*Stdev = 0.511032 ms"
          },
          {
            "name": "Line comment",
            "value": 185.6593248,
            "range": "17.8421",
            "unit": "ms",
            "extra": "2*Stdev = 17.8421 ms"
          },
          {
            "name": "Block comment",
            "value": 162.8908382,
            "range": "3.02072",
            "unit": "ms",
            "extra": "2*Stdev = 3.02072 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1017.1528184,
            "range": "96.0439",
            "unit": "ms",
            "extra": "2*Stdev = 96.0439 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1041.8652152,
            "range": "21.1212",
            "unit": "ms",
            "extra": "2*Stdev = 21.1212 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4d2f719d91a9de45fe1283cd675a9b608d4d4d0d",
          "message": "Faster megaparsec parsing (#2844)\n\n* Add a first-character fast path for Megaparsec whitespace\n\nDispatch whitespaceChunk on the next character so failed\nattempts do not run the four-way choice. Keep skipMany so\nerror hints still include whitespace.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Dispatch primitive expressions on the first character\n\nAvoid trying numeric, text, record, list, and identifier\nalternatives that cannot start with the next character.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Skip operator precedence layers when the next token cannot be an operator\n\nPeek after whitespace so delimiters do not try every operator\nparser, and dispatch each operator on its first character.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-28T16:11:22Z",
          "tree_id": "ba9c087689e49daf0ba5ed3c7f149344cd1b5d13",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/4d2f719d91a9de45fe1283cd675a9b608d4d4d0d"
        },
        "date": 1790612532345,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052878518,
            "range": "0.00398586",
            "unit": "ms",
            "extra": "2*Stdev = 0.00398586 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.052657431,
            "range": "0.00274496",
            "unit": "ms",
            "extra": "2*Stdev = 0.00274496 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033700338,
            "range": "0.00313039",
            "unit": "ms",
            "extra": "2*Stdev = 0.00313039 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 421.9358144,
            "range": "19.8941",
            "unit": "ms",
            "extra": "2*Stdev = 19.8941 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.01777948,
            "range": "0.00172906",
            "unit": "ms",
            "extra": "2*Stdev = 0.00172906 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 312.7637616,
            "range": "4.25896",
            "unit": "ms",
            "extra": "2*Stdev = 4.25896 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045689006,
            "range": "0.00284161",
            "unit": "ms",
            "extra": "2*Stdev = 0.00284161 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 83.2206586,
            "range": "3.78955",
            "unit": "ms",
            "extra": "2*Stdev = 3.78955 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.019937186,
            "range": "0.00156767",
            "unit": "ms",
            "extra": "2*Stdev = 0.00156767 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 263.5372042,
            "range": "3.66848",
            "unit": "ms",
            "extra": "2*Stdev = 3.66848 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017540372,
            "range": "0.00143574",
            "unit": "ms",
            "extra": "2*Stdev = 0.00143574 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 205.5937894,
            "range": "4.02252",
            "unit": "ms",
            "extra": "2*Stdev = 4.02252 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053593954,
            "range": "0.00333833",
            "unit": "ms",
            "extra": "2*Stdev = 0.00333833 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 188.3517842,
            "range": "5.84251",
            "unit": "ms",
            "extra": "2*Stdev = 5.84251 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.049085239,
            "range": "0.00423327",
            "unit": "ms",
            "extra": "2*Stdev = 0.00423327 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 245.2869084,
            "range": "4.56503",
            "unit": "ms",
            "extra": "2*Stdev = 4.56503 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000504745,
            "range": "3.3226e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.3226e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010984821,
            "range": "0.000552168",
            "unit": "ms",
            "extra": "2*Stdev = 0.000552168 ms"
          },
          {
            "name": "large1.parse",
            "value": 168.2668782,
            "range": "3.27977",
            "unit": "ms",
            "extra": "2*Stdev = 3.27977 ms"
          },
          {
            "name": "large1.resolve",
            "value": 60.8799318,
            "range": "4.49393",
            "unit": "ms",
            "extra": "2*Stdev = 4.49393 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 74.1715064,
            "range": "4.52988",
            "unit": "ms",
            "extra": "2*Stdev = 4.52988 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.6228484,
            "range": "4.32733",
            "unit": "ms",
            "extra": "2*Stdev = 4.32733 ms"
          },
          {
            "name": "large2.normalize",
            "value": 164.1083026,
            "range": "2.95864",
            "unit": "ms",
            "extra": "2*Stdev = 2.95864 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 340.678949,
            "range": "6.59922",
            "unit": "ms",
            "extra": "2*Stdev = 6.59922 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 814.823225,
            "range": "11.8404",
            "unit": "ms",
            "extra": "2*Stdev = 11.8404 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2327.5253268,
            "range": "111.386",
            "unit": "ms",
            "extra": "2*Stdev = 111.386 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 402.9903454,
            "range": "15.1157",
            "unit": "ms",
            "extra": "2*Stdev = 15.1157 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.671240718,
            "range": "0.0441602",
            "unit": "ms",
            "extra": "2*Stdev = 0.0441602 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1448.5645232,
            "range": "13.6573",
            "unit": "ms",
            "extra": "2*Stdev = 13.6573 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 92.1519099,
            "range": "8.25768",
            "unit": "ms",
            "extra": "2*Stdev = 8.25768 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.41248315,
            "range": "0.021329",
            "unit": "ms",
            "extra": "2*Stdev = 0.021329 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2431.3036873,
            "range": "119.993",
            "unit": "ms",
            "extra": "2*Stdev = 119.993 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4883.8345386,
            "range": "15.5165",
            "unit": "ms",
            "extra": "2*Stdev = 15.5165 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 691.9522902,
            "range": "14.0211",
            "unit": "ms",
            "extra": "2*Stdev = 14.0211 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2428.99196215,
            "range": "101.402",
            "unit": "ms",
            "extra": "2*Stdev = 101.402 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5068.489751,
            "range": "35.689",
            "unit": "ms",
            "extra": "2*Stdev = 35.689 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.254126285,
            "range": "0.017891",
            "unit": "ms",
            "extra": "2*Stdev = 0.017891 ms"
          },
          {
            "name": "large4.resolve",
            "value": 311.0439776,
            "range": "3.86584",
            "unit": "ms",
            "extra": "2*Stdev = 3.86584 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 435.8057142,
            "range": "2.91574",
            "unit": "ms",
            "extra": "2*Stdev = 2.91574 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.924737503,
            "range": "0.0995248",
            "unit": "ms",
            "extra": "2*Stdev = 0.0995248 ms"
          },
          {
            "name": "large5.resolve",
            "value": 130.7653956,
            "range": "5.30935",
            "unit": "ms",
            "extra": "2*Stdev = 5.30935 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.2612006,
            "range": "5.54675",
            "unit": "ms",
            "extra": "2*Stdev = 5.54675 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.0315072,
            "range": "5.80593",
            "unit": "ms",
            "extra": "2*Stdev = 5.80593 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 845.4101956,
            "range": "8.6444",
            "unit": "ms",
            "extra": "2*Stdev = 8.6444 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000217892,
            "range": "1.2944e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.2944e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000212453,
            "range": "1.7316e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.7316e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 403.931002,
            "range": "20.6261",
            "unit": "ms",
            "extra": "2*Stdev = 20.6261 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.248981006,
            "range": "0.0860973",
            "unit": "ms",
            "extra": "2*Stdev = 0.0860973 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.327070987,
            "range": "0.227268",
            "unit": "ms",
            "extra": "2*Stdev = 0.227268 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.314770243,
            "range": "0.0847856",
            "unit": "ms",
            "extra": "2*Stdev = 0.0847856 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 585.5327164,
            "range": "11.7793",
            "unit": "ms",
            "extra": "2*Stdev = 11.7793 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 507.9221642,
            "range": "16.4682",
            "unit": "ms",
            "extra": "2*Stdev = 16.4682 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 403.108388,
            "range": "4.50148",
            "unit": "ms",
            "extra": "2*Stdev = 4.50148 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 319.6379964,
            "range": "11.4158",
            "unit": "ms",
            "extra": "2*Stdev = 11.4158 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 3142.703489,
            "range": "11.475",
            "unit": "ms",
            "extra": "2*Stdev = 11.475 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3310.7012114,
            "range": "129.636",
            "unit": "ms",
            "extra": "2*Stdev = 129.636 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 177.5660428,
            "range": "5.35856",
            "unit": "ms",
            "extra": "2*Stdev = 5.35856 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 70.0632577,
            "range": "2.0419",
            "unit": "ms",
            "extra": "2*Stdev = 2.0419 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 819.7943156,
            "range": "4.90553",
            "unit": "ms",
            "extra": "2*Stdev = 4.90553 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 434.5150646,
            "range": "3.0982",
            "unit": "ms",
            "extra": "2*Stdev = 3.0982 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 195.8341126,
            "range": "5.07025",
            "unit": "ms",
            "extra": "2*Stdev = 5.07025 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 21.6401922,
            "range": "1.46895",
            "unit": "ms",
            "extra": "2*Stdev = 1.46895 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.807022425,
            "range": "0.168403",
            "unit": "ms",
            "extra": "2*Stdev = 0.168403 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 48.55998155,
            "range": "3.10742",
            "unit": "ms",
            "extra": "2*Stdev = 3.10742 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.308545759,
            "range": "0.0273571",
            "unit": "ms",
            "extra": "2*Stdev = 0.0273571 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.574608837,
            "range": "0.456359",
            "unit": "ms",
            "extra": "2*Stdev = 0.456359 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.145975651,
            "range": "0.0136728",
            "unit": "ms",
            "extra": "2*Stdev = 0.0136728 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 302.8700762,
            "range": "4.79232",
            "unit": "ms",
            "extra": "2*Stdev = 4.79232 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.00963595,
            "range": "0.702778",
            "unit": "ms",
            "extra": "2*Stdev = 0.702778 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.819758775,
            "range": "0.940603",
            "unit": "ms",
            "extra": "2*Stdev = 0.940603 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.32407,
            "range": "2.3331",
            "unit": "ms",
            "extra": "2*Stdev = 2.3331 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 38.7987744,
            "range": "3.29602",
            "unit": "ms",
            "extra": "2*Stdev = 3.29602 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 234.283517,
            "range": "17.8412",
            "unit": "ms",
            "extra": "2*Stdev = 17.8412 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 221.0681061,
            "range": "3.24829",
            "unit": "ms",
            "extra": "2*Stdev = 3.24829 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 208.1232124,
            "range": "14.0837",
            "unit": "ms",
            "extra": "2*Stdev = 14.0837 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 235.0600842,
            "range": "18.5405",
            "unit": "ms",
            "extra": "2*Stdev = 18.5405 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 225.9261574,
            "range": "5.07513",
            "unit": "ms",
            "extra": "2*Stdev = 5.07513 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1642.6636612,
            "range": "33.3484",
            "unit": "ms",
            "extra": "2*Stdev = 33.3484 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 400.0543806,
            "range": "9.11027",
            "unit": "ms",
            "extra": "2*Stdev = 9.11027 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 315.032226,
            "range": "24.0718",
            "unit": "ms",
            "extra": "2*Stdev = 24.0718 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.0528958,
            "range": "1.59192",
            "unit": "ms",
            "extra": "2*Stdev = 1.59192 ms"
          },
          {
            "name": "Line comment",
            "value": 411.379011,
            "range": "38.678",
            "unit": "ms",
            "extra": "2*Stdev = 38.678 ms"
          },
          {
            "name": "Block comment",
            "value": 365.108546,
            "range": "32.4841",
            "unit": "ms",
            "extra": "2*Stdev = 32.4841 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1216.6719034,
            "range": "28.6029",
            "unit": "ms",
            "extra": "2*Stdev = 28.6029 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1268.3183594,
            "range": "49.891",
            "unit": "ms",
            "extra": "2*Stdev = 49.891 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ae6fd58a0dcb90736dac2308ba27c9e9c803b2b8",
          "message": "Cap rendered normal forms with --max-output-size. (#2849)\n\nThe flag stops quoting and printing at a fixed size, defaulting to 128KiB when no number is given. Ordinary evaluation stays unbounded when the flag is omitted.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T10:32:16+02:00",
          "tree_id": "d9bf46985d88360e664e0be830c5496d0a459b46",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/ae6fd58a0dcb90736dac2308ba27c9e9c803b2b8"
        },
        "date": 1790671456700,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052450899,
            "range": "0.00428998",
            "unit": "ms",
            "extra": "2*Stdev = 0.00428998 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.052599073,
            "range": "0.0029268",
            "unit": "ms",
            "extra": "2*Stdev = 0.0029268 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033161101,
            "range": "0.00198636",
            "unit": "ms",
            "extra": "2*Stdev = 0.00198636 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 392.7615014,
            "range": "31.9537",
            "unit": "ms",
            "extra": "2*Stdev = 31.9537 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.018257513,
            "range": "0.00177498",
            "unit": "ms",
            "extra": "2*Stdev = 0.00177498 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 296.3740036,
            "range": "6.01531",
            "unit": "ms",
            "extra": "2*Stdev = 6.01531 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045916397,
            "range": "0.00419983",
            "unit": "ms",
            "extra": "2*Stdev = 0.00419983 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 83.1510902,
            "range": "3.90953",
            "unit": "ms",
            "extra": "2*Stdev = 3.90953 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020714809,
            "range": "0.00133166",
            "unit": "ms",
            "extra": "2*Stdev = 0.00133166 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 257.4097708,
            "range": "7.29291",
            "unit": "ms",
            "extra": "2*Stdev = 7.29291 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.018092149,
            "range": "0.000763876",
            "unit": "ms",
            "extra": "2*Stdev = 0.000763876 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 202.736972,
            "range": "4.32121",
            "unit": "ms",
            "extra": "2*Stdev = 4.32121 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053802248,
            "range": "0.0032596",
            "unit": "ms",
            "extra": "2*Stdev = 0.0032596 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 180.3306406,
            "range": "2.89875",
            "unit": "ms",
            "extra": "2*Stdev = 2.89875 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.049217414,
            "range": "0.00268615",
            "unit": "ms",
            "extra": "2*Stdev = 0.00268615 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 230.1674996,
            "range": "5.40511",
            "unit": "ms",
            "extra": "2*Stdev = 5.40511 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000500835,
            "range": "4.6926e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.6926e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010392602,
            "range": "0.000825134",
            "unit": "ms",
            "extra": "2*Stdev = 0.000825134 ms"
          },
          {
            "name": "large1.parse",
            "value": 169.064643,
            "range": "7.46139",
            "unit": "ms",
            "extra": "2*Stdev = 7.46139 ms"
          },
          {
            "name": "large1.resolve",
            "value": 56.8430713,
            "range": "2.93901",
            "unit": "ms",
            "extra": "2*Stdev = 2.93901 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 71.4691264,
            "range": "2.72683",
            "unit": "ms",
            "extra": "2*Stdev = 2.72683 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 139.2004436,
            "range": "4.38947",
            "unit": "ms",
            "extra": "2*Stdev = 4.38947 ms"
          },
          {
            "name": "large2.normalize",
            "value": 162.6367278,
            "range": "4.74998",
            "unit": "ms",
            "extra": "2*Stdev = 4.74998 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 335.4418004,
            "range": "17.8687",
            "unit": "ms",
            "extra": "2*Stdev = 17.8687 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 848.8871464,
            "range": "16.8181",
            "unit": "ms",
            "extra": "2*Stdev = 16.8181 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2309.7125514,
            "range": "110.019",
            "unit": "ms",
            "extra": "2*Stdev = 110.019 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 400.2079272,
            "range": "4.18622",
            "unit": "ms",
            "extra": "2*Stdev = 4.18622 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.657349018,
            "range": "0.0581555",
            "unit": "ms",
            "extra": "2*Stdev = 0.0581555 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1441.9704878,
            "range": "9.64124",
            "unit": "ms",
            "extra": "2*Stdev = 9.64124 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 92.8223605,
            "range": "2.75528",
            "unit": "ms",
            "extra": "2*Stdev = 2.75528 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.403647992,
            "range": "0.0250497",
            "unit": "ms",
            "extra": "2*Stdev = 0.0250497 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2403.7198228,
            "range": "145.185",
            "unit": "ms",
            "extra": "2*Stdev = 145.185 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4843.71664,
            "range": "79.6825",
            "unit": "ms",
            "extra": "2*Stdev = 79.6825 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 690.6145096,
            "range": "18.1813",
            "unit": "ms",
            "extra": "2*Stdev = 18.1813 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2431.8674964,
            "range": "125.817",
            "unit": "ms",
            "extra": "2*Stdev = 125.817 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 4959.0220784,
            "range": "14.4778",
            "unit": "ms",
            "extra": "2*Stdev = 14.4778 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.255070821,
            "range": "0.0174314",
            "unit": "ms",
            "extra": "2*Stdev = 0.0174314 ms"
          },
          {
            "name": "large4.resolve",
            "value": 305.3074768,
            "range": "6.20682",
            "unit": "ms",
            "extra": "2*Stdev = 6.20682 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 430.1332888,
            "range": "6.29622",
            "unit": "ms",
            "extra": "2*Stdev = 6.29622 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.882215575,
            "range": "0.0717418",
            "unit": "ms",
            "extra": "2*Stdev = 0.0717418 ms"
          },
          {
            "name": "large5.resolve",
            "value": 129.8491336,
            "range": "3.05067",
            "unit": "ms",
            "extra": "2*Stdev = 3.05067 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 67.8983386,
            "range": "2.78261",
            "unit": "ms",
            "extra": "2*Stdev = 2.78261 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.0522008,
            "range": "4.5748",
            "unit": "ms",
            "extra": "2*Stdev = 4.5748 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 813.2414296,
            "range": "15.1314",
            "unit": "ms",
            "extra": "2*Stdev = 15.1314 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000216271,
            "range": "1.5636e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.5636e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000202684,
            "range": "1.3274e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.3274e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 410.411317,
            "range": "16.387",
            "unit": "ms",
            "extra": "2*Stdev = 16.387 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.242770956,
            "range": "0.0740012",
            "unit": "ms",
            "extra": "2*Stdev = 0.0740012 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.233278331,
            "range": "0.0967083",
            "unit": "ms",
            "extra": "2*Stdev = 0.0967083 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.317952568,
            "range": "0.130244",
            "unit": "ms",
            "extra": "2*Stdev = 0.130244 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 537.945716,
            "range": "2.92357",
            "unit": "ms",
            "extra": "2*Stdev = 2.92357 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 468.178689,
            "range": "31.3752",
            "unit": "ms",
            "extra": "2*Stdev = 31.3752 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 370.316643,
            "range": "20.2546",
            "unit": "ms",
            "extra": "2*Stdev = 20.2546 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 320.2058094,
            "range": "8.72331",
            "unit": "ms",
            "extra": "2*Stdev = 8.72331 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 2821.4831256,
            "range": "177.959",
            "unit": "ms",
            "extra": "2*Stdev = 177.959 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 2874.409704,
            "range": "125.67",
            "unit": "ms",
            "extra": "2*Stdev = 125.67 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 176.679396,
            "range": "5.58338",
            "unit": "ms",
            "extra": "2*Stdev = 5.58338 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 70.9209168,
            "range": "1.4864",
            "unit": "ms",
            "extra": "2*Stdev = 1.4864 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 830.2714868,
            "range": "11.9816",
            "unit": "ms",
            "extra": "2*Stdev = 11.9816 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 434.5954036,
            "range": "3.55759",
            "unit": "ms",
            "extra": "2*Stdev = 3.55759 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 195.9453112,
            "range": "8.41567",
            "unit": "ms",
            "extra": "2*Stdev = 8.41567 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 22.473997,
            "range": "1.87627",
            "unit": "ms",
            "extra": "2*Stdev = 1.87627 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.794340643,
            "range": "0.101342",
            "unit": "ms",
            "extra": "2*Stdev = 0.101342 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 45.8750632,
            "range": "3.77612",
            "unit": "ms",
            "extra": "2*Stdev = 3.77612 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.327553275,
            "range": "0.110226",
            "unit": "ms",
            "extra": "2*Stdev = 0.110226 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.45146495,
            "range": "0.24279",
            "unit": "ms",
            "extra": "2*Stdev = 0.24279 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.145104412,
            "range": "0.0114828",
            "unit": "ms",
            "extra": "2*Stdev = 0.0114828 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 297.6101694,
            "range": "1.82445",
            "unit": "ms",
            "extra": "2*Stdev = 1.82445 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 13.905823475,
            "range": "0.531113",
            "unit": "ms",
            "extra": "2*Stdev = 0.531113 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.58155765,
            "range": "1.25343",
            "unit": "ms",
            "extra": "2*Stdev = 1.25343 ms"
          },
          {
            "name": "Long variable names",
            "value": 31.0649724,
            "range": "2.90212",
            "unit": "ms",
            "extra": "2*Stdev = 2.90212 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 39.6649597,
            "range": "2.2381",
            "unit": "ms",
            "extra": "2*Stdev = 2.2381 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 238.3327234,
            "range": "23.7767",
            "unit": "ms",
            "extra": "2*Stdev = 23.7767 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 221.9570748,
            "range": "2.90373",
            "unit": "ms",
            "extra": "2*Stdev = 2.90373 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 211.4164066,
            "range": "6.22401",
            "unit": "ms",
            "extra": "2*Stdev = 6.22401 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 226.1206452,
            "range": "5.08522",
            "unit": "ms",
            "extra": "2*Stdev = 5.08522 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 220.674457,
            "range": "3.88161",
            "unit": "ms",
            "extra": "2*Stdev = 3.88161 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1639.6061572,
            "range": "23.9886",
            "unit": "ms",
            "extra": "2*Stdev = 23.9886 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 400.0585724,
            "range": "3.10308",
            "unit": "ms",
            "extra": "2*Stdev = 3.10308 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 314.657603,
            "range": "3.38887",
            "unit": "ms",
            "extra": "2*Stdev = 3.38887 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.11371315,
            "range": "0.934925",
            "unit": "ms",
            "extra": "2*Stdev = 0.934925 ms"
          },
          {
            "name": "Line comment",
            "value": 424.6444553,
            "range": "22.3579",
            "unit": "ms",
            "extra": "2*Stdev = 22.3579 ms"
          },
          {
            "name": "Block comment",
            "value": 354.0346352,
            "range": "3.20371",
            "unit": "ms",
            "extra": "2*Stdev = 3.20371 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1224.178368,
            "range": "63.2528",
            "unit": "ms",
            "extra": "2*Stdev = 63.2528 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1256.1485932,
            "range": "13.1526",
            "unit": "ms",
            "extra": "2*Stdev = 13.1526 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3de48b67889aa9184a48c1da6fa92efc59998212",
          "message": "Render import errors without ANSI color (#2850)\n\n* Render import errors without ANSI colour.\n\nplainShowImportError strips the colour codes from the existing Show output so diagnostics can display the same text. The CLI keeps the coloured messages.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* unit tests\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T09:01:19Z",
          "tree_id": "12b1113c50028c4e35b0dc3cac01660b1367abac",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/3de48b67889aa9184a48c1da6fa92efc59998212"
        },
        "date": 1790673756904,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.05511267,
            "range": "0.00367929",
            "unit": "ms",
            "extra": "2*Stdev = 0.00367929 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.054753665,
            "range": "0.00280062",
            "unit": "ms",
            "extra": "2*Stdev = 0.00280062 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.033889926,
            "range": "0.00149395",
            "unit": "ms",
            "extra": "2*Stdev = 0.00149395 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 423.0867354,
            "range": "8.61107",
            "unit": "ms",
            "extra": "2*Stdev = 8.61107 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.018109358,
            "range": "0.00170453",
            "unit": "ms",
            "extra": "2*Stdev = 0.00170453 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 307.4429634,
            "range": "6.62244",
            "unit": "ms",
            "extra": "2*Stdev = 6.62244 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045918452,
            "range": "0.00267001",
            "unit": "ms",
            "extra": "2*Stdev = 0.00267001 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 85.1724822,
            "range": "5.42285",
            "unit": "ms",
            "extra": "2*Stdev = 5.42285 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.019768142,
            "range": "0.00188775",
            "unit": "ms",
            "extra": "2*Stdev = 0.00188775 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 263.26693,
            "range": "4.72275",
            "unit": "ms",
            "extra": "2*Stdev = 4.72275 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017828214,
            "range": "0.00144534",
            "unit": "ms",
            "extra": "2*Stdev = 0.00144534 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 204.8980578,
            "range": "7.73914",
            "unit": "ms",
            "extra": "2*Stdev = 7.73914 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.05333481,
            "range": "0.00530272",
            "unit": "ms",
            "extra": "2*Stdev = 0.00530272 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 187.0600676,
            "range": "5.12577",
            "unit": "ms",
            "extra": "2*Stdev = 5.12577 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.047848674,
            "range": "0.00300571",
            "unit": "ms",
            "extra": "2*Stdev = 0.00300571 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 235.4457042,
            "range": "5.36481",
            "unit": "ms",
            "extra": "2*Stdev = 5.36481 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000486608,
            "range": "4.6668e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.6668e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010797585,
            "range": "0.000577854",
            "unit": "ms",
            "extra": "2*Stdev = 0.000577854 ms"
          },
          {
            "name": "large1.parse",
            "value": 169.4905628,
            "range": "8.35292",
            "unit": "ms",
            "extra": "2*Stdev = 8.35292 ms"
          },
          {
            "name": "large1.resolve",
            "value": 58.6347592,
            "range": "3.28587",
            "unit": "ms",
            "extra": "2*Stdev = 3.28587 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 72.2699192,
            "range": "3.13977",
            "unit": "ms",
            "extra": "2*Stdev = 3.13977 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.03899,
            "range": "4.68825",
            "unit": "ms",
            "extra": "2*Stdev = 4.68825 ms"
          },
          {
            "name": "large2.normalize",
            "value": 167.3995654,
            "range": "6.37293",
            "unit": "ms",
            "extra": "2*Stdev = 6.37293 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 342.6810386,
            "range": "8.08957",
            "unit": "ms",
            "extra": "2*Stdev = 8.08957 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 836.0004422,
            "range": "7.82744",
            "unit": "ms",
            "extra": "2*Stdev = 7.82744 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2349.18390545,
            "range": "165.965",
            "unit": "ms",
            "extra": "2*Stdev = 165.965 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 404.3795756,
            "range": "11.6479",
            "unit": "ms",
            "extra": "2*Stdev = 11.6479 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.660204065,
            "range": "0.0519575",
            "unit": "ms",
            "extra": "2*Stdev = 0.0519575 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1453.1708072,
            "range": "8.05759",
            "unit": "ms",
            "extra": "2*Stdev = 8.05759 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 94.6291335,
            "range": "7.1602",
            "unit": "ms",
            "extra": "2*Stdev = 7.1602 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.414465006,
            "range": "0.0245869",
            "unit": "ms",
            "extra": "2*Stdev = 0.0245869 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2461.3592314,
            "range": "122.223",
            "unit": "ms",
            "extra": "2*Stdev = 122.223 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5447.6859813,
            "range": "202.862",
            "unit": "ms",
            "extra": "2*Stdev = 202.862 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 734.4785074,
            "range": "8.11329",
            "unit": "ms",
            "extra": "2*Stdev = 8.11329 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2588.37922965,
            "range": "106.817",
            "unit": "ms",
            "extra": "2*Stdev = 106.817 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5288.4365376,
            "range": "108.78",
            "unit": "ms",
            "extra": "2*Stdev = 108.78 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.258305829,
            "range": "0.0122",
            "unit": "ms",
            "extra": "2*Stdev = 0.0122 ms"
          },
          {
            "name": "large4.resolve",
            "value": 308.0643922,
            "range": "3.37882",
            "unit": "ms",
            "extra": "2*Stdev = 3.37882 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 428.495632,
            "range": "6.66108",
            "unit": "ms",
            "extra": "2*Stdev = 6.66108 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.913710981,
            "range": "0.102198",
            "unit": "ms",
            "extra": "2*Stdev = 0.102198 ms"
          },
          {
            "name": "large5.resolve",
            "value": 131.432063,
            "range": "5.44072",
            "unit": "ms",
            "extra": "2*Stdev = 5.44072 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.1353264,
            "range": "6.16786",
            "unit": "ms",
            "extra": "2*Stdev = 6.16786 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.8284138,
            "range": "4.779",
            "unit": "ms",
            "extra": "2*Stdev = 4.779 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 837.1832648,
            "range": "4.94067",
            "unit": "ms",
            "extra": "2*Stdev = 4.94067 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.00020921,
            "range": "1.5266e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.5266e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000237734,
            "range": "4.136e-06",
            "unit": "ms",
            "extra": "2*Stdev = 4.136e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 413.9993202,
            "range": "3.04287",
            "unit": "ms",
            "extra": "2*Stdev = 3.04287 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.2462661,
            "range": "0.0857172",
            "unit": "ms",
            "extra": "2*Stdev = 0.0857172 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.340562518,
            "range": "0.189889",
            "unit": "ms",
            "extra": "2*Stdev = 0.189889 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.338292587,
            "range": "0.0965399",
            "unit": "ms",
            "extra": "2*Stdev = 0.0965399 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 551.7172658,
            "range": "19.1417",
            "unit": "ms",
            "extra": "2*Stdev = 19.1417 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 525.0916344,
            "range": "52.2185",
            "unit": "ms",
            "extra": "2*Stdev = 52.2185 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 403.5764422,
            "range": "14.4465",
            "unit": "ms",
            "extra": "2*Stdev = 14.4465 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 323.6890642,
            "range": "10.6995",
            "unit": "ms",
            "extra": "2*Stdev = 10.6995 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 3177.6819,
            "range": "47.4571",
            "unit": "ms",
            "extra": "2*Stdev = 47.4571 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3177.600577,
            "range": "42.0865",
            "unit": "ms",
            "extra": "2*Stdev = 42.0865 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 178.789651,
            "range": "6.24997",
            "unit": "ms",
            "extra": "2*Stdev = 6.24997 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 71.2325855,
            "range": "1.99014",
            "unit": "ms",
            "extra": "2*Stdev = 1.99014 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 842.570171,
            "range": "3.63357",
            "unit": "ms",
            "extra": "2*Stdev = 3.63357 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 439.571227,
            "range": "4.68514",
            "unit": "ms",
            "extra": "2*Stdev = 4.68514 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 197.687931,
            "range": "2.87465",
            "unit": "ms",
            "extra": "2*Stdev = 2.87465 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 19.5009635,
            "range": "1.80655",
            "unit": "ms",
            "extra": "2*Stdev = 1.80655 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.837577537,
            "range": "0.177688",
            "unit": "ms",
            "extra": "2*Stdev = 0.177688 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 46.5975992,
            "range": "1.00893",
            "unit": "ms",
            "extra": "2*Stdev = 1.00893 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.3274352,
            "range": "0.104092",
            "unit": "ms",
            "extra": "2*Stdev = 0.104092 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.524427425,
            "range": "0.639324",
            "unit": "ms",
            "extra": "2*Stdev = 0.639324 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.143079427,
            "range": "0.00295829",
            "unit": "ms",
            "extra": "2*Stdev = 0.00295829 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 298.4681464,
            "range": "6.25071",
            "unit": "ms",
            "extra": "2*Stdev = 6.25071 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 13.895849675,
            "range": "0.516427",
            "unit": "ms",
            "extra": "2*Stdev = 0.516427 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.782721,
            "range": "0.828296",
            "unit": "ms",
            "extra": "2*Stdev = 0.828296 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.6071984,
            "range": "2.51184",
            "unit": "ms",
            "extra": "2*Stdev = 2.51184 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 40.1282682,
            "range": "3.93618",
            "unit": "ms",
            "extra": "2*Stdev = 3.93618 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 233.8147626,
            "range": "7.7833",
            "unit": "ms",
            "extra": "2*Stdev = 7.7833 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 219.7267266,
            "range": "4.12109",
            "unit": "ms",
            "extra": "2*Stdev = 4.12109 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 204.8251874,
            "range": "4.57875",
            "unit": "ms",
            "extra": "2*Stdev = 4.57875 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 226.9970464,
            "range": "5.39277",
            "unit": "ms",
            "extra": "2*Stdev = 5.39277 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 217.6896222,
            "range": "19.3592",
            "unit": "ms",
            "extra": "2*Stdev = 19.3592 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1635.1221132,
            "range": "42.1638",
            "unit": "ms",
            "extra": "2*Stdev = 42.1638 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 400.6947632,
            "range": "5.39532",
            "unit": "ms",
            "extra": "2*Stdev = 5.39532 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 315.7800996,
            "range": "23.3328",
            "unit": "ms",
            "extra": "2*Stdev = 23.3328 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.2244475,
            "range": "1.6361",
            "unit": "ms",
            "extra": "2*Stdev = 1.6361 ms"
          },
          {
            "name": "Line comment",
            "value": 404.7805464,
            "range": "26.7474",
            "unit": "ms",
            "extra": "2*Stdev = 26.7474 ms"
          },
          {
            "name": "Block comment",
            "value": 373.2426206,
            "range": "15.1977",
            "unit": "ms",
            "extra": "2*Stdev = 15.1977 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1215.5503304,
            "range": "29.8666",
            "unit": "ms",
            "extra": "2*Stdev = 29.8666 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1265.612428,
            "range": "54.5159",
            "unit": "ms",
            "extra": "2*Stdev = 54.5159 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e215400b4024c81a8091521015abdf7e08091a98",
          "message": "Fixes for bounded execution (#2854)\n\n* Require a size for --max-output-size and end truncated output with ….\n\nThe flag no longer accepts a bare form with a hidden 128KiB default, and a cut normal form no longer prints the next token fragment after the ellipsis.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Charge --max-output-size as rendered text and split the work limits.\n\nQuoting now spends the output budget on the bytes each syntax node prints, and allocation and time caps are independent flags that combine with it.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* adding new CLI flags\n\n* longer test\n\n* make benchmarks -threaded\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T11:29:54Z",
          "tree_id": "5a6447bb6a3587675aa31f98ec65ec1287bacbf3",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/e215400b4024c81a8091521015abdf7e08091a98"
        },
        "date": 1790682566585,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.053665658,
            "range": "0.00345684",
            "unit": "ms",
            "extra": "2*Stdev = 0.00345684 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053325325,
            "range": "0.0028814",
            "unit": "ms",
            "extra": "2*Stdev = 0.0028814 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.03305281,
            "range": "0.00133812",
            "unit": "ms",
            "extra": "2*Stdev = 0.00133812 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 404.9526746,
            "range": "28.4578",
            "unit": "ms",
            "extra": "2*Stdev = 28.4578 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017949382,
            "range": "0.00165765",
            "unit": "ms",
            "extra": "2*Stdev = 0.00165765 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 317.252433,
            "range": "12.5299",
            "unit": "ms",
            "extra": "2*Stdev = 12.5299 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.046323722,
            "range": "0.00336345",
            "unit": "ms",
            "extra": "2*Stdev = 0.00336345 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 87.235484,
            "range": "3.366",
            "unit": "ms",
            "extra": "2*Stdev = 3.366 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020046633,
            "range": "0.00131264",
            "unit": "ms",
            "extra": "2*Stdev = 0.00131264 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 267.3819018,
            "range": "9.94882",
            "unit": "ms",
            "extra": "2*Stdev = 9.94882 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017661354,
            "range": "0.00154376",
            "unit": "ms",
            "extra": "2*Stdev = 0.00154376 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 210.3361,
            "range": "4.27121",
            "unit": "ms",
            "extra": "2*Stdev = 4.27121 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.054772544,
            "range": "0.00418078",
            "unit": "ms",
            "extra": "2*Stdev = 0.00418078 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 190.892997,
            "range": "8.23926",
            "unit": "ms",
            "extra": "2*Stdev = 8.23926 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.049172397,
            "range": "0.00408436",
            "unit": "ms",
            "extra": "2*Stdev = 0.00408436 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 244.119137,
            "range": "5.37553",
            "unit": "ms",
            "extra": "2*Stdev = 5.37553 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000476181,
            "range": "3.4734e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.4734e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011054033,
            "range": "0.000984502",
            "unit": "ms",
            "extra": "2*Stdev = 0.000984502 ms"
          },
          {
            "name": "large1.parse",
            "value": 172.9052022,
            "range": "2.79446",
            "unit": "ms",
            "extra": "2*Stdev = 2.79446 ms"
          },
          {
            "name": "large1.resolve",
            "value": 59.958214,
            "range": "2.5786",
            "unit": "ms",
            "extra": "2*Stdev = 2.5786 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 75.3534242,
            "range": "4.34123",
            "unit": "ms",
            "extra": "2*Stdev = 4.34123 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 144.5896974,
            "range": "7.14697",
            "unit": "ms",
            "extra": "2*Stdev = 7.14697 ms"
          },
          {
            "name": "large2.normalize",
            "value": 168.5578526,
            "range": "5.41428",
            "unit": "ms",
            "extra": "2*Stdev = 5.41428 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 339.9575988,
            "range": "8.40258",
            "unit": "ms",
            "extra": "2*Stdev = 8.40258 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 837.4068874,
            "range": "9.81706",
            "unit": "ms",
            "extra": "2*Stdev = 9.81706 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2460.0635756,
            "range": "110.76",
            "unit": "ms",
            "extra": "2*Stdev = 110.76 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 410.7485418,
            "range": "9.27999",
            "unit": "ms",
            "extra": "2*Stdev = 9.27999 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.675608175,
            "range": "0.0220158",
            "unit": "ms",
            "extra": "2*Stdev = 0.0220158 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1560.6969126,
            "range": "72.3173",
            "unit": "ms",
            "extra": "2*Stdev = 72.3173 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.0138556,
            "range": "7.06126",
            "unit": "ms",
            "extra": "2*Stdev = 7.06126 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.408314231,
            "range": "0.0151205",
            "unit": "ms",
            "extra": "2*Stdev = 0.0151205 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2458.6161104,
            "range": "39.3497",
            "unit": "ms",
            "extra": "2*Stdev = 39.3497 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5034.3684148,
            "range": "73.7614",
            "unit": "ms",
            "extra": "2*Stdev = 73.7614 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 709.5618768,
            "range": "15.0588",
            "unit": "ms",
            "extra": "2*Stdev = 15.0588 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2500.94766075,
            "range": "158.292",
            "unit": "ms",
            "extra": "2*Stdev = 158.292 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5371.8632456,
            "range": "107.686",
            "unit": "ms",
            "extra": "2*Stdev = 107.686 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.263984681,
            "range": "0.0214145",
            "unit": "ms",
            "extra": "2*Stdev = 0.0214145 ms"
          },
          {
            "name": "large4.resolve",
            "value": 313.7521106,
            "range": "9.32424",
            "unit": "ms",
            "extra": "2*Stdev = 9.32424 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 440.283966,
            "range": "7.63153",
            "unit": "ms",
            "extra": "2*Stdev = 7.63153 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.920746815,
            "range": "0.0833538",
            "unit": "ms",
            "extra": "2*Stdev = 0.0833538 ms"
          },
          {
            "name": "large5.resolve",
            "value": 132.9750134,
            "range": "7.4526",
            "unit": "ms",
            "extra": "2*Stdev = 7.4526 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 69.3703176,
            "range": "5.77145",
            "unit": "ms",
            "extra": "2*Stdev = 5.77145 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.3554884,
            "range": "6.02717",
            "unit": "ms",
            "extra": "2*Stdev = 6.02717 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 855.6937098,
            "range": "5.57342",
            "unit": "ms",
            "extra": "2*Stdev = 5.57342 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000211206,
            "range": "2.0732e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.0732e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000215391,
            "range": "1.2684e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.2684e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 406.0215646,
            "range": "8.57363",
            "unit": "ms",
            "extra": "2*Stdev = 8.57363 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.21347705,
            "range": "0.0939678",
            "unit": "ms",
            "extra": "2*Stdev = 0.0939678 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.313894287,
            "range": "0.179216",
            "unit": "ms",
            "extra": "2*Stdev = 0.179216 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.400288756,
            "range": "0.0938024",
            "unit": "ms",
            "extra": "2*Stdev = 0.0938024 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 540.7523348,
            "range": "9.24062",
            "unit": "ms",
            "extra": "2*Stdev = 9.24062 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 510.7007768,
            "range": "6.11224",
            "unit": "ms",
            "extra": "2*Stdev = 6.11224 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 397.8048362,
            "range": "13.5237",
            "unit": "ms",
            "extra": "2*Stdev = 13.5237 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 328.525097,
            "range": "19.6821",
            "unit": "ms",
            "extra": "2*Stdev = 19.6821 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 3005.599263,
            "range": "33.8111",
            "unit": "ms",
            "extra": "2*Stdev = 33.8111 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3104.8640134,
            "range": "97.1541",
            "unit": "ms",
            "extra": "2*Stdev = 97.1541 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 175.5144948,
            "range": "9.61861",
            "unit": "ms",
            "extra": "2*Stdev = 9.61861 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 75.3472847,
            "range": "3.15829",
            "unit": "ms",
            "extra": "2*Stdev = 3.15829 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 849.800921,
            "range": "2.89169",
            "unit": "ms",
            "extra": "2*Stdev = 2.89169 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 442.228843,
            "range": "7.24207",
            "unit": "ms",
            "extra": "2*Stdev = 7.24207 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 201.1897044,
            "range": "4.0826",
            "unit": "ms",
            "extra": "2*Stdev = 4.0826 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 21.772917,
            "range": "1.71906",
            "unit": "ms",
            "extra": "2*Stdev = 1.71906 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.785623337,
            "range": "0.126053",
            "unit": "ms",
            "extra": "2*Stdev = 0.126053 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 45.0061712,
            "range": "4.03393",
            "unit": "ms",
            "extra": "2*Stdev = 4.03393 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.37586529,
            "range": "0.136681",
            "unit": "ms",
            "extra": "2*Stdev = 0.136681 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.599496175,
            "range": "0.277215",
            "unit": "ms",
            "extra": "2*Stdev = 0.277215 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.143068919,
            "range": "0.0102224",
            "unit": "ms",
            "extra": "2*Stdev = 0.0102224 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 292.1363673,
            "range": "20.9924",
            "unit": "ms",
            "extra": "2*Stdev = 20.9924 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.81609015,
            "range": "0.942828",
            "unit": "ms",
            "extra": "2*Stdev = 0.942828 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 14.011980925,
            "range": "0.836152",
            "unit": "ms",
            "extra": "2*Stdev = 0.836152 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.591342525,
            "range": "1.98659",
            "unit": "ms",
            "extra": "2*Stdev = 1.98659 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 40.4949809,
            "range": "2.5631",
            "unit": "ms",
            "extra": "2*Stdev = 2.5631 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 227.9867808,
            "range": "9.79582",
            "unit": "ms",
            "extra": "2*Stdev = 9.79582 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 204.0413692,
            "range": "15.6519",
            "unit": "ms",
            "extra": "2*Stdev = 15.6519 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 201.9821596,
            "range": "7.78125",
            "unit": "ms",
            "extra": "2*Stdev = 7.78125 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 212.6577395,
            "range": "13.5837",
            "unit": "ms",
            "extra": "2*Stdev = 13.5837 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 205.998852,
            "range": "7.63992",
            "unit": "ms",
            "extra": "2*Stdev = 7.63992 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1640.6016178,
            "range": "28.1397",
            "unit": "ms",
            "extra": "2*Stdev = 28.1397 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 385.0018916,
            "range": "27.3602",
            "unit": "ms",
            "extra": "2*Stdev = 27.3602 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 295.2010238,
            "range": "18.4837",
            "unit": "ms",
            "extra": "2*Stdev = 18.4837 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.557603112,
            "range": "0.953719",
            "unit": "ms",
            "extra": "2*Stdev = 0.953719 ms"
          },
          {
            "name": "Line comment",
            "value": 382.245213,
            "range": "34.5339",
            "unit": "ms",
            "extra": "2*Stdev = 34.5339 ms"
          },
          {
            "name": "Block comment",
            "value": 350.1979958,
            "range": "24.8087",
            "unit": "ms",
            "extra": "2*Stdev = 24.8087 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1206.6076398,
            "range": "7.10569",
            "unit": "ms",
            "extra": "2*Stdev = 7.10569 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1258.26229,
            "range": "32.7771",
            "unit": "ms",
            "extra": "2*Stdev = 32.7771 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "10b14f12b556e83f071e53170550002eda0ac210",
          "message": "Share name resolution in Dhall.Scope. (#2852)\n\ndhall-docs jump-to-definition now uses the same de Bruijn-aware resolver the language server will use for lets, lambdas, foralls, and record fields.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T14:47:44+02:00",
          "tree_id": "e67cb18396235105485d7bf3adf7bd03512d61d1",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/10b14f12b556e83f071e53170550002eda0ac210"
        },
        "date": 1790686679129,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.042399379,
            "range": "0.00400935",
            "unit": "ms",
            "extra": "2*Stdev = 0.00400935 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.042245354,
            "range": "0.00262746",
            "unit": "ms",
            "extra": "2*Stdev = 0.00262746 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.024155608,
            "range": "0.00135717",
            "unit": "ms",
            "extra": "2*Stdev = 0.00135717 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 295.0645804,
            "range": "15.1888",
            "unit": "ms",
            "extra": "2*Stdev = 15.1888 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.011802931,
            "range": "0.000683622",
            "unit": "ms",
            "extra": "2*Stdev = 0.000683622 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 271.2104986,
            "range": "6.37391",
            "unit": "ms",
            "extra": "2*Stdev = 6.37391 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.042817306,
            "range": "0.00316",
            "unit": "ms",
            "extra": "2*Stdev = 0.00316 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 82.3972686,
            "range": "4.11193",
            "unit": "ms",
            "extra": "2*Stdev = 4.11193 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.015606043,
            "range": "0.000746876",
            "unit": "ms",
            "extra": "2*Stdev = 0.000746876 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 243.5570702,
            "range": "5.32511",
            "unit": "ms",
            "extra": "2*Stdev = 5.32511 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.013380126,
            "range": "0.000698508",
            "unit": "ms",
            "extra": "2*Stdev = 0.000698508 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 198.950229,
            "range": "3.90595",
            "unit": "ms",
            "extra": "2*Stdev = 3.90595 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.045350075,
            "range": "0.00309926",
            "unit": "ms",
            "extra": "2*Stdev = 0.00309926 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 170.3950552,
            "range": "4.30767",
            "unit": "ms",
            "extra": "2*Stdev = 4.30767 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.039771741,
            "range": "0.0033158",
            "unit": "ms",
            "extra": "2*Stdev = 0.0033158 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 208.0297642,
            "range": "3.97752",
            "unit": "ms",
            "extra": "2*Stdev = 3.97752 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000375968,
            "range": "2.2454e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.2454e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.008738823,
            "range": "0.000711564",
            "unit": "ms",
            "extra": "2*Stdev = 0.000711564 ms"
          },
          {
            "name": "large1.parse",
            "value": 140.0804378,
            "range": "3.7527",
            "unit": "ms",
            "extra": "2*Stdev = 3.7527 ms"
          },
          {
            "name": "large1.resolve",
            "value": 50.9367296,
            "range": "2.06114",
            "unit": "ms",
            "extra": "2*Stdev = 2.06114 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 62.754071,
            "range": "3.32278",
            "unit": "ms",
            "extra": "2*Stdev = 3.32278 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 127.7037478,
            "range": "4.28278",
            "unit": "ms",
            "extra": "2*Stdev = 4.28278 ms"
          },
          {
            "name": "large2.normalize",
            "value": 133.3376902,
            "range": "2.82535",
            "unit": "ms",
            "extra": "2*Stdev = 2.82535 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 285.6893466,
            "range": "5.94251",
            "unit": "ms",
            "extra": "2*Stdev = 5.94251 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 724.5652168,
            "range": "7.12031",
            "unit": "ms",
            "extra": "2*Stdev = 7.12031 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2179.35377905,
            "range": "131.857",
            "unit": "ms",
            "extra": "2*Stdev = 131.857 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 325.0212376,
            "range": "6.36396",
            "unit": "ms",
            "extra": "2*Stdev = 6.36396 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.60273289,
            "range": "0.0222192",
            "unit": "ms",
            "extra": "2*Stdev = 0.0222192 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1388.1688212,
            "range": "44.9017",
            "unit": "ms",
            "extra": "2*Stdev = 44.9017 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 74.5032304,
            "range": "4.91328",
            "unit": "ms",
            "extra": "2*Stdev = 4.91328 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.333185432,
            "range": "0.0245671",
            "unit": "ms",
            "extra": "2*Stdev = 0.0245671 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2132.0725449,
            "range": "37.3948",
            "unit": "ms",
            "extra": "2*Stdev = 37.3948 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4370.5710822,
            "range": "26.1719",
            "unit": "ms",
            "extra": "2*Stdev = 26.1719 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 628.3167894,
            "range": "12.7258",
            "unit": "ms",
            "extra": "2*Stdev = 12.7258 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2206.5152594,
            "range": "104.751",
            "unit": "ms",
            "extra": "2*Stdev = 104.751 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 4542.3791672,
            "range": "42.9098",
            "unit": "ms",
            "extra": "2*Stdev = 42.9098 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.190216242,
            "range": "0.0125104",
            "unit": "ms",
            "extra": "2*Stdev = 0.0125104 ms"
          },
          {
            "name": "large4.resolve",
            "value": 247.0272264,
            "range": "8.00129",
            "unit": "ms",
            "extra": "2*Stdev = 8.00129 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 378.3753612,
            "range": "3.67765",
            "unit": "ms",
            "extra": "2*Stdev = 3.67765 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.68960159,
            "range": "0.0763614",
            "unit": "ms",
            "extra": "2*Stdev = 0.0763614 ms"
          },
          {
            "name": "large5.resolve",
            "value": 100.9938958,
            "range": "4.75545",
            "unit": "ms",
            "extra": "2*Stdev = 4.75545 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 55.2099299,
            "range": "1.76217",
            "unit": "ms",
            "extra": "2*Stdev = 1.76217 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 65.4190668,
            "range": "3.9006",
            "unit": "ms",
            "extra": "2*Stdev = 3.9006 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 661.469709,
            "range": "5.33756",
            "unit": "ms",
            "extra": "2*Stdev = 5.33756 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000171567,
            "range": "1.3856e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.3856e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000187915,
            "range": "1.2152e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.2152e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 289.8045118,
            "range": "6.28032",
            "unit": "ms",
            "extra": "2*Stdev = 6.28032 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.875429031,
            "range": "0.0594828",
            "unit": "ms",
            "extra": "2*Stdev = 0.0594828 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.485758662,
            "range": "0.104655",
            "unit": "ms",
            "extra": "2*Stdev = 0.104655 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.040527918,
            "range": "0.0569421",
            "unit": "ms",
            "extra": "2*Stdev = 0.0569421 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 462.2677946,
            "range": "9.48201",
            "unit": "ms",
            "extra": "2*Stdev = 9.48201 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 422.0764862,
            "range": "4.58348",
            "unit": "ms",
            "extra": "2*Stdev = 4.58348 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 343.5257502,
            "range": "5.36722",
            "unit": "ms",
            "extra": "2*Stdev = 5.36722 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 277.6548688,
            "range": "14.2202",
            "unit": "ms",
            "extra": "2*Stdev = 14.2202 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 2516.679301,
            "range": "8.43408",
            "unit": "ms",
            "extra": "2*Stdev = 8.43408 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 2502.0755002,
            "range": "11.4505",
            "unit": "ms",
            "extra": "2*Stdev = 11.4505 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 129.4054816,
            "range": "9.35134",
            "unit": "ms",
            "extra": "2*Stdev = 9.35134 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 54.4280922,
            "range": "1.36497",
            "unit": "ms",
            "extra": "2*Stdev = 1.36497 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 644.1966454,
            "range": "15.2795",
            "unit": "ms",
            "extra": "2*Stdev = 15.2795 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 373.4263924,
            "range": "2.76559",
            "unit": "ms",
            "extra": "2*Stdev = 2.76559 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 177.1068534,
            "range": "6.18111",
            "unit": "ms",
            "extra": "2*Stdev = 6.18111 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.74455105,
            "range": "0.698757",
            "unit": "ms",
            "extra": "2*Stdev = 0.698757 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.511298087,
            "range": "0.103897",
            "unit": "ms",
            "extra": "2*Stdev = 0.103897 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 40.3083074,
            "range": "1.50148",
            "unit": "ms",
            "extra": "2*Stdev = 1.50148 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.132902275,
            "range": "0.112781",
            "unit": "ms",
            "extra": "2*Stdev = 0.112781 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.612063768,
            "range": "0.102114",
            "unit": "ms",
            "extra": "2*Stdev = 0.102114 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.116808536,
            "range": "0.00821486",
            "unit": "ms",
            "extra": "2*Stdev = 0.00821486 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 264.7346387,
            "range": "25.1159",
            "unit": "ms",
            "extra": "2*Stdev = 25.1159 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 13.716582725,
            "range": "0.436338",
            "unit": "ms",
            "extra": "2*Stdev = 0.436338 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 12.303075325,
            "range": "0.759702",
            "unit": "ms",
            "extra": "2*Stdev = 0.759702 ms"
          },
          {
            "name": "Long variable names",
            "value": 22.8403913,
            "range": "1.60031",
            "unit": "ms",
            "extra": "2*Stdev = 1.60031 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 32.581199,
            "range": "1.57203",
            "unit": "ms",
            "extra": "2*Stdev = 1.57203 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 156.1448791,
            "range": "2.0956",
            "unit": "ms",
            "extra": "2*Stdev = 2.0956 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 148.4934966,
            "range": "5.77186",
            "unit": "ms",
            "extra": "2*Stdev = 5.77186 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 150.6140808,
            "range": "7.71236",
            "unit": "ms",
            "extra": "2*Stdev = 7.71236 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 151.8543494,
            "range": "13.6076",
            "unit": "ms",
            "extra": "2*Stdev = 13.6076 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 150.556824,
            "range": "9.35764",
            "unit": "ms",
            "extra": "2*Stdev = 9.35764 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1398.9978922,
            "range": "5.25253",
            "unit": "ms",
            "extra": "2*Stdev = 5.25253 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 281.02308,
            "range": "3.3951",
            "unit": "ms",
            "extra": "2*Stdev = 3.3951 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 203.45153,
            "range": "8.49888",
            "unit": "ms",
            "extra": "2*Stdev = 8.49888 ms"
          },
          {
            "name": "Whitespace",
            "value": 14.6581161,
            "range": "0.697954",
            "unit": "ms",
            "extra": "2*Stdev = 0.697954 ms"
          },
          {
            "name": "Line comment",
            "value": 258.4124338,
            "range": "3.84206",
            "unit": "ms",
            "extra": "2*Stdev = 3.84206 ms"
          },
          {
            "name": "Block comment",
            "value": 231.3376376,
            "range": "3.43526",
            "unit": "ms",
            "extra": "2*Stdev = 3.43526 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 950.477264,
            "range": "12.9647",
            "unit": "ms",
            "extra": "2*Stdev = 12.9647 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 991.6660406,
            "range": "34.5079",
            "unit": "ms",
            "extra": "2*Stdev = 34.5079 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "45b0ab6e019a672b5b92193413ca86efe74cfdd0",
          "message": "Add a resumable `TypingContext` for typechecking bindings once. (#2851)\n\n* Add a resumable TypingContext for typechecking bindings once.\n\nLater expressions can be checked against values and types already inferred, without wrapping those bindings in lets again. typeWith is unchanged.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* use newtype\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T15:45:03+02:00",
          "tree_id": "8185c77d4b48bb47c3a03557f6e29eb8b30a5012",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/45b0ab6e019a672b5b92193413ca86efe74cfdd0"
        },
        "date": 1790690053896,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.037773762,
            "range": "0.00339063",
            "unit": "ms",
            "extra": "2*Stdev = 0.00339063 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.037769077,
            "range": "0.00325121",
            "unit": "ms",
            "extra": "2*Stdev = 0.00325121 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.02160574,
            "range": "0.00143036",
            "unit": "ms",
            "extra": "2*Stdev = 0.00143036 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 292.5589428,
            "range": "22.8522",
            "unit": "ms",
            "extra": "2*Stdev = 22.8522 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.010327181,
            "range": "0.000678674",
            "unit": "ms",
            "extra": "2*Stdev = 0.000678674 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 248.7988018,
            "range": "10.6113",
            "unit": "ms",
            "extra": "2*Stdev = 10.6113 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.021383602,
            "range": "0.00158982",
            "unit": "ms",
            "extra": "2*Stdev = 0.00158982 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 68.8174558,
            "range": "2.92734",
            "unit": "ms",
            "extra": "2*Stdev = 2.92734 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.009263722,
            "range": "0.000349378",
            "unit": "ms",
            "extra": "2*Stdev = 0.000349378 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 238.233972,
            "range": "3.30876",
            "unit": "ms",
            "extra": "2*Stdev = 3.30876 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.00808959,
            "range": "0.000553304",
            "unit": "ms",
            "extra": "2*Stdev = 0.000553304 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 186.1376802,
            "range": "8.76724",
            "unit": "ms",
            "extra": "2*Stdev = 8.76724 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.025587037,
            "range": "0.00196502",
            "unit": "ms",
            "extra": "2*Stdev = 0.00196502 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 167.1320426,
            "range": "7.24996",
            "unit": "ms",
            "extra": "2*Stdev = 7.24996 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.022026202,
            "range": "0.00209853",
            "unit": "ms",
            "extra": "2*Stdev = 0.00209853 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 213.7133494,
            "range": "7.05278",
            "unit": "ms",
            "extra": "2*Stdev = 7.05278 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000324817,
            "range": "2.6994e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.6994e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.007709306,
            "range": "0.000666772",
            "unit": "ms",
            "extra": "2*Stdev = 0.000666772 ms"
          },
          {
            "name": "large1.parse",
            "value": 109.948157,
            "range": "4.49703",
            "unit": "ms",
            "extra": "2*Stdev = 4.49703 ms"
          },
          {
            "name": "large1.resolve",
            "value": 47.2191456,
            "range": "4.0984",
            "unit": "ms",
            "extra": "2*Stdev = 4.0984 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 52.495936,
            "range": "3.46573",
            "unit": "ms",
            "extra": "2*Stdev = 3.46573 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 96.7654032,
            "range": "2.92704",
            "unit": "ms",
            "extra": "2*Stdev = 2.92704 ms"
          },
          {
            "name": "large2.normalize",
            "value": 121.30293,
            "range": "3.11005",
            "unit": "ms",
            "extra": "2*Stdev = 3.11005 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 257.6246238,
            "range": "8.34122",
            "unit": "ms",
            "extra": "2*Stdev = 8.34122 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 640.1803852,
            "range": "4.77259",
            "unit": "ms",
            "extra": "2*Stdev = 4.77259 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 1932.38419375,
            "range": "84.4476",
            "unit": "ms",
            "extra": "2*Stdev = 84.4476 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 321.2377622,
            "range": "7.49638",
            "unit": "ms",
            "extra": "2*Stdev = 7.49638 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.473983903,
            "range": "0.0313146",
            "unit": "ms",
            "extra": "2*Stdev = 0.0313146 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1219.0031,
            "range": "64.9279",
            "unit": "ms",
            "extra": "2*Stdev = 64.9279 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 74.903157275,
            "range": "1.45008",
            "unit": "ms",
            "extra": "2*Stdev = 1.45008 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.280594631,
            "range": "0.0209882",
            "unit": "ms",
            "extra": "2*Stdev = 0.0209882 ms"
          },
          {
            "name": "large3.resolve",
            "value": 1965.1404053,
            "range": "45.839",
            "unit": "ms",
            "extra": "2*Stdev = 45.839 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4060.6044486,
            "range": "94.7451",
            "unit": "ms",
            "extra": "2*Stdev = 94.7451 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 565.166809,
            "range": "25.8062",
            "unit": "ms",
            "extra": "2*Stdev = 25.8062 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2011.89908765,
            "range": "120.532",
            "unit": "ms",
            "extra": "2*Stdev = 120.532 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 4406.225184,
            "range": "281.779",
            "unit": "ms",
            "extra": "2*Stdev = 281.779 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.177319912,
            "range": "0.0125515",
            "unit": "ms",
            "extra": "2*Stdev = 0.0125515 ms"
          },
          {
            "name": "large4.resolve",
            "value": 208.8298674,
            "range": "6.82456",
            "unit": "ms",
            "extra": "2*Stdev = 6.82456 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 318.532842,
            "range": "4.6761",
            "unit": "ms",
            "extra": "2*Stdev = 4.6761 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.317259843,
            "range": "0.0955664",
            "unit": "ms",
            "extra": "2*Stdev = 0.0955664 ms"
          },
          {
            "name": "large5.resolve",
            "value": 91.6766962,
            "range": "3.46879",
            "unit": "ms",
            "extra": "2*Stdev = 3.46879 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 49.9343938,
            "range": "1.44156",
            "unit": "ms",
            "extra": "2*Stdev = 1.44156 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 56.3040578,
            "range": "5.20852",
            "unit": "ms",
            "extra": "2*Stdev = 5.20852 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 631.1666842,
            "range": "32.4081",
            "unit": "ms",
            "extra": "2*Stdev = 32.4081 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000147415,
            "range": "6.408e-06",
            "unit": "ms",
            "extra": "2*Stdev = 6.408e-06 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000149197,
            "range": "1.1102e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.1102e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 236.5864096,
            "range": "5.48593",
            "unit": "ms",
            "extra": "2*Stdev = 5.48593 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 0.894926409,
            "range": "0.0379888",
            "unit": "ms",
            "extra": "2*Stdev = 0.0379888 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 1.671878643,
            "range": "0.124611",
            "unit": "ms",
            "extra": "2*Stdev = 0.124611 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.131139175,
            "range": "0.105327",
            "unit": "ms",
            "extra": "2*Stdev = 0.105327 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 384.1189246,
            "range": "10.4244",
            "unit": "ms",
            "extra": "2*Stdev = 10.4244 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 363.261142,
            "range": "12.6247",
            "unit": "ms",
            "extra": "2*Stdev = 12.6247 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 295.2755606,
            "range": "3.33187",
            "unit": "ms",
            "extra": "2*Stdev = 3.33187 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 242.4880558,
            "range": "12.3197",
            "unit": "ms",
            "extra": "2*Stdev = 12.3197 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 2186.5286732,
            "range": "9.54694",
            "unit": "ms",
            "extra": "2*Stdev = 9.54694 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 2221.9410422,
            "range": "27.8886",
            "unit": "ms",
            "extra": "2*Stdev = 27.8886 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 100.983359,
            "range": "9.00254",
            "unit": "ms",
            "extra": "2*Stdev = 9.00254 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 61.9583232,
            "range": "4.53399",
            "unit": "ms",
            "extra": "2*Stdev = 4.53399 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 548.5801388,
            "range": "7.46741",
            "unit": "ms",
            "extra": "2*Stdev = 7.46741 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 319.7303066,
            "range": "18.9748",
            "unit": "ms",
            "extra": "2*Stdev = 18.9748 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 150.5087948,
            "range": "4.84914",
            "unit": "ms",
            "extra": "2*Stdev = 4.84914 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 15.96482335,
            "range": "0.714723",
            "unit": "ms",
            "extra": "2*Stdev = 0.714723 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.32808985,
            "range": "0.107682",
            "unit": "ms",
            "extra": "2*Stdev = 0.107682 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 32.1833669,
            "range": "2.1064",
            "unit": "ms",
            "extra": "2*Stdev = 2.1064 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 0.940955973,
            "range": "0.045488",
            "unit": "ms",
            "extra": "2*Stdev = 0.045488 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 4.143225975,
            "range": "0.178599",
            "unit": "ms",
            "extra": "2*Stdev = 0.178599 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.096227219,
            "range": "0.00702036",
            "unit": "ms",
            "extra": "2*Stdev = 0.00702036 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 254.7551002,
            "range": "14.2271",
            "unit": "ms",
            "extra": "2*Stdev = 14.2271 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 10.4743911,
            "range": "0.694846",
            "unit": "ms",
            "extra": "2*Stdev = 0.694846 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 9.663496975,
            "range": "0.712229",
            "unit": "ms",
            "extra": "2*Stdev = 0.712229 ms"
          },
          {
            "name": "Long variable names",
            "value": 21.2929054,
            "range": "2.01646",
            "unit": "ms",
            "extra": "2*Stdev = 2.01646 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 28.64756255,
            "range": "2.84765",
            "unit": "ms",
            "extra": "2*Stdev = 2.84765 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 139.8327122,
            "range": "12.2854",
            "unit": "ms",
            "extra": "2*Stdev = 12.2854 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 158.5602428,
            "range": "14.8939",
            "unit": "ms",
            "extra": "2*Stdev = 14.8939 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 164.6820305,
            "range": "4.12985",
            "unit": "ms",
            "extra": "2*Stdev = 4.12985 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 148.3797308,
            "range": "12.738",
            "unit": "ms",
            "extra": "2*Stdev = 12.738 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 153.1454156,
            "range": "5.93842",
            "unit": "ms",
            "extra": "2*Stdev = 5.93842 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1358.9294166,
            "range": "51.2693",
            "unit": "ms",
            "extra": "2*Stdev = 51.2693 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 266.3861372,
            "range": "19.7204",
            "unit": "ms",
            "extra": "2*Stdev = 19.7204 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 201.3073372,
            "range": "3.64681",
            "unit": "ms",
            "extra": "2*Stdev = 3.64681 ms"
          },
          {
            "name": "Whitespace",
            "value": 15.401742,
            "range": "1.41573",
            "unit": "ms",
            "extra": "2*Stdev = 1.41573 ms"
          },
          {
            "name": "Line comment",
            "value": 296.901583,
            "range": "16.221",
            "unit": "ms",
            "extra": "2*Stdev = 16.221 ms"
          },
          {
            "name": "Block comment",
            "value": 265.8901318,
            "range": "11.8506",
            "unit": "ms",
            "extra": "2*Stdev = 11.8506 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 798.2161684,
            "range": "33.4397",
            "unit": "ms",
            "extra": "2*Stdev = 33.4397 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 830.6744282,
            "range": "51.183",
            "unit": "ms",
            "extra": "2*Stdev = 51.183 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "64a6a623ebc445cd22e62bca88ba8e8363b9e25f",
          "message": "Keep REPL bindings in a `TypingContext`. (#2855)\n\n* Keep REPL bindings in a TypingContext.\n\nEach command typechecks and normalizes only the new expression, instead of wrapping every earlier :let around it again.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* faster :let - defer normal form until needed\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T15:12:49Z",
          "tree_id": "b5f6fa9f8be0c4b4bb986593a715cccfc6a42f54",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/64a6a623ebc445cd22e62bca88ba8e8363b9e25f"
        },
        "date": 1790695410984,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052020778,
            "range": "0.00494638",
            "unit": "ms",
            "extra": "2*Stdev = 0.00494638 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.052903554,
            "range": "0.00406582",
            "unit": "ms",
            "extra": "2*Stdev = 0.00406582 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.034239911,
            "range": "0.00282923",
            "unit": "ms",
            "extra": "2*Stdev = 0.00282923 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 430.0380474,
            "range": "16.58",
            "unit": "ms",
            "extra": "2*Stdev = 16.58 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017537123,
            "range": "0.0017018",
            "unit": "ms",
            "extra": "2*Stdev = 0.0017018 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 338.4722532,
            "range": "12.4685",
            "unit": "ms",
            "extra": "2*Stdev = 12.4685 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.047341778,
            "range": "0.00337302",
            "unit": "ms",
            "extra": "2*Stdev = 0.00337302 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 86.7889944,
            "range": "2.7297",
            "unit": "ms",
            "extra": "2*Stdev = 2.7297 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.02052576,
            "range": "0.00139408",
            "unit": "ms",
            "extra": "2*Stdev = 0.00139408 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 273.2889206,
            "range": "5.08215",
            "unit": "ms",
            "extra": "2*Stdev = 5.08215 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.017871979,
            "range": "0.00154179",
            "unit": "ms",
            "extra": "2*Stdev = 0.00154179 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 211.453059,
            "range": "3.19062",
            "unit": "ms",
            "extra": "2*Stdev = 3.19062 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.055542017,
            "range": "0.005527",
            "unit": "ms",
            "extra": "2*Stdev = 0.005527 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 198.1771563,
            "range": "2.69245",
            "unit": "ms",
            "extra": "2*Stdev = 2.69245 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.048312301,
            "range": "0.00364615",
            "unit": "ms",
            "extra": "2*Stdev = 0.00364615 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 253.4462634,
            "range": "6.2851",
            "unit": "ms",
            "extra": "2*Stdev = 6.2851 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000514636,
            "range": "2.6452e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.6452e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011277695,
            "range": "0.000358702",
            "unit": "ms",
            "extra": "2*Stdev = 0.000358702 ms"
          },
          {
            "name": "large1.parse",
            "value": 173.1881632,
            "range": "6.94747",
            "unit": "ms",
            "extra": "2*Stdev = 6.94747 ms"
          },
          {
            "name": "large1.resolve",
            "value": 60.6594531,
            "range": "2.4803",
            "unit": "ms",
            "extra": "2*Stdev = 2.4803 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 73.9333414,
            "range": "3.43368",
            "unit": "ms",
            "extra": "2*Stdev = 3.43368 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 144.2921098,
            "range": "6.1135",
            "unit": "ms",
            "extra": "2*Stdev = 6.1135 ms"
          },
          {
            "name": "large2.normalize",
            "value": 165.4690365,
            "range": "15.3333",
            "unit": "ms",
            "extra": "2*Stdev = 15.3333 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 341.358502,
            "range": "11.3594",
            "unit": "ms",
            "extra": "2*Stdev = 11.3594 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 806.3955424,
            "range": "15.3608",
            "unit": "ms",
            "extra": "2*Stdev = 15.3608 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2451.92060015,
            "range": "141.519",
            "unit": "ms",
            "extra": "2*Stdev = 141.519 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 399.832251,
            "range": "9.76302",
            "unit": "ms",
            "extra": "2*Stdev = 9.76302 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.665961565,
            "range": "0.0581231",
            "unit": "ms",
            "extra": "2*Stdev = 0.0581231 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1562.739185,
            "range": "66.4698",
            "unit": "ms",
            "extra": "2*Stdev = 66.4698 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 89.5415724,
            "range": "3.02038",
            "unit": "ms",
            "extra": "2*Stdev = 3.02038 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.403077407,
            "range": "0.0218334",
            "unit": "ms",
            "extra": "2*Stdev = 0.0218334 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2484.4370149,
            "range": "76.5262",
            "unit": "ms",
            "extra": "2*Stdev = 76.5262 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5098.7500724,
            "range": "38.0894",
            "unit": "ms",
            "extra": "2*Stdev = 38.0894 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 706.0892248,
            "range": "58.6221",
            "unit": "ms",
            "extra": "2*Stdev = 58.6221 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2520.5952818,
            "range": "147.679",
            "unit": "ms",
            "extra": "2*Stdev = 147.679 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5392.2683292,
            "range": "69.3071",
            "unit": "ms",
            "extra": "2*Stdev = 69.3071 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.266751579,
            "range": "0.0210787",
            "unit": "ms",
            "extra": "2*Stdev = 0.0210787 ms"
          },
          {
            "name": "large4.resolve",
            "value": 315.1419698,
            "range": "11.404",
            "unit": "ms",
            "extra": "2*Stdev = 11.404 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 422.7975806,
            "range": "5.48044",
            "unit": "ms",
            "extra": "2*Stdev = 5.48044 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.92746064,
            "range": "0.081712",
            "unit": "ms",
            "extra": "2*Stdev = 0.081712 ms"
          },
          {
            "name": "large5.resolve",
            "value": 135.692365,
            "range": "4.87038",
            "unit": "ms",
            "extra": "2*Stdev = 4.87038 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 67.976834,
            "range": "4.16114",
            "unit": "ms",
            "extra": "2*Stdev = 4.16114 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.1815416,
            "range": "5.60186",
            "unit": "ms",
            "extra": "2*Stdev = 5.60186 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 851.1013922,
            "range": "41.723",
            "unit": "ms",
            "extra": "2*Stdev = 41.723 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000216158,
            "range": "1.414e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.414e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000215173,
            "range": "1.1272e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.1272e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 419.0867972,
            "range": "4.97572",
            "unit": "ms",
            "extra": "2*Stdev = 4.97572 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.174829465,
            "range": "0.0615048",
            "unit": "ms",
            "extra": "2*Stdev = 0.0615048 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.251136875,
            "range": "0.144614",
            "unit": "ms",
            "extra": "2*Stdev = 0.144614 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.393809818,
            "range": "0.0950328",
            "unit": "ms",
            "extra": "2*Stdev = 0.0950328 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 553.8745898,
            "range": "25.4785",
            "unit": "ms",
            "extra": "2*Stdev = 25.4785 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 507.5066374,
            "range": "46.2617",
            "unit": "ms",
            "extra": "2*Stdev = 46.2617 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 403.6850756,
            "range": "4.07958",
            "unit": "ms",
            "extra": "2*Stdev = 4.07958 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 334.2003508,
            "range": "7.65518",
            "unit": "ms",
            "extra": "2*Stdev = 7.65518 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 3068.4401934,
            "range": "213.168",
            "unit": "ms",
            "extra": "2*Stdev = 213.168 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3097.581659,
            "range": "92.1576",
            "unit": "ms",
            "extra": "2*Stdev = 92.1576 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 181.988761,
            "range": "1.2062",
            "unit": "ms",
            "extra": "2*Stdev = 1.2062 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 79.506424,
            "range": "6.11779",
            "unit": "ms",
            "extra": "2*Stdev = 6.11779 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 881.7558904,
            "range": "22.0842",
            "unit": "ms",
            "extra": "2*Stdev = 22.0842 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 452.4769326,
            "range": "9.76427",
            "unit": "ms",
            "extra": "2*Stdev = 9.76427 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 201.0191046,
            "range": "3.75096",
            "unit": "ms",
            "extra": "2*Stdev = 3.75096 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.8989363,
            "range": "1.80032",
            "unit": "ms",
            "extra": "2*Stdev = 1.80032 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.753594181,
            "range": "0.120748",
            "unit": "ms",
            "extra": "2*Stdev = 0.120748 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 47.8562054,
            "range": "2.07789",
            "unit": "ms",
            "extra": "2*Stdev = 2.07789 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.424349221,
            "range": "0.132536",
            "unit": "ms",
            "extra": "2*Stdev = 0.132536 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.5443287,
            "range": "0.0908376",
            "unit": "ms",
            "extra": "2*Stdev = 0.0908376 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.144279685,
            "range": "0.010607",
            "unit": "ms",
            "extra": "2*Stdev = 0.010607 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 300.4289791,
            "range": "6.95152",
            "unit": "ms",
            "extra": "2*Stdev = 6.95152 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.83514645,
            "range": "0.576732",
            "unit": "ms",
            "extra": "2*Stdev = 0.576732 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 14.45776265,
            "range": "0.723704",
            "unit": "ms",
            "extra": "2*Stdev = 0.723704 ms"
          },
          {
            "name": "Long variable names",
            "value": 31.36687955,
            "range": "2.46762",
            "unit": "ms",
            "extra": "2*Stdev = 2.46762 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 41.0906669,
            "range": "3.26765",
            "unit": "ms",
            "extra": "2*Stdev = 3.26765 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 222.8807792,
            "range": "6.53883",
            "unit": "ms",
            "extra": "2*Stdev = 6.53883 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 190.259294,
            "range": "5.07833",
            "unit": "ms",
            "extra": "2*Stdev = 5.07833 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 215.45197535,
            "range": "1.49764",
            "unit": "ms",
            "extra": "2*Stdev = 1.49764 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 218.749985,
            "range": "10.8091",
            "unit": "ms",
            "extra": "2*Stdev = 10.8091 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 205.2300789,
            "range": "16.8304",
            "unit": "ms",
            "extra": "2*Stdev = 16.8304 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1640.9526622,
            "range": "26.4021",
            "unit": "ms",
            "extra": "2*Stdev = 26.4021 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 391.1240964,
            "range": "15.2135",
            "unit": "ms",
            "extra": "2*Stdev = 15.2135 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 297.3732048,
            "range": "8.34643",
            "unit": "ms",
            "extra": "2*Stdev = 8.34643 ms"
          },
          {
            "name": "Whitespace",
            "value": 22.0015224,
            "range": "1.29665",
            "unit": "ms",
            "extra": "2*Stdev = 1.29665 ms"
          },
          {
            "name": "Line comment",
            "value": 388.3878466,
            "range": "26.1623",
            "unit": "ms",
            "extra": "2*Stdev = 26.1623 ms"
          },
          {
            "name": "Block comment",
            "value": 346.9905562,
            "range": "13.066",
            "unit": "ms",
            "extra": "2*Stdev = 13.066 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1207.5515392,
            "range": "10.719",
            "unit": "ms",
            "extra": "2*Stdev = 10.719 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1257.4500436,
            "range": "43.9461",
            "unit": "ms",
            "extra": "2*Stdev = 43.9461 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d47f1260253760241bf22a2624f9895a1881ff56",
          "message": "Record fetched import source text. (#2853)\n\nStatus keeps the text and location of imports that were actually fetched, and decodeSemanticCache reads a hashed semantic-cache entry without downloading the original source.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T18:35:54+02:00",
          "tree_id": "d6a92fd83f62e7e4f3a4ce9ad8ed8c8e2bd48f40",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/d47f1260253760241bf22a2624f9895a1881ff56"
        },
        "date": 1790700384179,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052066338,
            "range": "0.00336846",
            "unit": "ms",
            "extra": "2*Stdev = 0.00336846 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.050796062,
            "range": "0.00286177",
            "unit": "ms",
            "extra": "2*Stdev = 0.00286177 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.027683871,
            "range": "0.00137734",
            "unit": "ms",
            "extra": "2*Stdev = 0.00137734 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 360.4615796,
            "range": "26.2609",
            "unit": "ms",
            "extra": "2*Stdev = 26.2609 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.013323322,
            "range": "0.000759834",
            "unit": "ms",
            "extra": "2*Stdev = 0.000759834 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 323.0977878,
            "range": "5.15173",
            "unit": "ms",
            "extra": "2*Stdev = 5.15173 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.027691332,
            "range": "0.00195034",
            "unit": "ms",
            "extra": "2*Stdev = 0.00195034 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 87.756225,
            "range": "5.0216",
            "unit": "ms",
            "extra": "2*Stdev = 5.0216 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.011982562,
            "range": "0.000857064",
            "unit": "ms",
            "extra": "2*Stdev = 0.000857064 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 294.8514266,
            "range": "2.7144",
            "unit": "ms",
            "extra": "2*Stdev = 2.7144 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.010476689,
            "range": "0.000679442",
            "unit": "ms",
            "extra": "2*Stdev = 0.000679442 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 229.536039,
            "range": "3.13183",
            "unit": "ms",
            "extra": "2*Stdev = 3.13183 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.034248662,
            "range": "0.00219532",
            "unit": "ms",
            "extra": "2*Stdev = 0.00219532 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 202.5965182,
            "range": "4.56065",
            "unit": "ms",
            "extra": "2*Stdev = 4.56065 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.028746856,
            "range": "0.00146176",
            "unit": "ms",
            "extra": "2*Stdev = 0.00146176 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 254.6802914,
            "range": "11.1927",
            "unit": "ms",
            "extra": "2*Stdev = 11.1927 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000423627,
            "range": "3.0046e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.0046e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010126703,
            "range": "0.000811522",
            "unit": "ms",
            "extra": "2*Stdev = 0.000811522 ms"
          },
          {
            "name": "large1.parse",
            "value": 145.2204,
            "range": "3.34818",
            "unit": "ms",
            "extra": "2*Stdev = 3.34818 ms"
          },
          {
            "name": "large1.resolve",
            "value": 57.4684754,
            "range": "2.50062",
            "unit": "ms",
            "extra": "2*Stdev = 2.50062 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 68.9323636,
            "range": "4.55191",
            "unit": "ms",
            "extra": "2*Stdev = 4.55191 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 125.6387418,
            "range": "4.23269",
            "unit": "ms",
            "extra": "2*Stdev = 4.23269 ms"
          },
          {
            "name": "large2.normalize",
            "value": 161.5295606,
            "range": "2.74408",
            "unit": "ms",
            "extra": "2*Stdev = 2.74408 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 333.2429136,
            "range": "21.9499",
            "unit": "ms",
            "extra": "2*Stdev = 21.9499 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 788.2517486,
            "range": "7.15887",
            "unit": "ms",
            "extra": "2*Stdev = 7.15887 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2507.64449605,
            "range": "136.172",
            "unit": "ms",
            "extra": "2*Stdev = 136.172 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 399.4321644,
            "range": "2.72394",
            "unit": "ms",
            "extra": "2*Stdev = 2.72394 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.635523406,
            "range": "0.0459907",
            "unit": "ms",
            "extra": "2*Stdev = 0.0459907 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1595.1858708,
            "range": "14.3365",
            "unit": "ms",
            "extra": "2*Stdev = 14.3365 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 92.9091698,
            "range": "2.88503",
            "unit": "ms",
            "extra": "2*Stdev = 2.88503 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.367043871,
            "range": "0.0288522",
            "unit": "ms",
            "extra": "2*Stdev = 0.0288522 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2448.1314133,
            "range": "225.237",
            "unit": "ms",
            "extra": "2*Stdev = 225.237 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5170.3984226,
            "range": "128.092",
            "unit": "ms",
            "extra": "2*Stdev = 128.092 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 738.5954672,
            "range": "36.9795",
            "unit": "ms",
            "extra": "2*Stdev = 36.9795 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2577.50600355,
            "range": "211.66",
            "unit": "ms",
            "extra": "2*Stdev = 211.66 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5540.0460006,
            "range": "56.1599",
            "unit": "ms",
            "extra": "2*Stdev = 56.1599 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.230714835,
            "range": "0.0107825",
            "unit": "ms",
            "extra": "2*Stdev = 0.0107825 ms"
          },
          {
            "name": "large4.resolve",
            "value": 274.8301108,
            "range": "4.13743",
            "unit": "ms",
            "extra": "2*Stdev = 4.13743 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 413.784312,
            "range": "4.29391",
            "unit": "ms",
            "extra": "2*Stdev = 4.29391 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.770989412,
            "range": "0.064067",
            "unit": "ms",
            "extra": "2*Stdev = 0.064067 ms"
          },
          {
            "name": "large5.resolve",
            "value": 119.7780198,
            "range": "5.71644",
            "unit": "ms",
            "extra": "2*Stdev = 5.71644 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 65.4297858,
            "range": "5.46812",
            "unit": "ms",
            "extra": "2*Stdev = 5.46812 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.0679532,
            "range": "5.22061",
            "unit": "ms",
            "extra": "2*Stdev = 5.22061 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 823.3780272,
            "range": "4.74054",
            "unit": "ms",
            "extra": "2*Stdev = 4.74054 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000197495,
            "range": "1.312e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.312e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000200514,
            "range": "1.402e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.402e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 314.7602336,
            "range": "15.1422",
            "unit": "ms",
            "extra": "2*Stdev = 15.1422 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.159701318,
            "range": "0.0917993",
            "unit": "ms",
            "extra": "2*Stdev = 0.0917993 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.167847575,
            "range": "0.120378",
            "unit": "ms",
            "extra": "2*Stdev = 0.120378 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.424316531,
            "range": "0.0891639",
            "unit": "ms",
            "extra": "2*Stdev = 0.0891639 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 526.6170686,
            "range": "36.4746",
            "unit": "ms",
            "extra": "2*Stdev = 36.4746 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 460.9866536,
            "range": "14.8802",
            "unit": "ms",
            "extra": "2*Stdev = 14.8802 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 378.9174656,
            "range": "3.34265",
            "unit": "ms",
            "extra": "2*Stdev = 3.34265 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 316.7360432,
            "range": "3.76068",
            "unit": "ms",
            "extra": "2*Stdev = 3.76068 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 2785.1215324,
            "range": "37.2482",
            "unit": "ms",
            "extra": "2*Stdev = 37.2482 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 2816.3066556,
            "range": "36.1988",
            "unit": "ms",
            "extra": "2*Stdev = 36.1988 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 133.5885664,
            "range": "5.55965",
            "unit": "ms",
            "extra": "2*Stdev = 5.55965 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 76.954592,
            "range": "1.54215",
            "unit": "ms",
            "extra": "2*Stdev = 1.54215 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 737.890651,
            "range": "6.33294",
            "unit": "ms",
            "extra": "2*Stdev = 6.33294 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 420.0061412,
            "range": "3.53624",
            "unit": "ms",
            "extra": "2*Stdev = 3.53624 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 197.2711656,
            "range": "3.26035",
            "unit": "ms",
            "extra": "2*Stdev = 3.26035 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.2822915,
            "range": "1.43355",
            "unit": "ms",
            "extra": "2*Stdev = 1.43355 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.739921962,
            "range": "0.173244",
            "unit": "ms",
            "extra": "2*Stdev = 0.173244 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 44.553376,
            "range": "4.14838",
            "unit": "ms",
            "extra": "2*Stdev = 4.14838 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.250375531,
            "range": "0.114132",
            "unit": "ms",
            "extra": "2*Stdev = 0.114132 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.500504525,
            "range": "0.33537",
            "unit": "ms",
            "extra": "2*Stdev = 0.33537 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.127926559,
            "range": "0.00709203",
            "unit": "ms",
            "extra": "2*Stdev = 0.00709203 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 297.2418223,
            "range": "22.6221",
            "unit": "ms",
            "extra": "2*Stdev = 22.6221 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.4396198,
            "range": "0.574641",
            "unit": "ms",
            "extra": "2*Stdev = 0.574641 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.782087475,
            "range": "0.831991",
            "unit": "ms",
            "extra": "2*Stdev = 0.831991 ms"
          },
          {
            "name": "Long variable names",
            "value": 28.0446141,
            "range": "1.93007",
            "unit": "ms",
            "extra": "2*Stdev = 1.93007 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 37.77607095,
            "range": "1.15478",
            "unit": "ms",
            "extra": "2*Stdev = 1.15478 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 192.2565863,
            "range": "7.35517",
            "unit": "ms",
            "extra": "2*Stdev = 7.35517 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 199.9417864,
            "range": "11.1501",
            "unit": "ms",
            "extra": "2*Stdev = 11.1501 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 222.0102976,
            "range": "18.5577",
            "unit": "ms",
            "extra": "2*Stdev = 18.5577 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 200.3216794,
            "range": "13.2656",
            "unit": "ms",
            "extra": "2*Stdev = 13.2656 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 202.2206276,
            "range": "19.5885",
            "unit": "ms",
            "extra": "2*Stdev = 19.5885 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1782.6691596,
            "range": "90.49",
            "unit": "ms",
            "extra": "2*Stdev = 90.49 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 376.1805142,
            "range": "26.0625",
            "unit": "ms",
            "extra": "2*Stdev = 26.0625 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 297.8669982,
            "range": "28.466",
            "unit": "ms",
            "extra": "2*Stdev = 28.466 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.6454717,
            "range": "1.60084",
            "unit": "ms",
            "extra": "2*Stdev = 1.60084 ms"
          },
          {
            "name": "Line comment",
            "value": 377.9042158,
            "range": "15.2245",
            "unit": "ms",
            "extra": "2*Stdev = 15.2245 ms"
          },
          {
            "name": "Block comment",
            "value": 323.7364498,
            "range": "2.96977",
            "unit": "ms",
            "extra": "2*Stdev = 2.96977 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1037.8536422,
            "range": "15.0323",
            "unit": "ms",
            "extra": "2*Stdev = 15.0323 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1108.0910696,
            "range": "28.2468",
            "unit": "ms",
            "extra": "2*Stdev = 28.2468 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "31712f72952b7be02cfac289501fd24b5b63a2f7",
          "message": "Report every failed import from the `dhall` executable. (#2856)\n\n* Report every failed import from the dhall executable.\n\nThe executable records each import failure and exits without a result. load and loadWith stay fail-fast unless the caller sets CollectErrors.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* add source spans for collected import errors\n\n* Test fail-fast load against collecting every missing import.\n\nloadRelativeTo must stop at the first missing file, and loadCollecting must report both.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T20:14:03+02:00",
          "tree_id": "d8bcd36d078d54c310400b05769aee6c64038a1a",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/31712f72952b7be02cfac289501fd24b5b63a2f7"
        },
        "date": 1790706367178,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.05234913,
            "range": "0.0017477",
            "unit": "ms",
            "extra": "2*Stdev = 0.0017477 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.053505648,
            "range": "0.00529346",
            "unit": "ms",
            "extra": "2*Stdev = 0.00529346 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.032737887,
            "range": "0.00219936",
            "unit": "ms",
            "extra": "2*Stdev = 0.00219936 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 408.2896804,
            "range": "2.87669",
            "unit": "ms",
            "extra": "2*Stdev = 2.87669 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.017325364,
            "range": "0.000974986",
            "unit": "ms",
            "extra": "2*Stdev = 0.000974986 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 327.5632806,
            "range": "24.1707",
            "unit": "ms",
            "extra": "2*Stdev = 24.1707 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.045617725,
            "range": "0.00312775",
            "unit": "ms",
            "extra": "2*Stdev = 0.00312775 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 85.2184592,
            "range": "4.29578",
            "unit": "ms",
            "extra": "2*Stdev = 4.29578 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.019219261,
            "range": "0.00183526",
            "unit": "ms",
            "extra": "2*Stdev = 0.00183526 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 265.576584,
            "range": "3.84304",
            "unit": "ms",
            "extra": "2*Stdev = 3.84304 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.016644803,
            "range": "0.0015317",
            "unit": "ms",
            "extra": "2*Stdev = 0.0015317 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 209.6662178,
            "range": "2.68605",
            "unit": "ms",
            "extra": "2*Stdev = 2.68605 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053209964,
            "range": "0.00453639",
            "unit": "ms",
            "extra": "2*Stdev = 0.00453639 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 194.4710902,
            "range": "10.9786",
            "unit": "ms",
            "extra": "2*Stdev = 10.9786 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.045443135,
            "range": "0.00223668",
            "unit": "ms",
            "extra": "2*Stdev = 0.00223668 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 245.3849454,
            "range": "6.25353",
            "unit": "ms",
            "extra": "2*Stdev = 6.25353 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.00047049,
            "range": "4.3772e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.3772e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011534358,
            "range": "0.00068209",
            "unit": "ms",
            "extra": "2*Stdev = 0.00068209 ms"
          },
          {
            "name": "large1.parse",
            "value": 169.4834444,
            "range": "3.72909",
            "unit": "ms",
            "extra": "2*Stdev = 3.72909 ms"
          },
          {
            "name": "large1.resolve",
            "value": 57.7802661,
            "range": "3.07059",
            "unit": "ms",
            "extra": "2*Stdev = 3.07059 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 69.951413,
            "range": "4.59053",
            "unit": "ms",
            "extra": "2*Stdev = 4.59053 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 139.686928,
            "range": "4.92377",
            "unit": "ms",
            "extra": "2*Stdev = 4.92377 ms"
          },
          {
            "name": "large2.normalize",
            "value": 167.0918354,
            "range": "6.8511",
            "unit": "ms",
            "extra": "2*Stdev = 6.8511 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 339.448379,
            "range": "12.7567",
            "unit": "ms",
            "extra": "2*Stdev = 12.7567 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 819.6104596,
            "range": "23.17",
            "unit": "ms",
            "extra": "2*Stdev = 23.17 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2407.7963541,
            "range": "121.934",
            "unit": "ms",
            "extra": "2*Stdev = 121.934 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 394.8474482,
            "range": "2.7595",
            "unit": "ms",
            "extra": "2*Stdev = 2.7595 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.669675403,
            "range": "0.0493628",
            "unit": "ms",
            "extra": "2*Stdev = 0.0493628 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1594.459827,
            "range": "100.14",
            "unit": "ms",
            "extra": "2*Stdev = 100.14 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 91.214241,
            "range": "4.87601",
            "unit": "ms",
            "extra": "2*Stdev = 4.87601 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.403401096,
            "range": "0.0385549",
            "unit": "ms",
            "extra": "2*Stdev = 0.0385549 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2466.7214418,
            "range": "31.7874",
            "unit": "ms",
            "extra": "2*Stdev = 31.7874 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5039.4652802,
            "range": "184.366",
            "unit": "ms",
            "extra": "2*Stdev = 184.366 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 798.3850408,
            "range": "9.44028",
            "unit": "ms",
            "extra": "2*Stdev = 9.44028 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2794.97513905,
            "range": "136.621",
            "unit": "ms",
            "extra": "2*Stdev = 136.621 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 6180.0543806,
            "range": "382.591",
            "unit": "ms",
            "extra": "2*Stdev = 382.591 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.26526661,
            "range": "0.0229372",
            "unit": "ms",
            "extra": "2*Stdev = 0.0229372 ms"
          },
          {
            "name": "large4.resolve",
            "value": 324.7150002,
            "range": "2.89766",
            "unit": "ms",
            "extra": "2*Stdev = 2.89766 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 447.6070802,
            "range": "31.2297",
            "unit": "ms",
            "extra": "2*Stdev = 31.2297 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 2.046780642,
            "range": "0.0507168",
            "unit": "ms",
            "extra": "2*Stdev = 0.0507168 ms"
          },
          {
            "name": "large5.resolve",
            "value": 136.635078,
            "range": "7.21941",
            "unit": "ms",
            "extra": "2*Stdev = 7.21941 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 70.6974462,
            "range": "4.62057",
            "unit": "ms",
            "extra": "2*Stdev = 4.62057 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 76.8796156,
            "range": "7.06323",
            "unit": "ms",
            "extra": "2*Stdev = 7.06323 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 855.424279,
            "range": "33.4835",
            "unit": "ms",
            "extra": "2*Stdev = 33.4835 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000215326,
            "range": "1.1302e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.1302e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000218259,
            "range": "1.6084e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.6084e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 424.1452734,
            "range": "18.3247",
            "unit": "ms",
            "extra": "2*Stdev = 18.3247 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.185277721,
            "range": "0.0514257",
            "unit": "ms",
            "extra": "2*Stdev = 0.0514257 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.377776118,
            "range": "0.100564",
            "unit": "ms",
            "extra": "2*Stdev = 0.100564 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.449618018,
            "range": "0.0885048",
            "unit": "ms",
            "extra": "2*Stdev = 0.0885048 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 577.2523758,
            "range": "22.5832",
            "unit": "ms",
            "extra": "2*Stdev = 22.5832 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 556.0487594,
            "range": "44.9418",
            "unit": "ms",
            "extra": "2*Stdev = 44.9418 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 449.3292362,
            "range": "19.4502",
            "unit": "ms",
            "extra": "2*Stdev = 19.4502 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 344.825521,
            "range": "5.05659",
            "unit": "ms",
            "extra": "2*Stdev = 5.05659 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 3531.363211,
            "range": "121.75",
            "unit": "ms",
            "extra": "2*Stdev = 121.75 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3591.3874948,
            "range": "74.9469",
            "unit": "ms",
            "extra": "2*Stdev = 74.9469 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 186.4591328,
            "range": "13.6341",
            "unit": "ms",
            "extra": "2*Stdev = 13.6341 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 81.6918866,
            "range": "2.90095",
            "unit": "ms",
            "extra": "2*Stdev = 2.90095 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 873.6691236,
            "range": "23.8582",
            "unit": "ms",
            "extra": "2*Stdev = 23.8582 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 479.28273,
            "range": "26.2188",
            "unit": "ms",
            "extra": "2*Stdev = 26.2188 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 208.5696298,
            "range": "5.22773",
            "unit": "ms",
            "extra": "2*Stdev = 5.22773 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.6364907,
            "range": "1.99654",
            "unit": "ms",
            "extra": "2*Stdev = 1.99654 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.830798512,
            "range": "0.121773",
            "unit": "ms",
            "extra": "2*Stdev = 0.121773 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 52.5712134,
            "range": "1.02272",
            "unit": "ms",
            "extra": "2*Stdev = 1.02272 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.355080453,
            "range": "0.0840361",
            "unit": "ms",
            "extra": "2*Stdev = 0.0840361 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.6383782,
            "range": "0.460563",
            "unit": "ms",
            "extra": "2*Stdev = 0.460563 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.138976155,
            "range": "0.0127429",
            "unit": "ms",
            "extra": "2*Stdev = 0.0127429 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 289.4844463,
            "range": "23.0104",
            "unit": "ms",
            "extra": "2*Stdev = 23.0104 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.011798825,
            "range": "0.398204",
            "unit": "ms",
            "extra": "2*Stdev = 0.398204 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.61658895,
            "range": "1.23444",
            "unit": "ms",
            "extra": "2*Stdev = 1.23444 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.7590484,
            "range": "1.3695",
            "unit": "ms",
            "extra": "2*Stdev = 1.3695 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 38.8866231,
            "range": "2.02987",
            "unit": "ms",
            "extra": "2*Stdev = 2.02987 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 229.4815962,
            "range": "9.13584",
            "unit": "ms",
            "extra": "2*Stdev = 9.13584 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 201.3051222,
            "range": "4.50829",
            "unit": "ms",
            "extra": "2*Stdev = 4.50829 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 201.1103304,
            "range": "3.4569",
            "unit": "ms",
            "extra": "2*Stdev = 3.4569 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 219.9599728,
            "range": "14.3893",
            "unit": "ms",
            "extra": "2*Stdev = 14.3893 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 208.29958315,
            "range": "15.0174",
            "unit": "ms",
            "extra": "2*Stdev = 15.0174 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1636.292209,
            "range": "76.0382",
            "unit": "ms",
            "extra": "2*Stdev = 76.0382 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 392.3142316,
            "range": "3.72857",
            "unit": "ms",
            "extra": "2*Stdev = 3.72857 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 301.6824114,
            "range": "6.50669",
            "unit": "ms",
            "extra": "2*Stdev = 6.50669 ms"
          },
          {
            "name": "Whitespace",
            "value": 19.6982836,
            "range": "1.58396",
            "unit": "ms",
            "extra": "2*Stdev = 1.58396 ms"
          },
          {
            "name": "Line comment",
            "value": 358.0268868,
            "range": "32.4587",
            "unit": "ms",
            "extra": "2*Stdev = 32.4587 ms"
          },
          {
            "name": "Block comment",
            "value": 337.5920358,
            "range": "28.1451",
            "unit": "ms",
            "extra": "2*Stdev = 28.1451 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1188.743749,
            "range": "10.7555",
            "unit": "ms",
            "extra": "2*Stdev = 10.7555 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1255.9248164,
            "range": "50.9796",
            "unit": "ms",
            "extra": "2*Stdev = 50.9796 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8aed60d4f4b0635f697a6e5aa57e28d06d441f56",
          "message": "Typecheck imports while avoiding repeated typechecking of child imports. (#2857)\n\n* Typecheck imports against already-checked child imports.\n\nA parent file is checked against types and values already stored for its children, instead of inferring each inlined child again. load still returns the fully inlined expression.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Apply substitutions before typechecking against already-checked imports.\n\nHaskell-API substitutions live in Status rather than in the starting\ncontext, so a parent annotated as ./child.dhall : UserType reported\nUserType as unbound. Substitute the parent first, and add a regression\ntest for that case.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-29T21:30:09+02:00",
          "tree_id": "8ff00234d637739a0e5abb2a2a49d4d3e5ba0c33",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/8aed60d4f4b0635f697a6e5aa57e28d06d441f56"
        },
        "date": 1790710835976,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052638198,
            "range": "0.002809",
            "unit": "ms",
            "extra": "2*Stdev = 0.002809 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.051842227,
            "range": "0.0027783",
            "unit": "ms",
            "extra": "2*Stdev = 0.0027783 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.032495809,
            "range": "0.00208206",
            "unit": "ms",
            "extra": "2*Stdev = 0.00208206 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 410.4843472,
            "range": "16.7977",
            "unit": "ms",
            "extra": "2*Stdev = 16.7977 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.016849193,
            "range": "0.000792526",
            "unit": "ms",
            "extra": "2*Stdev = 0.000792526 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 308.6010208,
            "range": "7.28067",
            "unit": "ms",
            "extra": "2*Stdev = 7.28067 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.046001173,
            "range": "0.00287383",
            "unit": "ms",
            "extra": "2*Stdev = 0.00287383 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 85.937875,
            "range": "4.01112",
            "unit": "ms",
            "extra": "2*Stdev = 4.01112 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.019711579,
            "range": "0.00132105",
            "unit": "ms",
            "extra": "2*Stdev = 0.00132105 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 262.7178474,
            "range": "7.34005",
            "unit": "ms",
            "extra": "2*Stdev = 7.34005 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.01733635,
            "range": "0.00144138",
            "unit": "ms",
            "extra": "2*Stdev = 0.00144138 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 206.7355738,
            "range": "4.70998",
            "unit": "ms",
            "extra": "2*Stdev = 4.70998 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053010155,
            "range": "0.0042113",
            "unit": "ms",
            "extra": "2*Stdev = 0.0042113 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 184.1352748,
            "range": "5.30862",
            "unit": "ms",
            "extra": "2*Stdev = 5.30862 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.046999799,
            "range": "0.00385933",
            "unit": "ms",
            "extra": "2*Stdev = 0.00385933 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 238.4492802,
            "range": "8.17987",
            "unit": "ms",
            "extra": "2*Stdev = 8.17987 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000464283,
            "range": "2.7384e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.7384e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011012909,
            "range": "0.000349844",
            "unit": "ms",
            "extra": "2*Stdev = 0.000349844 ms"
          },
          {
            "name": "large1.parse",
            "value": 171.0553216,
            "range": "4.49031",
            "unit": "ms",
            "extra": "2*Stdev = 4.49031 ms"
          },
          {
            "name": "large1.resolve",
            "value": 56.9141245,
            "range": "2.80497",
            "unit": "ms",
            "extra": "2*Stdev = 2.80497 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 69.0963334,
            "range": "2.04919",
            "unit": "ms",
            "extra": "2*Stdev = 2.04919 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 141.297364,
            "range": "6.75841",
            "unit": "ms",
            "extra": "2*Stdev = 6.75841 ms"
          },
          {
            "name": "large2.normalize",
            "value": 162.0188524,
            "range": "2.79068",
            "unit": "ms",
            "extra": "2*Stdev = 2.79068 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 341.1168412,
            "range": "3.08777",
            "unit": "ms",
            "extra": "2*Stdev = 3.08777 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 792.4534802,
            "range": "38.11",
            "unit": "ms",
            "extra": "2*Stdev = 38.11 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2434.09299,
            "range": "138.047",
            "unit": "ms",
            "extra": "2*Stdev = 138.047 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 396.0694356,
            "range": "11.9077",
            "unit": "ms",
            "extra": "2*Stdev = 11.9077 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.652953718,
            "range": "0.0508831",
            "unit": "ms",
            "extra": "2*Stdev = 0.0508831 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1581.6554866,
            "range": "48.8147",
            "unit": "ms",
            "extra": "2*Stdev = 48.8147 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 88.5968398,
            "range": "1.42692",
            "unit": "ms",
            "extra": "2*Stdev = 1.42692 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.404743951,
            "range": "0.02218",
            "unit": "ms",
            "extra": "2*Stdev = 0.02218 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2496.7119111,
            "range": "150.76",
            "unit": "ms",
            "extra": "2*Stdev = 150.76 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4887.4133942,
            "range": "384.136",
            "unit": "ms",
            "extra": "2*Stdev = 384.136 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 681.6360932,
            "range": "18.3208",
            "unit": "ms",
            "extra": "2*Stdev = 18.3208 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2506.55317435,
            "range": "85.9704",
            "unit": "ms",
            "extra": "2*Stdev = 85.9704 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5173.380405,
            "range": "201.731",
            "unit": "ms",
            "extra": "2*Stdev = 201.731 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.263872976,
            "range": "0.0212193",
            "unit": "ms",
            "extra": "2*Stdev = 0.0212193 ms"
          },
          {
            "name": "large4.resolve",
            "value": 311.335728,
            "range": "14.8868",
            "unit": "ms",
            "extra": "2*Stdev = 14.8868 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 427.1987722,
            "range": "3.39682",
            "unit": "ms",
            "extra": "2*Stdev = 3.39682 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.925149175,
            "range": "0.0876374",
            "unit": "ms",
            "extra": "2*Stdev = 0.0876374 ms"
          },
          {
            "name": "large5.resolve",
            "value": 133.707046,
            "range": "4.13423",
            "unit": "ms",
            "extra": "2*Stdev = 4.13423 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.5399902,
            "range": "3.36957",
            "unit": "ms",
            "extra": "2*Stdev = 3.36957 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.006795,
            "range": "5.7011",
            "unit": "ms",
            "extra": "2*Stdev = 5.7011 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 872.8070674,
            "range": "22.9104",
            "unit": "ms",
            "extra": "2*Stdev = 22.9104 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000207753,
            "range": "1.214e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.214e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000216868,
            "range": "2.1316e-05",
            "unit": "ms",
            "extra": "2*Stdev = 2.1316e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 406.1199346,
            "range": "7.78767",
            "unit": "ms",
            "extra": "2*Stdev = 7.78767 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.151082678,
            "range": "0.0822736",
            "unit": "ms",
            "extra": "2*Stdev = 0.0822736 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.305880837,
            "range": "0.213101",
            "unit": "ms",
            "extra": "2*Stdev = 0.213101 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.418695237,
            "range": "0.103701",
            "unit": "ms",
            "extra": "2*Stdev = 0.103701 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 548.2747166,
            "range": "13.5658",
            "unit": "ms",
            "extra": "2*Stdev = 13.5658 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 488.4176298,
            "range": "30.5068",
            "unit": "ms",
            "extra": "2*Stdev = 30.5068 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 397.7246028,
            "range": "11.3289",
            "unit": "ms",
            "extra": "2*Stdev = 11.3289 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 340.7407028,
            "range": "13.7998",
            "unit": "ms",
            "extra": "2*Stdev = 13.7998 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 2922.7990466,
            "range": "71.1843",
            "unit": "ms",
            "extra": "2*Stdev = 71.1843 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 3040.456219,
            "range": "109.925",
            "unit": "ms",
            "extra": "2*Stdev = 109.925 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 182.8279036,
            "range": "14.989",
            "unit": "ms",
            "extra": "2*Stdev = 14.989 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 83.2002165,
            "range": "6.20944",
            "unit": "ms",
            "extra": "2*Stdev = 6.20944 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 865.262585,
            "range": "33.1442",
            "unit": "ms",
            "extra": "2*Stdev = 33.1442 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 378.5797496,
            "range": "5.83193",
            "unit": "ms",
            "extra": "2*Stdev = 5.83193 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 198.9022542,
            "range": "3.27266",
            "unit": "ms",
            "extra": "2*Stdev = 3.27266 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.3700472,
            "range": "1.03009",
            "unit": "ms",
            "extra": "2*Stdev = 1.03009 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.837435706,
            "range": "0.10812",
            "unit": "ms",
            "extra": "2*Stdev = 0.10812 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 49.0030841,
            "range": "3.5149",
            "unit": "ms",
            "extra": "2*Stdev = 3.5149 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.299182418,
            "range": "0.101501",
            "unit": "ms",
            "extra": "2*Stdev = 0.101501 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.410441112,
            "range": "0.232963",
            "unit": "ms",
            "extra": "2*Stdev = 0.232963 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.13998627,
            "range": "0.0119616",
            "unit": "ms",
            "extra": "2*Stdev = 0.0119616 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 287.911686,
            "range": "16.5651",
            "unit": "ms",
            "extra": "2*Stdev = 16.5651 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 13.87012115,
            "range": "0.443032",
            "unit": "ms",
            "extra": "2*Stdev = 0.443032 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 13.7720664,
            "range": "0.891513",
            "unit": "ms",
            "extra": "2*Stdev = 0.891513 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.5718422,
            "range": "1.6112",
            "unit": "ms",
            "extra": "2*Stdev = 1.6112 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 39.8630135,
            "range": "2.20695",
            "unit": "ms",
            "extra": "2*Stdev = 2.20695 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 224.706686,
            "range": "10.9133",
            "unit": "ms",
            "extra": "2*Stdev = 10.9133 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 200.4724092,
            "range": "5.32488",
            "unit": "ms",
            "extra": "2*Stdev = 5.32488 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 201.7295809,
            "range": "13.5259",
            "unit": "ms",
            "extra": "2*Stdev = 13.5259 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 218.8252557,
            "range": "21.7777",
            "unit": "ms",
            "extra": "2*Stdev = 21.7777 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 198.6294282,
            "range": "11.4465",
            "unit": "ms",
            "extra": "2*Stdev = 11.4465 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1626.6864648,
            "range": "34.0158",
            "unit": "ms",
            "extra": "2*Stdev = 34.0158 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 383.1691516,
            "range": "14.8167",
            "unit": "ms",
            "extra": "2*Stdev = 14.8167 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 297.6831506,
            "range": "3.59952",
            "unit": "ms",
            "extra": "2*Stdev = 3.59952 ms"
          },
          {
            "name": "Whitespace",
            "value": 19.0941849,
            "range": "1.06219",
            "unit": "ms",
            "extra": "2*Stdev = 1.06219 ms"
          },
          {
            "name": "Line comment",
            "value": 406.7519975,
            "range": "14.3967",
            "unit": "ms",
            "extra": "2*Stdev = 14.3967 ms"
          },
          {
            "name": "Block comment",
            "value": 356.4464214,
            "range": "13.9861",
            "unit": "ms",
            "extra": "2*Stdev = 13.9861 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1186.451243,
            "range": "21.2302",
            "unit": "ms",
            "extra": "2*Stdev = 21.2302 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1236.619907,
            "range": "10.728",
            "unit": "ms",
            "extra": "2*Stdev = 10.728 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5737af106e3a9f82fcd604b3c6ea50579de05e03",
          "message": " Evaluate each shared import at most once (#2858)\n\n* Evaluate each import once through a TypingContext.\n\nload still returns the fully inlined expression. The input functions evaluate the shared twin, unless a custom normalizer is set. The diamond benchmark uses the same path.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* Evaluate the default dhall command with shared imports.\n\nLimit flags quote and bound that same evaluation. dhall resolve still prints the fully inlined expression.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* wip fixing slowness\n\n* do not use special names\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-30T15:42:18+02:00",
          "tree_id": "6bc916b83f0d0f7ea72c2727e25f60d236fe8858",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/5737af106e3a9f82fcd604b3c6ea50579de05e03"
        },
        "date": 1790776948694,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052025015,
            "range": "0.00402243",
            "unit": "ms",
            "extra": "2*Stdev = 0.00402243 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.051216144,
            "range": "0.00321884",
            "unit": "ms",
            "extra": "2*Stdev = 0.00321884 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.02900378,
            "range": "0.00195303",
            "unit": "ms",
            "extra": "2*Stdev = 0.00195303 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 405.3417538,
            "range": "6.41348",
            "unit": "ms",
            "extra": "2*Stdev = 6.41348 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.013908152,
            "range": "0.000689648",
            "unit": "ms",
            "extra": "2*Stdev = 0.000689648 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 331.2578364,
            "range": "2.00913",
            "unit": "ms",
            "extra": "2*Stdev = 2.00913 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.028864086,
            "range": "0.00132567",
            "unit": "ms",
            "extra": "2*Stdev = 0.00132567 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 88.1471088,
            "range": "3.71842",
            "unit": "ms",
            "extra": "2*Stdev = 3.71842 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.012254133,
            "range": "0.000782958",
            "unit": "ms",
            "extra": "2*Stdev = 0.000782958 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 299.400227,
            "range": "5.01771",
            "unit": "ms",
            "extra": "2*Stdev = 5.01771 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.010600914,
            "range": "0.00094211",
            "unit": "ms",
            "extra": "2*Stdev = 0.00094211 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 233.8414386,
            "range": "8.5098",
            "unit": "ms",
            "extra": "2*Stdev = 8.5098 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.035784602,
            "range": "0.00178873",
            "unit": "ms",
            "extra": "2*Stdev = 0.00178873 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 206.5208544,
            "range": "5.87775",
            "unit": "ms",
            "extra": "2*Stdev = 5.87775 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.029190292,
            "range": "0.00199802",
            "unit": "ms",
            "extra": "2*Stdev = 0.00199802 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 257.2055908,
            "range": "8.3258",
            "unit": "ms",
            "extra": "2*Stdev = 8.3258 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000416504,
            "range": "3.1238e-05",
            "unit": "ms",
            "extra": "2*Stdev = 3.1238e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010870459,
            "range": "0.000920094",
            "unit": "ms",
            "extra": "2*Stdev = 0.000920094 ms"
          },
          {
            "name": "large1.parse",
            "value": 148.9877562,
            "range": "4.69682",
            "unit": "ms",
            "extra": "2*Stdev = 4.69682 ms"
          },
          {
            "name": "large1.resolve",
            "value": 59.880191,
            "range": "2.25045",
            "unit": "ms",
            "extra": "2*Stdev = 2.25045 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 69.9343525,
            "range": "2.39609",
            "unit": "ms",
            "extra": "2*Stdev = 2.39609 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 126.8914912,
            "range": "6.94152",
            "unit": "ms",
            "extra": "2*Stdev = 6.94152 ms"
          },
          {
            "name": "large2.normalize",
            "value": 159.135838,
            "range": "3.38206",
            "unit": "ms",
            "extra": "2*Stdev = 3.38206 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 333.957693,
            "range": "5.49895",
            "unit": "ms",
            "extra": "2*Stdev = 5.49895 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 813.0307576,
            "range": "25.6182",
            "unit": "ms",
            "extra": "2*Stdev = 25.6182 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2581.083913,
            "range": "119.964",
            "unit": "ms",
            "extra": "2*Stdev = 119.964 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 398.285104,
            "range": "5.70163",
            "unit": "ms",
            "extra": "2*Stdev = 5.70163 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.637649878,
            "range": "0.0610772",
            "unit": "ms",
            "extra": "2*Stdev = 0.0610772 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1591.9849492,
            "range": "41.391",
            "unit": "ms",
            "extra": "2*Stdev = 41.391 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 96.0184784,
            "range": "4.71309",
            "unit": "ms",
            "extra": "2*Stdev = 4.71309 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.37477555,
            "range": "0.0214933",
            "unit": "ms",
            "extra": "2*Stdev = 0.0214933 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2623.30389075,
            "range": "132.476",
            "unit": "ms",
            "extra": "2*Stdev = 132.476 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5180.2905872,
            "range": "105.467",
            "unit": "ms",
            "extra": "2*Stdev = 105.467 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 715.0765554,
            "range": "34.5573",
            "unit": "ms",
            "extra": "2*Stdev = 34.5573 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2574.18581395,
            "range": "121.591",
            "unit": "ms",
            "extra": "2*Stdev = 121.591 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5489.6931286,
            "range": "6.74648",
            "unit": "ms",
            "extra": "2*Stdev = 6.74648 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.22963995,
            "range": "0.0201858",
            "unit": "ms",
            "extra": "2*Stdev = 0.0201858 ms"
          },
          {
            "name": "large4.resolve",
            "value": 287.3457512,
            "range": "5.00567",
            "unit": "ms",
            "extra": "2*Stdev = 5.00567 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 413.7760484,
            "range": "23.1161",
            "unit": "ms",
            "extra": "2*Stdev = 23.1161 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.751745653,
            "range": "0.0798212",
            "unit": "ms",
            "extra": "2*Stdev = 0.0798212 ms"
          },
          {
            "name": "large5.resolve",
            "value": 124.0069504,
            "range": "4.75772",
            "unit": "ms",
            "extra": "2*Stdev = 4.75772 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 65.6376884,
            "range": "4.40322",
            "unit": "ms",
            "extra": "2*Stdev = 4.40322 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 72.6237372,
            "range": "5.34587",
            "unit": "ms",
            "extra": "2*Stdev = 5.34587 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 829.7072836,
            "range": "47.417",
            "unit": "ms",
            "extra": "2*Stdev = 47.417 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000191933,
            "range": "1.0402e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.0402e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000196071,
            "range": "1.8332e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.8332e-05 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 327.4222318,
            "range": "13.1227",
            "unit": "ms",
            "extra": "2*Stdev = 13.1227 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.124366025,
            "range": "0.0913899",
            "unit": "ms",
            "extra": "2*Stdev = 0.0913899 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.337554737,
            "range": "0.211231",
            "unit": "ms",
            "extra": "2*Stdev = 0.211231 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.44441285,
            "range": "0.1043",
            "unit": "ms",
            "extra": "2*Stdev = 0.1043 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 544.2634632,
            "range": "11.2404",
            "unit": "ms",
            "extra": "2*Stdev = 11.2404 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 508.3030038,
            "range": "50.2984",
            "unit": "ms",
            "extra": "2*Stdev = 50.2984 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 419.1156082,
            "range": "9.10259",
            "unit": "ms",
            "extra": "2*Stdev = 9.10259 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 330.1114024,
            "range": "7.30392",
            "unit": "ms",
            "extra": "2*Stdev = 7.30392 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 514.894427,
            "range": "8.66942",
            "unit": "ms",
            "extra": "2*Stdev = 8.66942 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 508.663086,
            "range": "3.63515",
            "unit": "ms",
            "extra": "2*Stdev = 3.63515 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 141.7910396,
            "range": "5.19016",
            "unit": "ms",
            "extra": "2*Stdev = 5.19016 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 82.1201579,
            "range": "2.75783",
            "unit": "ms",
            "extra": "2*Stdev = 2.75783 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 795.7491552,
            "range": "14.7875",
            "unit": "ms",
            "extra": "2*Stdev = 14.7875 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 346.4967798,
            "range": "14.498",
            "unit": "ms",
            "extra": "2*Stdev = 14.498 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 188.2102882,
            "range": "3.57759",
            "unit": "ms",
            "extra": "2*Stdev = 3.57759 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 20.24020895,
            "range": "1.82433",
            "unit": "ms",
            "extra": "2*Stdev = 1.82433 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.745491506,
            "range": "0.0924309",
            "unit": "ms",
            "extra": "2*Stdev = 0.0924309 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 46.9319175,
            "range": "3.5",
            "unit": "ms",
            "extra": "2*Stdev = 3.5 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.292317456,
            "range": "0.109523",
            "unit": "ms",
            "extra": "2*Stdev = 0.109523 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 5.358423681,
            "range": "0.367141",
            "unit": "ms",
            "extra": "2*Stdev = 0.367141 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.126867232,
            "range": "0.0115204",
            "unit": "ms",
            "extra": "2*Stdev = 0.0115204 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 303.4054412,
            "range": "25.7874",
            "unit": "ms",
            "extra": "2*Stdev = 25.7874 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.749269075,
            "range": "0.389861",
            "unit": "ms",
            "extra": "2*Stdev = 0.389861 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 14.260340775,
            "range": "0.962185",
            "unit": "ms",
            "extra": "2*Stdev = 0.962185 ms"
          },
          {
            "name": "Long variable names",
            "value": 28.270674,
            "range": "1.49586",
            "unit": "ms",
            "extra": "2*Stdev = 1.49586 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 37.0873283,
            "range": "2.06586",
            "unit": "ms",
            "extra": "2*Stdev = 2.06586 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 187.507044,
            "range": "12.2493",
            "unit": "ms",
            "extra": "2*Stdev = 12.2493 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 180.9093017,
            "range": "11.2145",
            "unit": "ms",
            "extra": "2*Stdev = 11.2145 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 209.1791766,
            "range": "6.57302",
            "unit": "ms",
            "extra": "2*Stdev = 6.57302 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 213.986387,
            "range": "11.6315",
            "unit": "ms",
            "extra": "2*Stdev = 11.6315 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 216.0566478,
            "range": "18.2145",
            "unit": "ms",
            "extra": "2*Stdev = 18.2145 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1791.99844,
            "range": "71.1446",
            "unit": "ms",
            "extra": "2*Stdev = 71.1446 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 371.6800268,
            "range": "26.7155",
            "unit": "ms",
            "extra": "2*Stdev = 26.7155 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 293.9131956,
            "range": "20.6867",
            "unit": "ms",
            "extra": "2*Stdev = 20.6867 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.73970875,
            "range": "0.715478",
            "unit": "ms",
            "extra": "2*Stdev = 0.715478 ms"
          },
          {
            "name": "Line comment",
            "value": 381.2307582,
            "range": "36.7686",
            "unit": "ms",
            "extra": "2*Stdev = 36.7686 ms"
          },
          {
            "name": "Block comment",
            "value": 336.1234282,
            "range": "25.6278",
            "unit": "ms",
            "extra": "2*Stdev = 25.6278 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1053.0866182,
            "range": "16.3557",
            "unit": "ms",
            "extra": "2*Stdev = 16.3557 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1111.2622248,
            "range": "38.522",
            "unit": "ms",
            "extra": "2*Stdev = 38.522 ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "winitzki@users.noreply.github.com",
            "name": "Sergei Winitzki",
            "username": "winitzki"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "58cb37f18a16740aa791456057515755fdfa0f31",
          "message": "LSP: Analyse open Dhall files in the background. (#2860)\n\n* Analyse open Dhall files in the background.\n\nThe server keeps a document snapshot, reports every failed import, reuses a TypingContext across unchanged let bindings, and caps normal forms it shows.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n* set 16KB default for printed normal forms\n\n* Keep hover working when a file imports a let block.\n\nSplitting an import path as a let used to abandon the whole file. Tests cover that hover, truncation at the 16KB type cap, and the lsp-types 2.2 document-change type.\n\nCo-authored-by: Cursor <cursoragent@cursor.com>\n\n---------\n\nCo-authored-by: Cursor <cursoragent@cursor.com>",
          "timestamp": "2026-09-30T18:12:34+02:00",
          "tree_id": "d1298785089e2549c908999b7842364b714571f3",
          "url": "https://github.com/dhall-lang/dhall-haskell/commit/58cb37f18a16740aa791456057515755fdfa0f31"
        },
        "date": 1790785353469,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.052325739,
            "range": "0.00408935",
            "unit": "ms",
            "extra": "2*Stdev = 0.00408935 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.052652001,
            "range": "0.00487832",
            "unit": "ms",
            "extra": "2*Stdev = 0.00487832 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.035720741,
            "range": "0.00326762",
            "unit": "ms",
            "extra": "2*Stdev = 0.00326762 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 430.5460067,
            "range": "9.48491",
            "unit": "ms",
            "extra": "2*Stdev = 9.48491 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.018129008,
            "range": "0.00134202",
            "unit": "ms",
            "extra": "2*Stdev = 0.00134202 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 342.578706,
            "range": "16.1447",
            "unit": "ms",
            "extra": "2*Stdev = 16.1447 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.047085704,
            "range": "0.00279501",
            "unit": "ms",
            "extra": "2*Stdev = 0.00279501 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 88.557244,
            "range": "4.59004",
            "unit": "ms",
            "extra": "2*Stdev = 4.59004 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.02099292,
            "range": "0.00114116",
            "unit": "ms",
            "extra": "2*Stdev = 0.00114116 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 284.6584516,
            "range": "19.0623",
            "unit": "ms",
            "extra": "2*Stdev = 19.0623 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.018958408,
            "range": "0.00178271",
            "unit": "ms",
            "extra": "2*Stdev = 0.00178271 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 220.504836,
            "range": "5.06491",
            "unit": "ms",
            "extra": "2*Stdev = 5.06491 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.055018758,
            "range": "0.00302528",
            "unit": "ms",
            "extra": "2*Stdev = 0.00302528 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 195.1975148,
            "range": "3.05179",
            "unit": "ms",
            "extra": "2*Stdev = 3.05179 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.049951629,
            "range": "0.00378069",
            "unit": "ms",
            "extra": "2*Stdev = 0.00378069 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 265.9554692,
            "range": "18.5329",
            "unit": "ms",
            "extra": "2*Stdev = 18.5329 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000501091,
            "range": "4.7148e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.7148e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.011453822,
            "range": "0.000712628",
            "unit": "ms",
            "extra": "2*Stdev = 0.000712628 ms"
          },
          {
            "name": "large1.parse",
            "value": 175.346326,
            "range": "3.36117",
            "unit": "ms",
            "extra": "2*Stdev = 3.36117 ms"
          },
          {
            "name": "large1.resolve",
            "value": 64.7894665,
            "range": "1.93541",
            "unit": "ms",
            "extra": "2*Stdev = 1.93541 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 76.161775,
            "range": "4.41179",
            "unit": "ms",
            "extra": "2*Stdev = 4.41179 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 143.1113126,
            "range": "11.2947",
            "unit": "ms",
            "extra": "2*Stdev = 11.2947 ms"
          },
          {
            "name": "large2.normalize",
            "value": 164.963239,
            "range": "7.41776",
            "unit": "ms",
            "extra": "2*Stdev = 7.41776 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 348.5431254,
            "range": "11.3437",
            "unit": "ms",
            "extra": "2*Stdev = 11.3437 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 829.3035776,
            "range": "48.5183",
            "unit": "ms",
            "extra": "2*Stdev = 48.5183 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2610.9058425,
            "range": "163.135",
            "unit": "ms",
            "extra": "2*Stdev = 163.135 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 414.9233022,
            "range": "4.33106",
            "unit": "ms",
            "extra": "2*Stdev = 4.33106 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.700344143,
            "range": "0.0441511",
            "unit": "ms",
            "extra": "2*Stdev = 0.0441511 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1732.3198914,
            "range": "48.5763",
            "unit": "ms",
            "extra": "2*Stdev = 48.5763 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 94.650201,
            "range": "4.48767",
            "unit": "ms",
            "extra": "2*Stdev = 4.48767 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.416510368,
            "range": "0.0338194",
            "unit": "ms",
            "extra": "2*Stdev = 0.0338194 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2597.6944968,
            "range": "188.803",
            "unit": "ms",
            "extra": "2*Stdev = 188.803 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 5519.759302,
            "range": "112.711",
            "unit": "ms",
            "extra": "2*Stdev = 112.711 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 726.3733568,
            "range": "35.3805",
            "unit": "ms",
            "extra": "2*Stdev = 35.3805 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2520.7781528,
            "range": "222.98",
            "unit": "ms",
            "extra": "2*Stdev = 222.98 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5918.8134964,
            "range": "53.0215",
            "unit": "ms",
            "extra": "2*Stdev = 53.0215 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.25979578,
            "range": "0.0234235",
            "unit": "ms",
            "extra": "2*Stdev = 0.0234235 ms"
          },
          {
            "name": "large4.resolve",
            "value": 335.7869927,
            "range": "5.75924",
            "unit": "ms",
            "extra": "2*Stdev = 5.75924 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 449.069111,
            "range": "13.7413",
            "unit": "ms",
            "extra": "2*Stdev = 13.7413 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.960948853,
            "range": "0.119962",
            "unit": "ms",
            "extra": "2*Stdev = 0.119962 ms"
          },
          {
            "name": "large5.resolve",
            "value": 138.3654166,
            "range": "3.0896",
            "unit": "ms",
            "extra": "2*Stdev = 3.0896 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 68.412656,
            "range": "3.31472",
            "unit": "ms",
            "extra": "2*Stdev = 3.31472 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 71.4965248,
            "range": "5.98799",
            "unit": "ms",
            "extra": "2*Stdev = 5.98799 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 873.3783612,
            "range": "13.2434",
            "unit": "ms",
            "extra": "2*Stdev = 13.2434 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000204662,
            "range": "1.4622e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.4622e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000211891,
            "range": "5.896e-06",
            "unit": "ms",
            "extra": "2*Stdev = 5.896e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 435.0996294,
            "range": "15.9188",
            "unit": "ms",
            "extra": "2*Stdev = 15.9188 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.213533493,
            "range": "0.0432803",
            "unit": "ms",
            "extra": "2*Stdev = 0.0432803 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.372974687,
            "range": "0.0784394",
            "unit": "ms",
            "extra": "2*Stdev = 0.0784394 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.543799818,
            "range": "0.102229",
            "unit": "ms",
            "extra": "2*Stdev = 0.102229 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 563.1657628,
            "range": "18.1087",
            "unit": "ms",
            "extra": "2*Stdev = 18.1087 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 496.552262,
            "range": "11.9914",
            "unit": "ms",
            "extra": "2*Stdev = 11.9914 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 397.305645,
            "range": "8.55535",
            "unit": "ms",
            "extra": "2*Stdev = 8.55535 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 364.505876,
            "range": "22.8479",
            "unit": "ms",
            "extra": "2*Stdev = 22.8479 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 510.2315878,
            "range": "10.4023",
            "unit": "ms",
            "extra": "2*Stdev = 10.4023 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 520.8307122,
            "range": "47.849",
            "unit": "ms",
            "extra": "2*Stdev = 47.849 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 201.4475658,
            "range": "11.0271",
            "unit": "ms",
            "extra": "2*Stdev = 11.0271 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 89.0587888,
            "range": "2.05588",
            "unit": "ms",
            "extra": "2*Stdev = 2.05588 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 973.5672108,
            "range": "3.79292",
            "unit": "ms",
            "extra": "2*Stdev = 3.79292 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 411.8028694,
            "range": "6.67101",
            "unit": "ms",
            "extra": "2*Stdev = 6.67101 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 201.0395562,
            "range": "11.8337",
            "unit": "ms",
            "extra": "2*Stdev = 11.8337 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 21.9228899,
            "range": "1.92183",
            "unit": "ms",
            "extra": "2*Stdev = 1.92183 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.860511731,
            "range": "0.112958",
            "unit": "ms",
            "extra": "2*Stdev = 0.112958 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 49.4252393,
            "range": "2.52856",
            "unit": "ms",
            "extra": "2*Stdev = 2.52856 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.4017807,
            "range": "0.0834479",
            "unit": "ms",
            "extra": "2*Stdev = 0.0834479 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.620716662,
            "range": "0.251983",
            "unit": "ms",
            "extra": "2*Stdev = 0.251983 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.142616318,
            "range": "0.00544174",
            "unit": "ms",
            "extra": "2*Stdev = 0.00544174 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 309.7073059,
            "range": "2.34205",
            "unit": "ms",
            "extra": "2*Stdev = 2.34205 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 15.04343475,
            "range": "1.50154",
            "unit": "ms",
            "extra": "2*Stdev = 1.50154 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 14.575147675,
            "range": "1.0483",
            "unit": "ms",
            "extra": "2*Stdev = 1.0483 ms"
          },
          {
            "name": "Long variable names",
            "value": 29.5273121,
            "range": "2.76584",
            "unit": "ms",
            "extra": "2*Stdev = 2.76584 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 41.097612,
            "range": "3.87788",
            "unit": "ms",
            "extra": "2*Stdev = 3.87788 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 227.551136,
            "range": "4.89964",
            "unit": "ms",
            "extra": "2*Stdev = 4.89964 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 199.3890536,
            "range": "13.5661",
            "unit": "ms",
            "extra": "2*Stdev = 13.5661 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 203.3482596,
            "range": "9.39125",
            "unit": "ms",
            "extra": "2*Stdev = 9.39125 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 222.6042079,
            "range": "8.80004",
            "unit": "ms",
            "extra": "2*Stdev = 8.80004 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 202.9057464,
            "range": "18.341",
            "unit": "ms",
            "extra": "2*Stdev = 18.341 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1657.0390878,
            "range": "41.6392",
            "unit": "ms",
            "extra": "2*Stdev = 41.6392 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 393.1481872,
            "range": "8.21896",
            "unit": "ms",
            "extra": "2*Stdev = 8.21896 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 293.0433336,
            "range": "19.1045",
            "unit": "ms",
            "extra": "2*Stdev = 19.1045 ms"
          },
          {
            "name": "Whitespace",
            "value": 20.5402681,
            "range": "0.863652",
            "unit": "ms",
            "extra": "2*Stdev = 0.863652 ms"
          },
          {
            "name": "Line comment",
            "value": 384.4365798,
            "range": "14.7955",
            "unit": "ms",
            "extra": "2*Stdev = 14.7955 ms"
          },
          {
            "name": "Block comment",
            "value": 334.255294,
            "range": "3.31814",
            "unit": "ms",
            "extra": "2*Stdev = 3.31814 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1217.457728,
            "range": "34.3734",
            "unit": "ms",
            "extra": "2*Stdev = 34.3734 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1283.9145016,
            "range": "65.9067",
            "unit": "ms",
            "extra": "2*Stdev = 65.9067 ms"
          }
        ]
      }
    ]
  }
}