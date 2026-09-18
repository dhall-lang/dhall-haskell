window.BENCHMARK_DATA = {
  "lastUpdate": 1789753094972,
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
      }
    ]
  }
}