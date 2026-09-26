window.BENCHMARK_DATA = {
  "lastUpdate": 1790456062627,
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
      }
    ]
  }
}