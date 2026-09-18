window.BENCHMARK_DATA = {
  "lastUpdate": 1789738581459,
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
      }
    ]
  }
}