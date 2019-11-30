1.6.0

* BREAKING CHANGE: [Move `HsYAML` code to a new `dhall-yaml` package](https://github.com/dhall-lang/dhall-haskell/pull/1514)
    * This package is now fully BSD-licensed (both for the JSON and YAML
      utilities)
    * The GPL-licensed YAML utilities are now provided as part of the
      `dhall-yaml` package
    * Note that this package still provides `dhall-to-yaml` and the `dhall-yaml`
      package provides a separate `dhall-to-yaml-ng` executable
    * This is a breaking change in the following respects:
        * The YAML-related modules in this package were renamed
        * This package no longer accepts a `-fgpl` cabal configure flag
        * This package no longer provides the `yaml-to-dhall` executable (which
          now resides within the `dhall-yaml` package)
* [BREAKING CHANGE: Rename some options of `dhall-to-{json,yaml}` to more consistent ones](https://github.com/dhall-lang/dhall-haskell/issues/1430):
    * rename `--omitEmpty` to `--omit-empty`
    * rename `--preserveNull` to `--preserve-null`
    * rename `--noMaps` to `--no-maps`
    * drop `--omitNull` as redundant because of `--preserve-null` (see below)
* [Don't normalize schema before type-checking](https://github.com/dhall-lang/dhall-haskell/pull/1555)
    * This improves the error messages for `{json,yaml}-to-dhall`
* [Fix `dhall-to-yaml` to quote special strings](https://github.com/dhall-lang/dhall-haskell/pull/1474)
* [Make some of CLI options for `dhall-json` more consistent](https://github.com/dhall-lang/dhall-haskell/pull/1475)

1.5.0

* [BREAKING CHANGE: Enable `--pretty` by default for `dhall-to-json`](https://github.com/dhall-lang/dhall-haskell/issues/716)
* [BREAKING CHANGE: Enable `--omitNull` by default for `dhall-to-{json,yaml}`](https://github.com/dhall-lang/dhall-haskell/pull/1365)
    * To recover the old behavior use the `--preserveNull` flag
* Add support for building against the `HsYAML` package [#1248](https://github.com/dhall-lang/dhall-haskell/pull/1248) / [#1417](https://github.com/dhall-lang/dhall-haskell/pull/1417) / [#1420](https://github.com/dhall-lang/dhall-haskell/pull/1420)
    * To enable the use of `HsYAML`, enable the `-fgpl` `cabal configure` flag,
      which means that anything built using that flag is GPLv3 licensed
    * By default `dhall-json` builds against `aeson-yaml` which is BSD-3
      licensed
    * The `yaml-to-dhall` executable is only available with the `-fgpl` flag,
      meaning that it is necessarily GPLv3 licensed
* [New `--omissible-lists` flag for `{json,yaml}-to-dhall`](https://github.com/dhall-lang/dhall-haskell/pull/1414)
    * This flag is sort of the inverse of the `--omitEmpty` flag, meaning that
      missing YAML/JSON lists can be translated to present (but empty) Dhall
      lists

1.4.1

* [Enable `--records-strict` by default for `{json-yaml}-to-dhall`](https://github.com/dhall-lang/dhall-haskell/pull/1181)
* [Fix `--file` flag for `dhall-to-{json,yaml}`](https://github.com/dhall-lang/dhall-haskell/pull/1191)
* [Fix `--version` flag for `{yaml,json}-to-dhall`](https://github.com/dhall-lang/dhall-haskell/pull/1199)
* [`Nesting`: Support empty alternatives as contents](https://github.com/dhall-lang/dhall-haskell/pull/1204)
* [Fix `yaml-to-dhall` support for empty objects](https://github.com/dhall-lang/dhall-haskell/pull/1186)
* [Throw error when union value is incompatible with inline nesting](https://github.com/dhall-lang/dhall-haskell/pull/1226)
* [Add `--output` options](https://github.com/dhall-lang/dhall-haskell/pull/1304)
* [Minor bug fixes for `yaml-to-dhall` error messages](https://github.com/dhall-lang/dhall-haskell/pull/1305)

1.4.0

* BREAKING CHANGE: Split `Dhall.YAML` into `Dhall.YAML` + `Dhall.YAMLToDhall`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/993
* BUG FIX: Fix `dhall-to-{json,yaml}`'s support for preserving alternative
  names
    * The `Nested`/`Inline` unions are now correctly given special treatment
      again
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1080
* Feature: Support weakly-typed JSON value added to Prelude
    * You can now encode/decode values of type `./Prelude/JSON/Type` which
      can store arbitrary JSON
    * This is useful when dealing with "pass-through" or schema-free JSON
      values
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1007
* Feature: Eta support for `dhall-json`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1013
* Feature: Add `--file` option to `dhall-json` executables
* Feature: Support unions for keys
    * You can now decode record fields as enums instead of `Text` so that you
      can pattern match on them
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1094
* Pretty-print output of `{json,yaml}-to-dhall`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1150

1.3.0

* BREAKING CHANGE: Change YAML/JSON encoding for `NaN`/`Infinity`/`-Infinity`
    * They are now encoded as the standard `"nan"`/`"inf"`/`"-inf"`
      representations instead of `null`/`MIN_DOUBLE/`/`MAX_DOUBLE`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/946
* BREAKING CHANGE: Isolate YAML code to one modulee
    * This is a breaking change because it moves `Dhall.JSON.jsonToYaml` to
      `Dhall.YAML.jsonToYaml`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/989
* New `yaml-to-dhall` command-line utility
    * See: https://github.com/dhall-lang/dhall-haskell/pull/977
* Add `--quoted` flag to force quoting of YAML string literals
    * See: https://github.com/dhall-lang/dhall-haskell/pull/941

1.2.8

* New `json-to-dhall` command-line utility
    * See: https://github.com/dhall-lang/dhall-haskell/pull/884
* `--omitEmpty` now also omits empty arrays
    * See: https://github.com/dhall-lang/dhall-haskell/pull/872
* Build against `dhall-1.22.0`
* Improved error messages:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/895
    * See: https://github.com/dhall-lang/dhall-haskell/pull/900

1.2.7

* Build against `dhall-1.21.0`
* Support GHC 7.10.3
    * See: https://github.com/dhall-lang/dhall-haskell/pull/814
* Add new `--omitEmpty` flag for omitting nulls and empty records

1.2.6

* Add `--version` flag
    * See: https://github.com/dhall-lang/dhall-haskell/pull/704
* Build against `tasty-1.2`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/731
* Add `--compact` flag to `dhall-to-json`
    * Eventually `--pretty` will become the default, so this flag provides a
      way for a user to preserve the old 1-line output when that happens
    * See: https://github.com/dhall-lang/dhall-haskell/pull/743

1.2.5

* Build against `dhall-1.19.0`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/667
    * See: https://github.com/dhall-lang/dhall-haskell/pull/675
    * See: https://github.com/dhall-lang/dhall-haskell/pull/689

1.2.4

* Build against `dhall-1.18.0`
    * See: https://github.com/dhall-lang/dhall-json/pull/61
    * See: https://github.com/dhall-lang/dhall-json/pull/63
    * See: https://github.com/dhall-lang/dhall-json/pull/67
* New `dhall-to-yaml` `--documents` flag for generating split documents
    * See: https://github.com/dhall-lang/dhall-json/pull/59
* Build against `yaml-0.10.2.0`
    * This improves the multi-line string literals in generated YAML
    * See: https://github.com/dhall-lang/dhall-json/pull/57

1.2.3

* Correctly handle nested association lists
* Increase upper bound on `dhall` dependency
* Increase upper bound on `yaml` dependency

1.2.2

* Increase upper bound on `dhall` dependency
* Increase upper bound on `yaml` dependency

1.2.1

* Add support for preserving sum type tags
* Sort keys when pretty-printing JSON
* Increase upper bound on `dhall` dependency

1.2.0

* BREAKING CHANGE: Add support for converting association lists to JSON records
    * This changes the behavior for records with two keys named `mapKey` and
      `mapValue`
* Fix `--help` to not print `ExitSuccess`
* Fix lower bound on `dhall` dependency
* Remove `optparse-generic` dependency
* Increase upper bound on `dhall` dependency

1.1.0

* BREAKING CHANGE: Build against `dhall-1.13.0`
    * This requires a breaking change to the type of `codeToValue`

1.0.13

* Add `--omitNull` flag to omit null-valued fields from generated JSON records
* Add `codeToValue`
* Increase upper bound on `dhall` dependency

1.0.12

* Increase upper bound on `aeson` dependency
* Increase upper bound on `dhall` dependency

1.0.11

* Increase upper bound on `dhall` dependency
* Increase upper bound on `optparse-generic` dependency

1.0.10

* Increase upper bound on `dhall` dependency

1.0.9

* Add trailing newline to JSON output
* Remove dependency on `QuasiQuotes`

1.0.8

* Increase upper bound on `dhall` dependency

1.0.7

* Add `--pretty` flag
* Increase upper bound on `dhall` dependency

1.0.6

* Increase upper bound on `aeson` dependency
* Increase upper bound on `dhall` dependency

1.0.5

* Increase upper bound on `optparse-generic` dependency
* Increase upper bound on `trifecta` dependency

1.0.4

* Increase upper bound on `dhall` dependency

1.0.3

* Increase upper bound on `dhall` dependency

1.0.2

* Add support for translating unions by stripping the tag
* Increase upper bound on `dhall` dependency

1.0.1

* Increase upper bound on `dhall` dependency

1.0.0

* Initial release
