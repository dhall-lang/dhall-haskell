1.39.0

* [Supports version 20.2.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v20.2.0)
    * [Add support for Unix shebangs](https://github.com/dhall-lang/dhall-haskell/pull/2175)
* [BREAKING CHANGE TO THE API: `dhall {format,freeze,lint}` now accept multiple
  files](https://github.com/dhall-lang/dhall-haskell/pull/2169)
    * The `--inplace` flag is no longer necessary and you can now specify
      multiple files to update in place on the command line, like
      `dhall format foo.dhall bar.dhall`
    * The `--inplace` flag is still accepted, but does nothing, and will emit a
      warning
    * This is a breaking change to the API for formatting/freezing/linting files
      because now you can specify multiple inputs instead of one input
* [BREAKING CHANGE: Pre-6.0.0 hashes are no longer supported](https://github.com/dhall-lang/dhall-haskell/pull/2190)
    * The interpreter no longer provides backwards compatibility for integrity
      checks computed before standard version 6.0.0
    * This is a breaking change to the API of the `Dhall.Binary` module, where
      certain utilities are no longer parameterized on a `StandardVersion`
    * This is also a breaking change to any Dhall code that depended on these
      really old integrity checks
* [BUG FIX: Formatting `≡` now correctly preserves the original character set](https://github.com/dhall-lang/dhall-haskell/pull/2176)
* [BUG FIX: Don't panic on `Text/replace ""`](https://github.com/dhall-lang/dhall-haskell/pull/2184)
* [Quasiquotation support for Dhall](https://github.com/dhall-lang/dhall-haskell/pull/2198)
    * You can now convert a Dhall expression to the corresponding syntax tree
      using a quasiquoter like this: `[dhall| \x -> x + 2 ]`
* [New `Dhall.Marshal.{Encode,Decode}` modules](https://github.com/dhall-lang/dhall-haskell/pull/2193)
    * These modules split up the `Dhall` module into two smaller modules for
      encoding and decoding logic, respectively
    * The `Dhall` module still re-exports the same functionality as before, so
      this is not a breaking change
* [Support GHC 9.0.1](https://github.com/dhall-lang/dhall-haskell/pull/2154)
* Fixes and improvements to code formatting
    * [Improve pretty-printing of `sha256`](https://github.com/dhall-lang/dhall-haskell/pull/2189)

1.38.1

* [Add `INLINABLE` annotations in more places](https://github.com/dhall-lang/dhall-haskell/pull/2164)
    * This may improve performance by enabling more specializations
* [Fix `hashable`-related test failures](https://github.com/dhall-lang/dhall-haskell/pull/2152)
* [Fix support for GHC 8.4.4](https://github.com/dhall-lang/dhall-haskell/pull/2143)
    * … by using `GeneralizedNewtypeDeriving` (with a `z`)
* [Allow doctest-0.18](https://github.com/dhall-lang/dhall-haskell/pull/2148)
* [Allow bytestring-0.11](https://github.com/dhall-lang/dhall-haskell/pull/2144)

1.38.0

* [BREAKING CHANGE: Detect preferred character set from input](https://github.com/dhall-lang/dhall-haskell/pull/2108)
    * `dhall format` will now preserve the character set of the formatted file
      by default.  In other words, if the file uses ASCII punctuation then
      `dhall format` will format the file using ASCII punctuation.
    * If the file contains both ASCII and Unicode punctuation it will prefer
      Unicode by default
    * This is a breaking change because the `Lam` / `Pi` / `Combine` /
      `CombineTypes`, and `Prefer` constructors now take an additional argument
      to record which character set was used
* [BUG FIX: Fix CORS compliance check](https://github.com/dhall-lang/dhall-haskell/pull/2121)
    * Previous versions were not correctly enforcing CORS compliance
    * This implies that some imports that would have worked previously by
      accident will now fail; specifically: an import from one domain
      transitively importing something from another domain that has not opted
      into CORS
* [Add `ToDhall (Fix f)` instance](https://github.com/dhall-lang/dhall-haskell/pull/2122)
* Fixes and improvements to error messages
    * [#2130](https://github.com/dhall-lang/dhall-haskell/pull/2130)

1.37.1

* [Fix performance regression for `with` expressions](https://github.com/dhall-lang/dhall-haskell/pull/2112)

1.37.0

* [Supports version 20.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v20.0.0)
    * [Implement revised `Text/replace` β-normalization](https://github.com/dhall-lang/dhall-haskell/pull/2072)
    * [Allow `Text/replace ""` to support an abstract haystack](https://github.com/dhall-lang/dhall-haskell/pull/2084)
    * [Support `if` expressions returning a type or kind](https://github.com/dhall-lang/dhall-haskell/pull/2080)
* BREAKING CHANGE TO THE API: [Improve error message for duplicate projection label](https://github.com/dhall-lang/dhall-haskell/pull/2097)
    * This also makes the implementation more standards-compliant, by treating
      a duplicate label as a type error instead of a parse error
    * This is a breaking change since the `Project` constructor now stores a
      `[Text]` instead of `Set Text`
* [Add `--cache` flag to `dhall hash`](https://github.com/dhall-lang/dhall-haskell/pull/2093)
    * This flag adds the hashed expression to the cache when enabled
* [Deprecate `Inject` / `Interpret`](https://github.com/dhall-lang/dhall-haskell/pull/2099)
    * You should instead use `ToDhall` / `FromDhall`, respectively
* Fixes and improvements to the haddocks:
    * [#2098](https://github.com/dhall-lang/dhall-haskell/pull/2098)
    * [#2100](https://github.com/dhall-lang/dhall-haskell/pull/2100)
* Fixes and improvements to error messages:
    * [#2082](https://github.com/dhall-lang/dhall-haskell/pull/2082)
    * [#2095](https://github.com/dhall-lang/dhall-haskell/pull/2095)
* Fixes and improvements to the parser:
    * [#2083](https://github.com/dhall-lang/dhall-haskell/pull/2089)
* Fixes and improvements to the pretty printer:
    * [#2083](https://github.com/dhall-lang/dhall-haskell/pull/2083)
    * [#2101](https://github.com/dhall-lang/dhall-haskell/pull/2101)

1.36.0

* [Supports version 19.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v19.0.0)
    * BREAKING CHANGE TO THE API: [Add `Text/replace` built-in](https://github.com/dhall-lang/dhall-haskell/pull/2063)
    * [Implement `with` without syntactic sugar](https://github.com/dhall-lang/dhall-haskell/pull/2055)
* [`dhall lint` will now add a `.dhall` extension to all Prelude imports](https://github.com/dhall-lang/dhall-haskell/pull/2061)
    * The old extension-free Prelude imports are deprecated
* [Fix command-line completions for files](https://github.com/dhall-lang/dhall-haskell/pull/2016)
* [Improve Template Haskell support for record constructors](https://github.com/dhall-lang/dhall-haskell/pull/2070)
* Fixes and improvements to code formatting
    * [#2037](https://github.com/dhall-lang/dhall-haskell/pull/2037)
    * [#2048](https://github.com/dhall-lang/dhall-haskell/pull/2048)
    * [#2069](https://github.com/dhall-lang/dhall-haskell/pull/2069)

1.35.0

* [Supports version 18.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v18.0.0)
    * [Implement more efficient `with` desugaring](https://github.com/dhall-lang/dhall-haskell/pull/1993)
        * Chained `with` expressions will now be much more efficient
* [BREAKING CHANGE TO THE API: Preserve whitespace for `Lam` constructor](https://github.com/dhall-lang/dhall-haskell/pull/1980)
    * This change extends the `Lam` constructor to preserve whitespace around
      the variable binding
    * The motivation for this change is to enable `dhall-docs` to support
      jumping to definitions
    * You can replace your existing `Lam` constructors with
      `Dhall.Core.makeFunctionBinding`
* [BREAKING CHANGE TO THE API: Preserve whitespace for `Field` constructors](https://github.com/dhall-lang/dhall-haskell/pull/1991)
    * This change extends the `Field` constructor to preserve whitespace around
      the selected field
    * The motivation for this change is to enable `dhall-docs` to support
      jumping to definitions
    * You can use `Dhall.Core.makeFieldSelection` and
      `Dhall.Core.fieldSelectionLabel` to convert between the detailed and the
      simple representation of the selected field.
* [Add `FromDhall` instances for `{Int,Word}{,8,16,32,64}`](https://github.com/dhall-lang/dhall-haskell/pull/2012)
* [Add `--output` option for `dhall text` subcommand](https://github.com/dhall-lang/dhall-haskell/pull/1974)
* [Add `Dhall.Crypto.toString`](https://github.com/dhall-lang/dhall-haskell/pull/1976)
* [Make the HTTP `Manager` configurable](https://github.com/dhall-lang/dhall-haskell/pull/2027)
    * Several import-related functions now provide an alternative variants that
      allows the user to supply a custom `Manager`
    * You can use this to tweak HTTP request timeouts or use a different TLS
      manager (e.g. one from `http-client-openssl`)
* Fixes and improvements to code formatting
    * [#2000](https://github.com/dhall-lang/dhall-haskell/pull/2000)
    * [#2021](https://github.com/dhall-lang/dhall-haskell/pull/2021)

      The formatter now preserves comments for record fields (both record types
      and record literals)
* Fixes and improvements to documentation
    * [#2011](https://github.com/dhall-lang/dhall-haskell/pull/2011)
    * [#2013](https://github.com/dhall-lang/dhall-haskell/pull/2013)
    * [#2014](https://github.com/dhall-lang/dhall-haskell/pull/2014)
* Fixes and improvements to test suite
    * [#2020](https://github.com/dhall-lang/dhall-haskell/pull/2020)

1.34.0

* [Supports version 17.1.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v17.1.0)
    * [Add support for trailing commas](https://github.com/dhall-lang/dhall-haskell/pull/1885)
* BREAKING CHANGE to the API: [Support prefix comments on record key-value pairs](https://github.com/dhall-lang/dhall-haskell/pull/1908)
    * The Dhall AST (i.e. `Expr`) now preserves some comments for record types
      and record literals
    * The impact of this change is that you will need to add
      `Dhall.Syntax.makeRecordField` or `Dhall.Syntax.recordFieldValue` in a few
      places wherever your Haskell assembles or disassembles record expressions
    * The motivation of this change is two-fold:
        * To eventually enable `dhall-docs` support for rendering record
          comments as documentation
        * To eventually enable support for preserving record-related comments
          when formatting Dhall code
* BUG FIX: [Fix `with` expressions to permit functions on their left-hand side](https://github.com/dhall-lang/dhall-haskell/pull/1897)
    * This was a case of the Haskell implementation not being compliant with the
      standard grammar
* [Drop support for GHC 8.2](https://github.com/dhall-lang/dhall-haskell/pull/1949)
* [Add a new `dhall rewrite-with-schemas` command](https://github.com/dhall-lang/dhall-haskell/pull/1902)
    * You can now simplify a Dhall expression using a schema record (e.g. a
      `./schemas.dhall` record that a package might provide)
    * This simplification replaces large anonymous records with an
      equivalent use of a record completion when possible
* [Add `--transitive` flag to `dhall {format,lint,freeze}](https://github.com/dhall-lang/dhall-haskell/pull/1880)
    * This flag lets you format/lint/freeze a file and all of its transitive
      dependencies that are reachable via relative file imports
* [Move `man/dhall.1` to `data-files`](https://github.com/dhall-lang/dhall-haskell/pull/1921)
    * This ensures that Cabal will install `dhall`'s `man` pages in the
      correct directory
* Performance improvements
    * [#1879](https://github.com/dhall-lang/dhall-haskell/pull/1879)
* Standards compliance
    * [#1953](https://github.com/dhall-lang/dhall-haskell/pull/1953)
    * [#1954](https://github.com/dhall-lang/dhall-haskell/pull/1954)
    * [#1956](https://github.com/dhall-lang/dhall-haskell/pull/1956)
    * [#1957](https://github.com/dhall-lang/dhall-haskell/pull/1957)
    * [#1958](https://github.com/dhall-lang/dhall-haskell/pull/1958)
* Fixes and improvements to haddocks
    * [#1881](https://github.com/dhall-lang/dhall-haskell/pull/1881)
    * [#1955](https://github.com/dhall-lang/dhall-haskell/pull/1955)

1.33.1

* [Multi-line REPL / support `repline-0.4.0.0`](https://github.com/dhall-lang/dhall-haskell/pull/1867)
    * `dhall repl` supports a new `:paste` command that lets you input a command
       by pasting one or more lines

1.33.0

* [Supports version 17.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v17.0.0)
    * BREAKING CHANGE: [URLs no longer support quoted path components](https://github.com/dhall-lang/dhall-haskell/pull/1812)
    * BREAKING CHANGE: [`Optional/{fold,build}` are no longer built-ins](https://github.com/dhall-lang/dhall-haskell/pull/1802)
    * [Record fields now permit empty labels](https://github.com/dhall-lang/dhall-haskell/pull/1812)
* BREAKING CHANGE: [Fail instead of hanging when deriving `FromDhall` for recursive types](https://github.com/dhall-lang/dhall-haskell/pull/1825)
    * This is a breaking change as now the `expected` type returns an
      `Expector (Expr Src Void)` (essentially an `Either`) instead of
      `Expr Src Void`
    * If you really don't want to handle the new error-related wrapper, you can
      get the old behavior using a partial pattern match (which will be partial,
      still an improvement over the previous behavior, which was hanging)
* [Fix invalid cache entries](https://github.com/dhall-lang/dhall-haskell/pull/1793)
    * The interpreter will now correct cached expressions that are incorrect
      and warn you when this happens
    * Specifically, if there is a hash mismatch from the cached expression the
      interpreter will resolve the import again and fix the cache if the
      resolved import matches the expected hash
* [Make `encodeExpression` polymorphic](https://github.com/dhall-lang/dhall-haskell/pull/1789)
    * `encodeExpression` now has a more general type, which means that you
      can use it to serialise expressions without imports (i.e.
      ones of type `Expr Void Void`)
* [Add `--quiet` option for `dhall decode`](https://github.com/dhall-lang/dhall-haskell/pull/1803)
* [Add `--noted` flag for `dhall haskell-syntax-tree`](https://github.com/dhall-lang/dhall-haskell/pull/1843)
* Performance improvements:
    * There were several performance improvements related to binary decoding,
      which should improve cache lookup speed
    * [#1807](https://github.com/dhall-lang/dhall-haskell/pull/1807)
    * [#1809](https://github.com/dhall-lang/dhall-haskell/pull/1809)
    * [#1857](https://github.com/dhall-lang/dhall-haskell/pull/1857)
* Improvements to error messages
    * [#1824](https://github.com/dhall-lang/dhall-haskell/pull/1824)
    * [#1849](https://github.com/dhall-lang/dhall-haskell/pull/1849)
    * [#1851](https://github.com/dhall-lang/dhall-haskell/pull/1851)
* Fixes to haddocks
    * [#1815](https://github.com/dhall-lang/dhall-haskell/pull/1815)

1.32.0

* [Supports version 16.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v16.0.0)
    * BREAKING CHANGE: Change the precedence of `with` and `===`
        * The precedence change to `with` means that some old expressions that
          were valid now require explicit parentheses
    * BREAKING CHANGE: Use RFC7049bis encoding for `Double`s
        * This is a breaking change because the hashes of expressions with small
          `Double` literals will change now
    * Add support for unions mixing terms and types
        * For example, `< A : Bool | B : Type >` is legal now
        * You can now write `someRecord with a.b.c = x` to update a nested
          fields
* DEPRECATION: [Deprecate `Dhall.Parser.exprA`](https://github.com/dhall-lang/dhall-haskell/pull/1740)
    * `Dhall.Parser` module will eventually drop support for parsing custom
      import types
    * This is necessary in order to fix several parsing bugs and improve
      parsing error messages
* BUG FIX: [GHC Generics instance for `:+:` now uses `union`](https://github.com/dhall-lang/dhall-haskell/pull/1725)
    * This fixes a few subtle bugs in how Dhall unions are marshalled into
      Haskell types, and also improves the error messages
* Formatting improvements
    * [Change formatting of `if` expressions](https://github.com/dhall-lang/dhall-haskell/pull/1767)
    * [Change formatting for functions and their types](https://github.com/dhall-lang/dhall-haskell/pull/1759)
    * [Prefer puns when formatting record completions](https://github.com/dhall-lang/dhall-haskell/pull/1736)
* [Convert union alternatives to directory tree](https://github.com/dhall-lang/dhall-haskell/pull/1757)
    * `dhall to-directory-tree` now supports unions which are automatically
      unwrapped
* [Fix `dhall freeze --cache` to better handle protected imports](https://github.com/dhall-lang/dhall-haskell/pull/1772)
    * `dhall freeze --cache` will now also update imports that already have
      integrity checks
* [Don't normalized partially saturated `{List,Natural}/fold`](https://github.com/dhall-lang/dhall-haskell/pull/1742)
    * The behavior now matches the standard.  Previously, the Haskell
      implementation was not standards-compliant because it would normalize
      these partially saturated built-ins

1.31.1

* BUG FIX: [Allow whitespace after record pun entry](https://github.com/dhall-lang/dhall-haskell/pull/1733)
    * The record pun feature introduced in the previous release did not
      correctly parse record puns with trailing whitespace, which this change
      fixes.
* [Expose `{default,}InputNormalizer`](https://github.com/dhall-lang/dhall-haskell/pull/1727)
    * The previous version introduced a breaking change to the `autoWith` type
      that required access to the implementation of `InputNormalizer`, which was
      not exported.  This change fixes that.
* Build against latest dependencies
    * [`QuickCheck-2.14`](https://github.com/dhall-lang/dhall-haskell/pull/1721)
    * [`haskell-lsp-0.21`](https://github.com/dhall-lang/dhall-haskell/pull/1730)
    * [`repline-0.3` / `haskeline-0.8`](https://github.com/dhall-lang/dhall-haskell/pull/1717)
    * [`template-haskell-2.16`](https://github.com/dhall-lang/dhall-haskell/pull/1719)
* [Prefer to format using record puns when possible](https://github.com/dhall-lang/dhall-haskell/pull/1729)
    * `dhall format` will now reformat code to use record puns when applicable
* Fixes and improvements to error messages:
    * [#1721](https://github.com/dhall-lang/dhall-haskell/pull/1724)

1.31.0

* [Supports version 15.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v15.0.0)
    * [Implement `with` keyword](https://github.com/dhall-lang/dhall-haskell/pull/1685)
        * You can now write `someRecord with a.b.c = x` to update a nested
          fields
    * [Add support for record puns](https://github.com/dhall-lang/dhall-haskell/pull/1710)
        * You can now write `{ x, y }` as a shorthand for `{ x = x, y = y }`
* BREAKING CHANGE TO THE API: [Auto-derive `Generic`/`FromDhall`/`ToDhall` with Template Haskell](https://github.com/dhall-lang/dhall-haskell/pull/1682)
    * Now the `Dhall.TH.makeHaskell*` utilities will include these derived
      instances in the generated declarations
    * This is a breaking change since users were likely already generating these
      instances separately, which will now conflict with the included instances
* BREAKING CHANGE TO THE API: [`From/ToDhall` no longer takes `InterpretOptions` argument](https://github.com/dhall-lang/dhall-haskell/pull/1696)
    * The types of the `autoWith` and `injectWith` methods have changed to
      take an `InputNormalizer` instead of an `InterpretOptions`
        * Note that `InputNormalizer` is a subset of `InterpretOptions`
    * This is a breaking change to how derived `FromDhall` / `ToDhall` instances
      are customized to more closely match how other Haskell packages customize
      derived instances (e.g. `aeson` with `FromJSON` / `ToJSON`)
        * Previously you would customize the behavior globally by passing in
          a top-level `InterpretOptions` record to `autoWith`
        * Now you can customize the behavior locally on a per-instance basis
    * This change enables the following change ...
* [Add `Dhall.Deriving` module for `deriving-via` helpers](https://github.com/dhall-lang/dhall-haskell/pull/1700)
    * Now you can take advantage of the `-XDerivingVia` language extension to
      customize derived `FromDhall`/`ToDhall` instances, like this:
        * `deriving (FromDhall, ToDhall) via Codec (SetSingletonConstructors Bare) Name`
* BREAKING CHANGE TO THE LANGUAGE: [Match standard with respect to `using toMap`](https://github.com/dhall-lang/dhall-haskell/pull/1673)
    * `https://example.com using toMap customHeaders` is now a parse error
      and needs to be explicitly parenthesized as
      `https://example.com using (toMap customHeaders)`
    * The language standard had always required the parentheses, but the Haskell
      implementation was not correctly matching the standard
* [Fix formatting of indented comments containing empty lines](https://github.com/dhall-lang/dhall-haskell/pull/1688)
    * `dhall format` was previously not idempotent when formatting indented
      comments with empty lines
    * Specifically, the formatter kept indenting things further with each
      format, which this change fixes
* [Fix pretty-printer to preserve original numeric literals](https://github.com/dhall-lang/dhall-haskell/pull/1674)
    * Now `dhall format` will preserve numeric literals exactly how you wrote
      them
    * For example, `0xFF` will no longer be reformatted as `255`
* [Add `dhall to-directory-tree` support for `Map`s](https://github.com/dhall-lang/dhall-haskell/pull/1705)
    * `Map`s are now converted to directories (just like records)
* [Add manpage](https://github.com/dhall-lang/dhall-haskell/pull/1677)
    * ... mainly for the benefit of people packaging Dhall for various
      distributions
* [Group commands in CLI](https://github.com/dhall-lang/dhall-haskell/pull/1692)
    * The command-line `--help` output now groups commands into useful
      sections
* [Fix numeric parsing for GHCJS](https://github.com/dhall-lang/dhall-haskell/pull/1681)
    * The GHCJS backend for Dhall was failing to parse numbers, which this
      change fixes
* Fixes and improvements to error messages:
    * [#1656](https://github.com/dhall-lang/dhall-haskell/pull/1656)
    * [#1698](https://github.com/dhall-lang/dhall-haskell/pull/1698)
    * [#1702](https://github.com/dhall-lang/dhall-haskell/pull/1702)
* Fixes and improvements to the haddocks:
    * [#1708](https://github.com/dhall-lang/dhall-haskell/pull/1708)
    * [#1712](https://github.com/dhall-lang/dhall-haskell/pull/1712)

1.30.0

* [Supports version 14.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v14.0.0)
* BREAKING CHANGE TO THE API: [Add `--check` flag to `dhall {lint,freeze}`](https://github.com/dhall-lang/dhall-haskell/pull/1636)
    * You can now use the `--check` flag to verify that a file has already been
      linted or frozen
    * This is a breaking change to the types used by the `Dhall.Format` module
* BREAKING CHANGE TO THE LANGUAGE: [Disallow `Natural` literals with leading zeros](https://github.com/dhall-lang/dhall-haskell/pull/1658)
    * Now a literal like `042` is no longer valid
    * See the [changelog for standard version 14.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v14.0.0) for more details
* BUG FIX: [Fix parsing of `Double` literal trailing whitespace](https://github.com/dhall-lang/dhall-haskell/pull/1647)
    * Certain expressions using `Double` literals would fail to parse, which this
      change fixes
* BUG FIX: [Use `DeriveLift` instead of GHC Generics to derive `Lift` ](https://github.com/dhall-lang/dhall-haskell/pull/1640)
    * This fixes a build failure on GHC 8.10
* [Drop support for GHC 7.10.3](https://github.com/dhall-lang/dhall-haskell/pull/1649)
    * GHC 8.0.2 is now the earliest supported version
* [Add support for dotted field syntax](https://github.com/dhall-lang/dhall-haskell/pull/1651)
    * `{ x.y.z = 1 }` now legal syntax for nested fields
    * See the [changelog for standard version 14.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v14.0.0) for more details
* [Add support for duplicate record fields](https://github.com/dhall-lang/dhall-haskell/pull/1643)
    * This combines with the previous feature to let you write
      `{ x.y = 1, x.z = True }`, which is equivalent to
      `{ x = { y = 1, z = True } }`
    * See the [changelog for standard version 14.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v14.0.0) for more details
* [Add `dhall lint` support for deprecating `Optional/{fold,build}`](https://github.com/dhall-lang/dhall-haskell/pull/1628)
    * The `Optional/{fold,build}` built-ins are deprecated and can be implemented
      in terms of other language features
    * `Optional/fold` can be implemented in terms of `merge` (which now works on
      `Optional` values)
    * `Optional/build` could always be implemented using `Some`/`None`
    * `dhall lint` now transforms the deprecated built-ins to use their
      equivalent built-in-free versions
* [Support Template Haskell for multiple datatypes](https://github.com/dhall-lang/dhall-haskell/pull/1664)
    * This extends the Template Haskell support added in the previous release to
      work for datatypes that refer to one another
* [Add support for custom substitutions](https://github.com/dhall-lang/dhall-haskell/pull/1650)
    * You can now add custom substitutions, which are like `let` bindings that
      propagate to transitive imports
* [Small formatting fixes](https://github.com/dhall-lang/dhall-haskell/pull/1652)

1.29.0

* [Supports version 13.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v13.0.0)
* BREAKING CHANGE: [Generate Haskell datatype declarations from Dhall types](https://github.com/dhall-lang/dhall-haskell/commit/b0280826790930d18a5498fb802120478fa11767#diff-a9729dccf50be61ce3d8c68c16f0fd50)
    * You can now use the `makeHaskellTypeFromUnion` Template Haskell utility
      to generate a Haskell datatype declaration from a Dhall union type
    * This helps ensure that your Haskell types and Dhall types stay in sync,
      when you want the Dhall type to be the source of truth
    * This is a breaking change because the default `InterpretOptions` changed
      the default handling of singleton constructors from `Wrapped` to `Smart`
    * You can preserve the old behavior using:
      `autoWith defaultInterpretOptions{ singletonConstructors = Wrapped }`
* BUG FIX: [Fix `dhall freeze --cache` and `dhall lint` to preserve `let`-related comments](https://github.com/dhall-lang/dhall-haskell/pull/1597)
    * Now they match the behavior of `dhall format` with regard to preserving
      these comments
* BUG FIX: [Fix escaping of significant leading whitespace when formatting code](https://github.com/dhall-lang/dhall-haskell/pull/1598)
    * The formatter would sometimes unnecessarily escape significant leading
      whitespace for the final line of multiline string literals, which this
      change fixes
* BUG FIX: [Fix `dhall encode --json` for `Double` values](https://github.com/dhall-lang/dhall-haskell/issues/1350)
* NEW FEATURE: [`dhall to-directory-tree` command](https://github.com/dhall-lang/dhall-haskell/pull/1606)
    * You can now generate a directory tree from a Dhall expression
    * Specifically:
        * records are converted to directories
        * `Text` fields are converted to files named after the field
        * `Optional` values are omitted if `None`
        * Everything else is rejected
* NEW FEATURE: [Hexadecimal literals](https://github.com/dhall-lang/dhall-haskell/pull/1607)
    * See the [changelog for standard version 13.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v13.0.0) for more details
* NEW FEATURE: [`merge` works on `Optional` values](https://github.com/dhall-lang/dhall-haskell/pull/1608)
    * See the [changelog for standard version 13.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v13.0.0) for more details
* [Improve formatter](https://github.com/dhall-lang/dhall-haskell/pull/1609)
    * `dhall format` will now render expressions nested inside record fields or
      alternatives more compactly, including:
        * Records
        * Record completion expressions
        * Expressions wrapped in `Some`
        * Lists
* [Exclude the `using ...` suffix from imports listed by `dhall resolve`](https://github.com/dhall-lang/dhall-haskell/pull/1603)
    * Specifically when using the `--{immediate,transitive}-dependencies` flags

1.28.0

* [Supports version 12.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v12.0.0)
* BREAKING CHANGE: [Add `Integer/{clamp,negate}` built-ins](https://github.com/dhall-lang/dhall-haskell/pull/1486)
    * This is a technically breaking change API since this adds a new
      constructor to the `Expr` type
    * This is also a technically breaking change to the language.  See the [changelog for standard version 12.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v12.0.0) for more details
* BREAKING CHANGE: [Remove support for fusion](https://github.com/dhall-lang/dhall-haskell/pull/1478)
    * This is also a technically breaking change to the language.  See the [changelog for standard version 12.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v12.0.0) for more details
* BREAKING CHANGE: [Parse whitespace more precisely](https://github.com/dhall-lang/dhall-haskell/pull/1483)
    * The Haskell implementation now matches the official grammar much more
      closely, but as a result will now reject some programs that it used to
      accept
    * For example, `1:Natural` used to be valid and now is no longer valid as
      the standard requires mandatory whitespace after the `:`
    * Consult the [standard grammar](https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf) if you run into a new parsing error as a result of this change
    * This is also a parsing performance regression (specifically for parsing
      comments), but should not be noticeable in practice.  See [#1512](https://github.com/dhall-lang/dhall-haskell/pull/1512) for more details
* BREAKING CHANGE: Rename `Type` to `Decoder` and `InputType` to `Encoder` [#1483](https://github.com/dhall-lang/dhall-haskell/pull/1485) / [#1489](https://github.com/dhall-lang/dhall-haskell/pull/1489)
* BUG FIX: [Fix `dhall format --check`](https://github.com/dhall-lang/dhall-haskell/pull/1462)
    * Before this change `dhall format --check` would fail due to attempting to
      read all of standard input in twice
* BUG FIX: [Fix `dhall freeze` to always re-freeze an import](https://github.com/dhall-lang/dhall-haskell/pull/1471)
    * Before this fix, `dhall freeze` would not attempt to refreeze an already
      frozen import
* [Permit spaces around completion operator](https://github.com/dhall-lang/dhall-haskell/pull/1532)
    * See the [changelog for standard version 12.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v12.0.0) for more details
* [Make `missing` referentially transparent](https://github.com/dhall-lang/dhall-haskell/pull/1509)
    * `missing` can now be imported transitively via a remote import
    * Normally resolving `missing` would still still fail, except for
      `missing as Location`, which is now a valid transitive import
    * See the [changelog for standard version 12.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v12.0.0) for more details
* [Write cache files atomically](https://github.com/dhall-lang/dhall-haskell/pull/1544)
    * This is a resilience improvement so that the cache is not left in a
      corrupt state in the event of a power outage or exhausting disk/memory
* [New `Dhall.function` utility](https://github.com/dhall-lang/dhall-haskell/pull/1507)
    * This is provides the same functionality as the `Interpret` instance for
      `(->)`, except without the use of typeclasses
* [New `dhall haskell-syntax-tree` command](https://github.com/dhall-lang/dhall-haskell/pull/1553)
    * This command displays the Haskell syntax tree of an expression
      (primarily for debugging purposes)
    * Note that this is highly-volatile and subject to change, so don't depend
      on this programmatically.  We may break the output of this command without
      any notice.
* [Add `instance Show Ann`](https://github.com/dhall-lang/dhall-haskell/pull/1567)
* [Move normalization code from `Dhall.Core` to `Dhall.Normalize`](https://github.com/dhall-lang/dhall-haskell/pull/1452)
    * Note that this is not a breaking change.  The relocated utilities are
      still re-exported from `Dhall.Core`
* [Fix `dhall resolve --transitive-dependencies` to list dependencies in "post-order"](https://github.com/dhall-lang/dhall-haskell/pull/1539)
* Performance improvements
    * [#1500](https://github.com/dhall-lang/dhall-haskell/pull/1500)
    * [#1522](https://github.com/dhall-lang/dhall-haskell/pull/1522)
    * [#1568](https://github.com/dhall-lang/dhall-haskell/pull/1568)
    * [#1580](https://github.com/dhall-lang/dhall-haskell/pull/1578)
* Fixes and improvements to code formatting
    * [#1460](https://github.com/dhall-lang/dhall-haskell/pull/1460)
    * [#1466](https://github.com/dhall-lang/dhall-haskell/pull/1466)
    * [#1508](https://github.com/dhall-lang/dhall-haskell/pull/1508)
    * [#1527](https://github.com/dhall-lang/dhall-haskell/pull/1527)
    * [#1422](https://github.com/dhall-lang/dhall-haskell/pull/1422)
    * [#1552](https://github.com/dhall-lang/dhall-haskell/pull/1552)
    * [#1543](https://github.com/dhall-lang/dhall-haskell/pull/1543)
    * [#1554](https://github.com/dhall-lang/dhall-haskell/pull/1554)
    * [#1569](https://github.com/dhall-lang/dhall-haskell/pull/1569)
* Fixes and improvements to code linting
    * [#1518](https://github.com/dhall-lang/dhall-haskell/pull/1518)
    * [#1531](https://github.com/dhall-lang/dhall-haskell/pull/1531)
* Fixes and improvements to error messages
    * [#1443](https://github.com/dhall-lang/dhall-haskell/pull/1443)
    * [#1448](https://github.com/dhall-lang/dhall-haskell/pull/1448)
    * [#1482](https://github.com/dhall-lang/dhall-haskell/pull/1482)
    * [#1519](https://github.com/dhall-lang/dhall-haskell/pull/1519)
    * [#1556](https://github.com/dhall-lang/dhall-haskell/pull/1556)
    * [#1528](https://github.com/dhall-lang/dhall-haskell/pull/1528)
* Fixes and improvements to the parser
    * [#1473](https://github.com/dhall-lang/dhall-haskell/pull/1473)
    * [#1549](https://github.com/dhall-lang/dhall-haskell/pull/1549)
    * [#1563](https://github.com/dhall-lang/dhall-haskell/pull/1563)
    * [#1584](https://github.com/dhall-lang/dhall-haskell/pull/1584)
* Fixes and improvements to diffs
    * [#1585](https://github.com/dhall-lang/dhall-haskell/pull/1585)
* Fixes and improvements to the REPL
    * [#1573](https://github.com/dhall-lang/dhall-haskell/pull/1573)
* Fixes and improvements to documentation
    * [#1530](https://github.com/dhall-lang/dhall-haskell/pull/1530)

1.27.0

* [Supports version 11.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v11.0.0)
* BREAKING CHANGE: Rename `Inject`/`Interpret` to `ToDhall`/`FromDhall`
    * This change `ConstraintKinds` to minimize disruption by keeping around
      `Inject`/`Interpret` as synonyms for `ToDhall`/`FromDhall`
    * In other words, constraints and derived instances using `Inject` or
      `Interpret` will still work
    * However, manual instances using `Inject` or `Interpret` won't work
      unless you rename them to `ToDhall`/`FromDhall` or enable the
      `TypeSynonymInstances` extension
* BREAKING CHANGE: Fix `Eq` instance for `Expr`s with special `Double`s
    * This fixes the `Eq` instance for `Expr`s to match the standard regarding
      `Double` comparisons
    * Specifically: `NaN == NaN` and `-0.0 /= 0.0`
    * This is a breaking change because the `DoubleLit` constructor of `Expr`
      now stores a `DhallDouble` instead of a `Double`
* BREAKING CHANGE: [Add `--file` option for `dhall hash`](https://github.com/dhall-lang/dhall-haskell/pull/1445)
    * This is a breaking change because it also removes the `Dhall.Hash` module,
      which wasn't really carrying its own weight
* [Add support for leading separators](https://github.com/dhall-lang/dhall-haskell/pull/1355)
    * See the [changelog for standard version 11.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v11.0.0) for more details
* [Add record completion operator](https://github.com/dhall-lang/dhall-haskell/pull/1375)
    * See the [changelog for standard version 11.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v11.0.0) for more details
* [Add `dhall tags` subcommand](https://github.com/dhall-lang/dhall-haskell/pull/1398)
    * This generates an ETags file from an input file or directory
* [Add `dhall lint` support for fixing malformed assertions](https://github.com/dhall-lang/dhall-haskell/pull/1353)
    * `dhall lint` now fixes `let example = foo === bar` to be
       `let example = assert : foo === bar`
* Normalize inferred types [#1337](https://github.com/dhall-lang/dhall-haskell/pull/1337)
* New `FromDhall`/`ToDhall` (previously `Interpret`/`Inject`) instances for:
    * [`Void`](https://github.com/dhall-lang/dhall-haskell/pull/1382)
    * `Map` [#1391](https://github.com/dhall-lang/dhall-haskell/pull/1391) / [#1412](https://github.com/dhall-lang/dhall-haskell/pull/1412)
    * [`HashMap`](https://github.com/dhall-lang/dhall-haskell/pull/1426)
    * [`Set` and `HashSet`](https://github.com/dhall-lang/dhall-haskell/pull/1405)
    * [`()`](https://github.com/dhall-lang/dhall-haskell/pull/1388)
* [Add `--output` option to `dhall`](https://github.com/dhall-lang/dhall-haskell/pull/1399)
* [Move syntax things from `Dhall.Core` to a new `Dhall.Syntax` module](https://github.com/dhall-lang/dhall-haskell/pull/1440)
    * This is not a breaking change.  `Dhall.Core` still re-exports the same
      API as before
* Performance improvements
    * [#1362](https://github.com/dhall-lang/dhall-haskell/pull/1362)
    * [#1366](https://github.com/dhall-lang/dhall-haskell/pull/1366)
    * [#1368](https://github.com/dhall-lang/dhall-haskell/pull/1368)
    * [#1397](https://github.com/dhall-lang/dhall-haskell/pull/1397)
    * [#1401](https://github.com/dhall-lang/dhall-haskell/pull/1401)
* Fixes and improvements to code formatting
    * [#1360](https://github.com/dhall-lang/dhall-haskell/pull/1360)
    * [#1372](https://github.com/dhall-lang/dhall-haskell/pull/1372)
    * [#1380](https://github.com/dhall-lang/dhall-haskell/pull/1380)
    * [#1415](https://github.com/dhall-lang/dhall-haskell/pull/1415)

1.26.1

* TECHNICALLY BREAKING CHANGES: [Simplify `⫽` within projection](https://github.com/dhall-lang/dhall-haskell/pull/1283) / [Simplify nested record projections](https://github.com/dhall-lang/dhall-haskell/pull/1307)
    * These are technically breaking changes because you will need to update
      integrity checks that protect code simplified in this way
    * We're not bumping the major version since the likelihood that you're
      affected is quite low
* BUG FIX: [Fix performance regression](https://github.com/dhall-lang/dhall-haskell/pull/1335)
    * This change fixes the performance regression introduced in the previous
      release (version 1.26.0)
* BUG FIX: [Prevent REPL from inserting inferred `Sort`s into context](https://github.com/dhall-lang/dhall-haskell/pull/1318)
    * This protects the REPL from getting hosed if you define an expression
      using `:let` that has an inferred type of `Sort`
* NEW FEATURE: Improved `Inject`/`Interpret` support for 1-field constructors - ([#1315](https://github.com/dhall-lang/dhall-haskell/pull/1315) / [#1321](https://github.com/dhall-lang/dhall-haskell/pull/1321))
    * This adds a new `singletonConstructors` field to `InterpretOptions` that
      lets you control what Dhall type 1-field Haskell constructors correspond
      to
    * The default (`Wrapped`) is backwards compatible with the old behavior
    * The `Smart` option is probably what you want: it will strip the
      Haskell constructor from the Dhall type if the constructor has one
      anonymous field
    * The `Bare` option always strips 1-field Haskell constructors from the
      Dhall type
* NEW FEATURE: `--censor` flag that disables source code display ([#1312](https://github.com/dhall-lang/dhall-haskell/pull/1312) / [#1329](https://github.com/dhall-lang/dhall-haskell/pull/1329))
    * Use this flag when you don't want sensitive `Text` literals showing up in
      parsing or type-checking error messages
* [Format record fields more compactly if they fit on 1 line](https://github.com/dhall-lang/dhall-haskell/pull/1314)
    * The formatter will now format record fields on a field-by-field basis to
      avoid unnecessary vertical sprawl of formatted records
    * Specifically, record fields that fit on one line will now be formatted
      on one line
* [Add `--quiet` option to `dhall type`](https://github.com/dhall-lang/dhall-haskell/pull/1325)
    * This lets you use `dhall type` in "check only" mode (e.g. for CI for
      for development feedback)
* Improved GHCJS support - ([#1311](https://github.com/dhall-lang/dhall-haskell/pull/1311) / [#1330](https://github.com/dhall-lang/dhall-haskell/pull/1330))
* [Fix all executables to accept `--version`](https://github.com/dhall-lang/dhall-haskell/pull/1334)
    * Note that the `dhall version` subcommand is still supported, too
* [New `Dhall.Version` module](https://github.com/dhall-lang/dhall-haskell/pull/1332)
* [Don't normalize inferred types](https://github.com/dhall-lang/dhall-haskell/pull/1317)
    * This fixes `dhall type` to more accurately follow the standard
* [Initial changes for GHC 8.8 support](https://github.com/dhall-lang/dhall-haskell/pull/1324)
* [Fix Haddock formatting for `dhallFromJSON`](https://github.com/dhall-lang/dhall-haskell/pull/1316)
* [Improved Windows caching support](https://github.com/dhall-lang/dhall-haskell/pull/1272)
    * `dhall` will now prefer the `%LOCALAPPDATA%` directory for caching if it is
      available
* [Warn about missing cache directories](https://github.com/dhall-lang/dhall-haskell/pull/1320)

1.26.0

* [Supports version 10.0.0 of the standard](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0)
* BREAKING CHANGE TO THE LANGUAGE: [Remove old union literal syntax](https://github.com/dhall-lang/dhall-haskell/pull/1176)
    * Union literals of the form `< x = e | ... >` are no longer valid
    * For more details, see: [Migration: Deprecation of old union literal syntax](https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Deprecation-of-old-union-literal-syntax)
    * Also see the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* BREAKING CHANGE TO THE API: [Change `X` to be a type synonym for `Data.Void`](https://github.com/dhall-lang/dhall-haskell/pull/1172)
    * This is a breaking change if you were previously pattern matching on the
      `X` constructor.  You can replace that with the use of `Data.Void.absurd`
* BREAKING CHANGE TO THE API: [Treat multi-`let`s as syntactic sugar](https://github.com/dhall-lang/dhall-haskell/pull/1242)
    * This is a breaking change because the `Let` constructor now only stores
      one `Binding` instead of a `NonEmpty` list of `Binding`s
* PERFORMANCE REGRESSION & TECHNICALLY BREAKING CHANGE TO THE LANGUAGE: [Dependent types](https://github.com/dhall-lang/dhall-haskell/pull/1164)
    * You can now write functions from terms to types
    * There is also now language support for tests of the form
      `assert : x === y`
    * This deteriorates the performance of large multi-`let` expressions
      (See: [#1306](https://github.com/dhall-lang/dhall-haskell/issues/1306))
    * Splitting large multi-`let` expressions into smaller files may mitigate
      the problem as a work-around for now
    * Follow [#1129](https://github.com/dhall-lang/dhall-haskell/issues/1129)
      for work to fix this performance regression
    * This is also a technically breaking change because `assert` is now a
      reserved keyword
    * See the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* TECHNICALLY BREAKING CHANGE TO THE LANGUAGE: [Add `Natural/subtract` built-in](https://github.com/dhall-lang/dhall-haskell/pull/1133)
    * The language now supports machine subtraction, which can be used to
      support several other high-performance operations (like `Natural`
      comparisons)
    * This is a technically breaking change if you used `Natural/subtract` as an
      identifier in your code
    * See the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* TECHNICALLY BREAKING CHANGE TO THE LANGUAGE: [More simplifications for field selection](https://github.com/dhall-lang/dhall-haskell/pull/1174)
    * Now the interpreter will more intelligently simplify certain field
      projections
    * For example: `λ(x : { a : Bool, b : Bool }) → (x ⫽ { c = 0 }).{ a, c }.c`
      will now simplify to `λ(x : { a : Bool, b : Bool }) → 0 `
    * This is a technically breaking change because you will need to update
      integrity checks that protect code simplified in this way
    * See the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* TECHNICALLY BREAKING CHANGE TO THE LANGUAGE: [Simplify `⫽` when its arguments are equivalent](https://github.com/dhall-lang/dhall-haskell/pull/1196)
    * This is a technically breaking change for the same reason: this will
      perturb semantic integrity checks for affected code
    * See the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* NEW FEATURE: [Restore support for records containing both types and terms](https://github.com/dhall-lang/dhall-haskell/pull/1173)
    * In other words `{ foo = 1, bar = Bool }` is now valid again
    * This means that you now can export a single package containing both types
      and terms
    * See the [changelog for standard version 10.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v10.0.0) for more details
* [`dhall format` now preserves `let` comments](https://github.com/dhall-lang/dhall-haskell/pull/1273)
    * `dhall` format will now preserve comments in the following locations of
      a `let` binding:
    * `let {- HERE -} x {- HERE -} : {- HERE -} Bool = {- HERE -} True in x`
    * This support handles both single-line and multi-line comments and also
      takes care of correctly indenting/dedenting them
    * Note that comments before the `let` keyword are still not preserved
      (unless it is the beginning of the file)
* [Add API support for marshalling recursive types](https://github.com/dhall-lang/dhall-haskell/pull/1195)
    * You can now marshal recursive types from Dhall into Haskell using the
      newly-added utilities
    * See also: [#1298](https://github.com/dhall-lang/dhall-haskell/pull/1298)
* [New `:help` command for `dhall repl`](https://github.com/dhall-lang/dhall-haskell/pull/1237)
* New `--no-cache` flag [#1290](https://github.com/dhall-lang/dhall-haskell/pull/1290) / [#1434](https://github.com/dhall-lang/dhall-haskell/pull/1434) / [#1436](https://github.com/dhall-lang/dhall-haskell/pull/1436)
    * You can now disable use of the cache with this flag
    * This comes in handy if you want to disable α-normalization for imports
      protected by a semantic integrity check
* Bug fixes
    * [Fix `isNormalized` for field selections](https://github.com/dhall-lang/dhall-haskell/pull/1210)
    * [Simplify `Natural/subtract` when its arguments are equivalent](https://github.com/dhall-lang/dhall-haskell/pull/1220)
    * [Fix `NaN` to be judgmentally equivalent to itself](https://github.com/dhall-lang/dhall-haskell/pull/1231)
    * [Fix `Inject` instance for lists](https://github.com/dhall-lang/dhall-haskell/pull/1261)
    * [Fix typechecking of `toMap`](https://github.com/dhall-lang/dhall-haskell/pull/1279)
* Performance optimizations
    * [Optimize a few `Set` instances](https://github.com/dhall-lang/dhall-haskell/pull/1184)
    * [Remove some redundant sorting during normalization](https://github.com/dhall-lang/dhall-haskell/pull/1228)
* Improvements to error messages
    * [Improve error reporting for failed remote imports](https://github.com/dhall-lang/dhall-haskell/pull/1188)
    * [Improve HTTP errors](https://github.com/dhall-lang/dhall-haskell/pull/1253)
* Improvements to formatting
    * [Indent function arguments when formatting](https://github.com/dhall-lang/dhall-haskell/pull/1167)
    * [Prefer unquoted URLs](https://github.com/dhall-lang/dhall-haskell/pull/1235)
    * [Strip leading whitespace](https://github.com/dhall-lang/dhall-haskell/pull/1270)
* Improvements to diffs
    * [Fix diffs for lists](https://github.com/dhall-lang/dhall-haskell/pull/1213)
    * [Improve diff for non-empty lists](https://github.com/dhall-lang/dhall-haskell/pull/1244)
    * [Small fixes to `dhall diff`](https://github.com/dhall-lang/dhall-haskell/pull/1263)
* Improvements to documentation
    * [Fix documentation for `UnionInputType`](https://github.com/dhall-lang/dhall-haskell/pull/1230)
    * [Document support for caching protected imports](https://github.com/dhall-lang/dhall-haskell/pull/1247)
* Improvements to command-line interface
    * [Improve description of `dhall lint`](https://github.com/dhall-lang/dhall-haskell/pull/1264)
    * [Change `dhall type` to resolve imports](https://github.com/dhall-lang/dhall-haskell/pull/1281)

1.25.0

* Supports version 9.0.0 of the standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v9.0.0
* BREAKING CHANGE: Remove support for old-style `List`-like `Optional` literals
    * List-like `Optional` Literals (i.e. `[ 1 ] : Optional Natural`) are no
      longer valid
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1002
* BREAKING CHANGE: Add support for semi-semantic caching
    * This change significantly improves the performance of imports
    * This change also automatically caches imports without an integrity check
    * This changes several types in `Dhall.Import` to support this new
      feature
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1113
* BREAKING CHANGE: Implement new Unicode braced escape sequence
    * Escape sequences encoding surrogate pairs are no longer valid
    * Instead, characters previously encoded as surrogate pairs can instead be
      encoded as a braced sequence
    * For example: "\uD834\uDD1E" must now be written as "\u{1D11E}"
    * See: https://github.com/dhall-lang/dhall-haskell/pull/987
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1104
* BREAKING CHANGE: Make the type of extract richer:
    * `Dhall.extract` can now return a detailed error instead of just a `Maybe`
    * This is a breaking chnage because the type of `extract` changed
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1011
* BREAKING CHANGE: Add support for importing expressions `as Location`
    * This is a breaking change because a new `Location` constructor was added
      to `ImportMode`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1019
* BREAKING CHANGE: Switch `Var` to use an `Int`
    * This is a performance improvement, but also a breaking change since the
      `Integer` in the `Var` constructor was changed to an `Int`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1044
* BREAKING CHANGE: Add new `toMap` keyword
    * This is a breaking change to the API because a new `ToMap` constructor
      was added to the `Expr` type
    * This is also a technically breaking change to the language because `toMap`
      is now a reserved keyword, although most code should be unaffected in
      practice
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1041
* BREAKING CHANGE: Sort the fields of a record projection during normalization
    * This is a technically breaking change to the language because any
      expressions with an uninterpreted record projection will have a different
      semantic integrity check.  However, most could should be unaffected in
      practice
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1111
* BUG FIX: Fix substitution into record projection by type
    * An expression like this one was being incorrectly rejected:
      `let e = { a = 10, b = "Text" } let s = { a : Natural } in e.(s)`, which
      this change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1012
* BUG FIX: Reject record projection when there is a field type mismatch
    * Record projection by type was previously not checking the expected
      field types, which this change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1027
* BUG FIX: Fix linting of unused let bindings
    * Certain let bindings were not correctly detected as unused, which this
      change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1001
* BUG FIX: Fix `--file` option
    * The `--file` option from the previous release did not work, due to not
      computing relative paths correctly, which this change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1004
* BUG FIX: Minor fix to `dhall diff`
    * `dhall diff` was incorrectly displaying spurious differences for
      identical lists that were function arguments, which this change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1006
* BUG FIX: Allow `Sort` as type annotation
    * This should have been implemented in the previous release as part of
      supporting version 8.0.0 of the standard, but was missed
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1024
* BUG FIX: `Dhall.Map`: Reflect original key ordering in `Ord` instance
    * `Dhall.Map` now considers key order when comparing `Map`s, which it should
      have done before, but didn't
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1050
* BUG FIX: Consistently format multi-line strings
    * The formatter now formats naked multi-line strings the same as nested
      multi-line strings
    * Specifically, naked multi-line strings can now be formatted on a single
      (just like nested multi-line strings)
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1056
* BUG FIX: Make `isNormalized` consistent with `normalize`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1115
* BUG FIX: Make `normalizeWithM` consistent with `normalize`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1126
* BUG FIX: Fix import alternatives to recover from type errors
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1152
* Feature: Semi-semantic caching
    * The Haskell implementation now implicitly caches *all* local imports, not
      just imports frozen by integrity checks, so that you don't have to freeze
      them when doing local development
    * These cached imports are still correctly invalidated if they or any of
      their dependencies change
    * This new implicit cache is stored underneath `~/.cache/dhall-haskell` by
      default
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1154
* Feature: New `dhall text` subcommand
    * This new subcommand supersedes the old `dhall-to-text` executable
* Feature: Add `instance Lift (Expr s a)`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1119
* Fixes and improvements to error messages:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1030
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1137
* Fixes and improvements to tests:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1155
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1159
* Performance improvements
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1036
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1051
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1048
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1057
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1065
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1066
    * See: https://github.com/dhall-lang/dhall-haskell/pull/1085

1.24.0

* Supports version 8.0.0 of the standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v8.0.0
* BREAKING CHANGE: Allow tabs and blank lines in multi-line strings
    * Blank lines are now ignored for the purpose of dedenting multiline strings
    * Lines with leading tabs (or mixed tabs and spaces) are now dedented, too,
      so long as they all share the same prefix
    * This is technically a breaking change, but unlikely to affect programs
      in practice, especially if they were formatted with `dhall format`.  This
      change mainly affects programs that were not indented correctly.
    * See the changelog for standard version 8.0.0 for more details
* BREAKING CHANGE: Simplify bare interpolations
    * Expressions like `λ(x : Text) → "${x}"` now simplify to `λ(x : Text) → x`
    * This is a technically breaking change because it changes how these sorts
      of expressions are serialized.  This does not affect semantic integrity
      checks and the new simplified expressions are extensionally equivalent to
      their older counterpart expressions.
    * See the changelog for standard version 8.0.0 for more details
* BREAKING CHANGE: Encode integrity check as multihash
    * Semantic integrity checks are now encoded using the multihash spec
    * This is a technically breaking change that does not perturb the hash for
      user-facing semantic integrity checks.  This only affects how expressions
      with unresolved imports are serialized, but semantic integrity checks are
      only computed for fully-resolved imports.
    * See the changelog for standard version 8.0.0 for more details
* BUG FIX: Fix type-checker to reject invalid record type annotations
    * e.g. `{ x = 1 } : { x : Text }` was not properly rejected by the type
      checker
    * See: https://github.com/dhall-lang/dhall-haskell/pull/965
* BUG FIX: Custom header forwarding fixed
    * Forwarding custom headers could previously fail in various ways, such as:
        * Cyclic imports leading to endless network requests
        * Resolving a non-existent import for the custom headers
        * Resolving an existing but incorrect import for the custom headers
    * This change fixes that by forwarding custom headers by value instead of
      by reference
    * See: https://github.com/dhall-lang/dhall-haskell/pull/967
* BUG FIX: Fix GHCJS support
    * `Natural/fold` was broken in version 1.22, which this change fixes
    * Specifically, it would hang for `Natural` numbers greater than 1
    * See: https://github.com/dhall-lang/dhall-haskell/pull/985
* BUG FIX: `dhall diff` no longer double-prints key-value separators
    * See: https://github.com/dhall-lang/dhall-haskell/pull/952
* Feature: Record projection by expression
    * You can now project out a subset of record fields by the expected type
    * `let t = { x : Natural } let p = { x = 1, y = 2 } in p.(t) = { x = 1 }`
    * See the changelog for standard version 8.0.0 for more details
* Feature: Inline headers
    * You no longer need to specify custom headers in a separate import.  You
      can now specify them inline within the same file.
    * e.g.: `https://example.com/x using [ { header = "Foo", value = "Bar" } ]`
    * See the changelog for standard version 8.0.0 for more details
* Feature: Allow `Sort` as a type annotation
    * An expression such as `Kind → Kind : Sort` will now type-check
    * `Sort` is still disallowed outside of a type annotation
    * See the changelog for standard version 8.0.0 for more details
* Feature: Allow self-describe-cbor when decoding
    * Dhall expressions serialized as CBOR can be tagged to describe themselves
      as CBOR without affecting decoding
    * See the changelog for standard version 8.0.0 for more details
* Feature: New `--file` option for `dhall` commands
    * In other words, instead of `dhall <<< './some/file` you can now use
      `dhall --file some/file`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/949
* Feature: New `--cache` flag for `dhall freeze` command
    * This automates the idiom used by the Prelude to optimistically cache
      imports but gracefully degrade if the semantic integrity check fails
    * See: https://github.com/dhall-lang/dhall-haskell/pull/980
* Feature: Add `:clear` command to `dhall repl`
    * This deletes previous bindings from the history so that they can be
      garbage collected
    * See: https://github.com/dhall-lang/dhall-haskell/pull/966
* Feature: New `chunkExprs` `Traversal` added to `Dhall.Core`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/954
* Feature: New `Dhall.Optics` module
    * This re-exports some convenient @lens@ utilities used internally for
      packages trying to avoid a @lens@ dependency
    * See: https://github.com/dhall-lang/dhall-haskell/pull/986
* More GHC 8.8 support
    * See: https://github.com/dhall-lang/dhall-haskell/pull/961

1.23.0

* BREAKING CHANGE: Fix marshaling union literals
    * 1.22.0 introduced two separate bugs in marshaling union literals between
      Dhall and Haskell, which this release fixes:
        * Dhall enums did not correctly map onto Haskell enums
        * New-style union literals (i.e. `< A : T >.A x`) were not correctly
          supported
    * See: https://github.com/dhall-lang/dhall-haskell/pull/918
    * See: https://github.com/dhall-lang/dhall-haskell/pull/927
    * See: https://github.com/dhall-lang/dhall-haskell/pull/936
* BUG FIX: Fix α-normalization
    * Version 1.22.0 introduced a new faster evaluation algorithm, but the new
      algorithm introduced two α-normalization regression, which this release
      fixes
    * The primary effect of this bug was that semantic integrity checks would
      fail for expressions that contain an `if`/`then`/else` expression in their
      normal form
    * See: https://github.com/dhall-lang/dhall-haskell/pull/931
    * See: https://github.com/dhall-lang/dhall-haskell/pull/938
* BUG FIX: Fix merging of sort-level record types
    * The language standard requires that `{ a : Kind } ⩓ { b : Kind }` is
      valid, which this change fixes
    * See: https://github.com/dhall-lang/dhall-haskell/pull/891
* BUG FIX: `dhall freeze` respects the `--ascii` flag
    * See: https://github.com/dhall-lang/dhall-haskell/pull/934
* BUG FIX: Don't autocomplete fields for record types
    * This prevents the REPL from expanding `{ x : T }.<TAB>` to `{ x : T }.x`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/937
* Support `MonadFail`-related changes in GHC 8.8
    * See: https://github.com/dhall-lang/dhall-haskell/pull/912
* Add `cross` flag to simplify cross-compilation
    * This allows the `dhall` package to be built without using
      `TemplateHaskell`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/928
* Increase lines of context for error messages 
    * Error messages now provide at least 20 lines of context instead of 3
      before truncating large expressions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/916
* Add line numbers to error messages
    * The bottom of every Dhall type error includes the original source code,
      which now has line numbers on the left margin
    * See: https://github.com/dhall-lang/dhall-haskell/pull/919
* Expand lower bounds on `megaparsec`/`transformers-compat` dependencies
    * This is to support `dhall` on Debian Sid
    * See: https://github.com/dhall-lang/dhall-haskell/pull/939

1.22.0

* Supports version 7.0.0 of the standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v7.0.0
* BREAKING CHANGE: Add support for empty alternatives
    * The `Union` type now has an optional (`Maybe`) type for each alternative
    * See the changelog for standard version 7.0.0 for more details
    * See: https://github.com/dhall-lang/dhall-haskell/pull/863
* BREAKING CHANGE: Remove support for URL fragments
    * The `URL` type no longer has a field for a URL fragment since the language
      no longer supports fragments
    * See the changelog for standard version 7.0.0 for more details
    * See: https://github.com/dhall-lang/dhall-haskell/pull/851
* BREAKING CHANGE: Remove deprecated `Path` type synonym
    * See: https://github.com/dhall-lang/dhall-haskell/pull/858
* BUG FIX: Correctly parse identifiers beginning with `http`
    * i.e. `httpPort` was supposed to be a valid identifier name and now is
    * See: https://github.com/dhall-lang/dhall-haskell/pull/870
* BUG FIX: Fix `dhall encode` bug
    * `dhall encode` bug was generating binary expressions that were valid
      (i.e. they would decode correctly) but were non-standard (i.e. hashing
      them would not match the hash you would normally get from a semantic
      integrity check)
    * Semantic integrity checks were not affected by this bug since they used
      a slightly different code path that generated the correct binary input to
      the hash.  Only the `dhall decode` subcommand was affected
    * See: https://github.com/dhall-lang/dhall-haskell/pull/859
* BUG FIX: Fix for `Dhall.UnionType`
    * This fixes some expressions that would previously fail to marshal into
      Haskell, specifically those were the marshalling logic was built using
      the `UnionType` utilities
    * See: https://github.com/dhall-lang/dhall-haskell/pull/857
* Feature: New `--alpha` flag to α-normalize command-line output
    * See: https://github.com/dhall-lang/dhall-haskell/pull/855
* Performance improvements
    * The normalizer is now *much* faster
    * See: https://github.com/dhall-lang/dhall-haskell/pull/876

1.21.0

* Supports version 6.0.0 of the language standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v6.0.0
* BREAKING CHANGE: Remove the `constructors` keyword
    * ... as standardized in version 6.0.0 of the language standard
    * The deprecation cycle is over, so the keyword is no longer supported
    * For more details, see: https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Deprecation-of-constructors-keyword
    * See: https://github.com/dhall-lang/dhall-haskell/pull/829
* BREAKING CHANGE: CBOR-encode only special `Double`s as half-floats
    * ... as standardized in version 6.0.0 of the language standard
    * CBOR `Double`s except `Infinity`/`-Infinity`/`NaN`/`0.0` are now encoded in at
      least 32 bits
    * See: https://github.com/dhall-lang/dhall-haskell/pull/822
* BREAKING CHANGE: Sort record and union fields when CBOR-encoding
    * Fields and alternatives are now sorted when serialized
    * This does not affect semantic integrity checks, which already sorted these
      fields/alternatives before hashing expressions
    * This does affect the serialization of expressions that have not been
      normalized (e.g. uninterpreted expressions transmitted over the wire)
    * See: https://github.com/dhall-lang/dhall-haskell/pull/835
* BUG FIX: Fix non-exhaustive pattern match in `dhall lint`
    * This fixes: `Irrefutable pattern failed for pattern Let (l' :| ls') d'`
    * This bug would cause `dhall lint` to fail on some nested `let`/`in`
      expressions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/780
    * See: https://github.com/dhall-lang/dhall-haskell/pull/784
* BUG FIX: Don't fail if `$HOME` environment variable is unset
    * The interpreter was incorrectly throwing an exception if `HOME` was unset
    * The standard requires that implementations should handle the `HOME`
      environment variable being missing
    * See: https://github.com/dhall-lang/dhall-haskell/pull/789
* Feature: Remove version tag from semantic integrity check
    * ... as standardized in version 6.0.0 of the language standard
    * This is not a breaking change because this change also includes
      backwards-compatible support for semantic integrity checks produced by
      older versions of the interpreter
* Feature: Support Unicode path components
    * ... as standardized in version 6.0.0 of the language standard
    * You can now use Unicode in path components if they are quoted
    * i.e. `./families/"禺.dhall"` is now legal
* Feature: Add `Text/show` built-in
    * ... as standardized in version 6.0.0 of the language standard
    * You can now convert a `Text` literal to its equivalent Dhall source code
      (which is itself a `Text` literal)
    * This comes in handy when using Dhall code to generate JSON or Dhall code
    * See: https://github.com/dhall-lang/dhall-haskell/pull/811
* Feature: Add `--immediate-dependencies`/`--transitive-dependencies` flags for
  `dhall resolve`
    * You can now retrieve all of your immediate or transitive dependencies as a
      textual list
    * This simplifies integration with other command-line tools (such as file
      watchers)
    * See: https://github.com/dhall-lang/dhall-haskell/pull/795
    * See: https://github.com/dhall-lang/dhall-haskell/pull/803
* Feature: `dhall freeze` now only freezes remote imports by default
    * `dhall freeze` used to freeze all imports (including local imports and
      environment variables)
    * Now `dhall freeze` only freezes remote imports by default, which is what
      most users want
    * You can install freeze all imports using the `--all` flag
    * See: https://github.com/dhall-lang/dhall-haskell/pull/808
* Feature: `:save` and `:load` REPL state
    * `:save` with no arguments now saves the REPL state to a `.dhall-repl-N`
       file
    * The file format is a list of `dhall repl` commands
    * You can use `:load` to load the saved state back into the REPL
    * See: https://github.com/dhall-lang/dhall-haskell/pull/807
* Feature: Add `:hash` command to `dhall repl`
    * This lets you conveniently hash expressions within the `dhall repl`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/806
* Feature: Add `--check` flag to `dhall format`
    * Use this to check if the input is already formatted
    * Useful for continuous integration when you want to ensure that all code
      under version control remains formatted
    * See: https://github.com/dhall-lang/dhall-haskell/pull/810
* Feature: Add  `UnionInputType` builder for `InputType`s
    * This is the union analog of `RecordInputType`, letting you build a
      record explicitly instead of deriving the instance using GHC generics
    * See: https://github.com/dhall-lang/dhall-haskell/pull/775
* Feature: Add `:set`/`:unset` commands to `dhall repl`
    * You can use these commands to set or unset command-line options
    * Currently only setting/unsetting `--explain` is supported
* Standards-compliance fixes:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/779
    * See: https://github.com/dhall-lang/dhall-haskell/pull/804
    * See: https://github.com/dhall-lang/dhall-haskell/pull/833
* Documentation fixes:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/792
    * See: https://github.com/dhall-lang/dhall-haskell/pull/825
* Test fixes:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/782
    * See: https://github.com/dhall-lang/dhall-haskell/pull/836
* Improved error messages:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/812
    * See: https://github.com/dhall-lang/dhall-haskell/pull/815
    * See: https://github.com/dhall-lang/dhall-haskell/pull/824
* Formatting fixes:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/831
* REPL fixes:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/837

1.20.1

* BUG FIX: Fix binary encoding to use correct standard version
    * This fixes computed hashes to correctly match standard version 5.0.0
    * This is not marked as a breaking change since it is a bug fix.  The
      1.20.0 release will be blacklisted on Hackage and users should upgrade
      from 1.19.* directly to 1.20.1
    * See: https://github.com/dhall-lang/dhall-haskell/pull/771

1.20.0

* Supports version 5.0.0 of the language standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v5.0.0
* BREAKING CHANGE TO THE LANGUAGE: Implement standardized support for multi-line
  literals
    * This updates the multi-line support to match the standard
    * This is a breaking change because empty lines within the multi-line
      literal now require leading whitespace whereas previously they did not
    * This is also a breaking change because now a newline is required after
      the opening `''` quotes whereas previously it was not required
    * If you use `dhall format` then your multi-line literals already have the
      necessary leading whitespace
* BREAKING CHANGE TO THE LANGUAGE: `constructors x = x`
    * Now the `constructors` keyword behaves like an identity function, since
      constructors can already be accessed as fields off the original union
      type.
    * This is a breaking change since any record of terms that contains a
      `constructors` field will now be a forbidden mixed record of types and
      terms.
    * This is also a breaking change if you annotated the type of what used to
      be a `constructors` record.
    * `dhall lint` will now remove the obsolete `constructors` keyword for you
    * See: https://github.com/dhall-lang/dhall-haskell/pull/693
    * See: https://github.com/dhall-lang/dhall-haskell/pull/701
* BREAKING CHANGE TO THE API: Restore `Parent` constructor for `Local` type
    * This more closely matches the standard and also enables `dhall format` to
      produce a leading `../` for imports instead of `./../`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/718
* BUG FIX: Fix type-checking bug for unions
    * The first fix was that the inferred type was wrong for unions where
      alternatives were types or kinds
    * The second fix was that unions that mixed terms/types/kinds were not
      properly rejected
    * See: https://github.com/dhall-lang/dhall-haskell/pull/763
* BUG FIX: Change how `dhall repl` handles prior definitions
    * This changes the REPL to handle previous bindings as if they were
      defined using a large `let` expression instead of adding them to the
      context
    * This fixes some type-checking false negatives
    * See: https://github.com/dhall-lang/dhall-haskell/pull/729
* Feature: Autocomplete for `dhall repl`
    * You can now auto-complete record fields, union constructors, and
      identifiers that are in scope
    * See: https://github.com/dhall-lang/dhall-haskell/pull/727
* Feature: GHCJS support
    * `dhall` can now be built using GHCJS, although some features are still
      not supported for GHCJS, such as:
        * Semantic integrity checks
        * Custom HTTP headers
    * Also, HTTP imports only work for URLs that support CORS
    * See: https://github.com/dhall-lang/dhall-haskell/pull/739
* Feature: Add support for records of records of types
    * You can now nest records of types
    * See: https://github.com/dhall-lang/dhall-haskell/pull/700
* Feature: Add `:quit` command for `dhall repl`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/719
* Feature: Add `--json` flag for `dhall {encode,decode}`
    * You can now produce/consume CBOR expressions via JSON instead of binary
    * See: https://github.com/dhall-lang/dhall-haskell/pull/717
* Feature: Add decoding logic for `as Text`
    * You can now preserve the `as Text` qualifier on imports when serializing
      them
    * See: https://github.com/dhall-lang/dhall-haskell/pull/712
* Prenormalize substituted expressions
    * This is a performance improvement that reduces the time and memory
      consumption when normalizing expressions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/765

1.19.1


* BUG FIX: Fix serious `dhall lint` bug
    * `dhall lint` would sometimes remove `let` expressions that were still
      in use
    * See: https://github.com/dhall-lang/dhall-haskell/pull/703
* BUG FIX: Fix import caching efficiency bug
    * Some imports were being wastefully fetched multiple times
    * See: https://github.com/dhall-lang/dhall-haskell/pull/702
* Feature: Generate dot graph to visualize import graph
    * Use the `dhall resolve --dot` command
    * See: https://github.com/dhall-lang/dhall-haskell/pull/698
    * See: https://github.com/dhall-lang/dhall-haskell/pull/713
* Improve HTTP error messages
    * See: https://github.com/dhall-lang/dhall-haskell/pull/710

1.19.0

* Supports version 4.0.0 of the language standard
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v4.0.0
* BREAKING CHANGE TO THE LANGUAGE AND API: Prevent Hurkens' paradox
    * This fixes a type-checking soundness bug which permitted infinite loops
    * This is a breaking change because infinite loops are no longer possible
    * This is also a breaking change because a record of types is now treated as
      a kind instead of a type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/680
* BREAKING CHANGE TO THE LANGUAGE AND API: `Double`s are now double-precision
  floating point numbers
    * This restricts the range of `Double`s to IEEE 754 double-precision
      floating point
    * This also implies that you can no longer convert `Scientific` values to
      `Dhall` expressions (i.e. no `Inject` instance for `Scientific`)
    * See: https://github.com/dhall-lang/dhall-haskell/pull/667
* BREAKING CHANGE TO THE API: Preserve field order for record projection
    * The API uses a new `Dhall.Set.Set` type instead of `Data.Set.Set`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/670
* BREAKING CHANGE TO THE API: Add support for multi-`let` expressions
    * This changes the `Let` constructor to now support storing multiple
      bindings per `let` expression
    * See: https://github.com/dhall-lang/dhall-haskell/pull/675
* Access constructors as if they were fields of the union type
    * In other words: `< Left : Bool | Right : Natural >.Left`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/657
* Support GHC 8.6
    * See: https://github.com/dhall-lang/dhall-haskell/pull/669
* Add support for quoted path components
    * i.e. `/"foo"/bar/"baz qux"` or `https://example.com/foo/"bar?baz"?qux`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/690
* Fix parsing of `//\\` operator
    * See: https://github.com/dhall-lang/dhall-haskell/commit/9d0fd42d95ab69fa64da4afd8b60d69aca8e65a6
* Preserve Unicode characters when formatting code
    * See: https://github.com/dhall-lang/dhall-haskell/pull/679
* Allow identifier names to begin with `Some`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/658
* Add `subExpressions` `Traversal`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/660
* Add `normalizeWithM` for monadic normalization
    * See: https://github.com/dhall-lang/dhall-haskell/pull/371
* Custom normalizers now take precedence over default normalization logic
    * This allows one to override the implementation of built-in operators
    * See: https://github.com/dhall-lang/dhall-haskell/pull/684

1.18.0

* Supports version 3.0.0 of the language standard:
    * See: https://github.com/dhall-lang/dhall-lang/releases/tag/v3.0.0
* BREAKING CHANGE TO THE LANGUAGE AND API: New `Some`/`None` constructors for
  `Optional` values
    * Example: `[ Some 1, None Natural ]`
    * This is a breaking change to the language because `Some` and `None` are
      now reserved keywords
    * This is a breaking change to the API because `Some` and `None` are new
      constructors for the `Expr` type
* BREAKING CHANGE TO THE LANGUAGE AND API: Support for kind polymorphism
    * This adds a new `Sort` constant above `Kind` in the hierarchy
    * i.e. `Type : Kind : Sort`
    * This is a breaking change to the language because `Sort` is now a
      reserved keyword
    * This is a breaking change to the API because `Sort` is a new
      constructor for the `Expr` type
* BREAKING CHANGE TO THE API: New `Dhall.Map` module
    * This replaces `InsOrdHashMap` in the API
    * The primary motivation is to improve performance and to remove the
      dependency on `insert-ordered-containers`
* BREAKING CHANGE TO THE API: Use standard version instead of protocol version
    * The binary protocol is now versioned alongside the standard
    * The `ProtocolVersion` type is renamed to `StandardVersion` and the
    * `--protocol-version` option is renamed to `--standard-version`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/634
* BUG FIX: Fix import chaining for custom header imports
    * See: https://github.com/dhall-lang/dhall-haskell/pull/618
* BUG FIX: Fix import chaining for imports protected by semantic integrity
  checks
    * See: https://github.com/dhall-lang/dhall-haskell/pull/584
* BUG FIX: Record literals and types produced by `∧`/`⫽`/`⩓` are now sorted
    * This ensures that β-normalization is idempotent
    * See: https://github.com/dhall-lang/dhall-haskell/pull/572
* BUG FIX: `dhall freeze` now correctly handles the starting file being
  located outside the current working directory
    * See: https://github.com/dhall-lang/dhall-haskell/commit/a22aa79d1957be9ecf166ea066e2a9a5b309e1ae
* BUG FIX: Fix parsing of IPv4-mapped IPv6 addresses
    * See: https://github.com/dhall-lang/dhall-haskell/pull/632
* FEATURE: New `--ascii` flag for ASCII output
    * See: https://github.com/dhall-lang/dhall-haskell/pull/570
* FEATURE: New `dhall encode` and `dhall decode` subcommands
    * These allow you to transform Dhall source code to and from its binary
      representation
    * See: https://github.com/dhall-lang/dhall-haskell/pull/588
* LARGE parsing performance improvements
    * Parsing is about 10x-100x faster on most code
    * See: https://github.com/dhall-lang/dhall-haskell/pull/591
    * See: https://github.com/dhall-lang/dhall-haskell/pull/592
    * See: https://github.com/dhall-lang/dhall-haskell/pull/597
    * See: https://github.com/dhall-lang/dhall-haskell/pull/601
    * See: https://github.com/dhall-lang/dhall-haskell/pull/602
    * See: https://github.com/dhall-lang/dhall-haskell/pull/604
    * See: https://github.com/dhall-lang/dhall-haskell/pull/606
* Type-checking performance improvements:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/566
* Normalization performance improvements:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/610
* `dhall freeze` now caches the imports as it freezes them
    * See: https://github.com/dhall-lang/dhall-haskell/pull/587
* `dhall freeze` now refreezes imports with invalid semantic integrity checks
    * See: https://github.com/dhall-lang/dhall-haskell/pull/637
* `dhall freeze` now adds a trailing newline
    * See: https://github.com/dhall-lang/dhall-haskell/pull/629
* Build against `megaparsec-7.0.*`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/565
* Support GHC 8.6
    * See: https://github.com/dhall-lang/dhall-haskell/pull/599
    * See: https://github.com/dhall-lang/dhall-haskell/pull/623
* Support GHC all the way back to 7.10.3
    * See: https://github.com/dhall-lang/dhall-haskell/pull/595
    * See: https://github.com/dhall-lang/dhall-haskell/pull/621
* Improvements to error messages:
    * See: https://github.com/dhall-lang/dhall-haskell/pull/563
    * See: https://github.com/dhall-lang/dhall-haskell/pull/576
    * See: https://github.com/dhall-lang/dhall-haskell/pull/583
    * See: https://github.com/dhall-lang/dhall-haskell/pull/589


1.17.0

* This release corresponds to version 2.0.0 of the language standard
* BREAKING CHANGE TO THE LANGUAGE AND API: Binary serialization support
    * This is a breaking change to the hash for all semantic integrity checks
    * The hash used by the semantic integrity check is now based on the
      binary representation instead of a text representation of the
      expression
    * You can pin the new hashes by supplying the `--protocol-version 1.0`
      option on the command line until you need support for newer language
      features 
    * This also includes a breaking change to `ImportType` in the API
* BREAKING CHANGE TO THE LANGUAGE: Disallow combining records of terms and
  types
    * This is mainly for consistency and to improve type errors that would
      have otherwise happened further downstream
    * This should not affect the vast majority of code
    * See: https://github.com/dhall-lang/dhall-haskell/pull/538
* BUG FIX: Semantic integrity checks now work for imported expression using
  the `constructors` keyword
    * See: https://github.com/dhall-lang/dhall-haskell/pull/554
* BUG FIX: Fix α-normalization of expressions with bound variables named `_`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/524
* BUG FIX: Fix `isNormalized` to match `normalize`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/522
* BUG FIX: `dhall lint` now correctly handles nested `let` expressions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/555
* FEATURE: Imports protected by a semantic integrity check are now cached
    * See: https://github.com/dhall-lang/dhall-haskell/pull/533
* The default `dhall` command no longer outputs the type to `stderr`
    * You can add back the type as a type annotation using the
      `--annotate` switch
    * See: https://github.com/dhall-lang/dhall-haskell/pull/544
* New utilities for building `InputTypes`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/530
* Improve parsing performance for long variable names
    * See: https://github.com/dhall-lang/dhall-haskell/pull/526
* More succinct type diffs for function types
    * See: https://github.com/dhall-lang/dhall-haskell/pull/540
* Identifier names can now begin with keywords
    * i.e. `ifChanged` and `lettuce` are now legal identifiers
    * See: https://github.com/dhall-lang/dhall-haskell/pull/551

1.16.1

* Fix test failure due to missing test data file

1.16.0

* BREAKING CHANGE: Consolidate `input` family of functions
    * These now take a record of options
    * This also `_stack` field of the `Status` type from `[Import]` to
      `NonEmpty Import`
* Permit `$` in quoted variable names
    * See: https://github.com/dhall-lang/dhall-haskell/pull/510

1.15.1

* Fix infinite loop when formatting expressions containing `?`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/491

1.15.0

* BREAKING CHANGE TO THE API: Support alternative imports using new `?` operator
    * This adds a new constructor which affects exhaustive pattern matches
    * See: https://github.com/dhall-lang/dhall-haskell/pull/473
* BREAKING CHANGE TO THE API: Add `Integer/toDouble` built-in function
    * This adds a new constructor which affects exhaustive pattern matches
    * See: https://github.com/dhall-lang/dhall-haskell/pull/434
* BREAKING CHANGE TO THE API: Use strict `Text` instead of lazy `Text`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/455
* BREAKING CHANGE TO THE API: Remove `Buildable` in favor of `Pretty`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/459
* BREAKING CHANGE TO THE API: Removed the `Parent` constructor from `FilePrefix`
    * Instead, use `Here` with a `".."` prefix.
    * See: https://github.com/dhall-lang/dhall-haskell/pull/407
* BUG FIX: Disallow duplicate fields in records
    * See: https://github.com/dhall-lang/dhall-haskell/pull/430
* BUG FIX: Fix stripping of leading whitespace in multi-line strings
    * See: https://github.com/dhall-lang/dhall-haskell/pull/469
* BUG FIX: Fix formatting field access of an import
    * See: https://github.com/dhall-lang/dhall-haskell/pull/471
* Add `dhall freeze` command
    * See: https://github.com/dhall-lang/dhall-haskell/pull/486
* Add `dhall diff` command
    * See: https://github.com/dhall-lang/dhall-haskell/pull/442
* Add `dhall lint` command
    * See: https://github.com/dhall-lang/dhall-haskell/pull/484
* Change `dhall-repl`/`dhall-hash`/`dhall-format` to `dhall` subcommands
    * i.e. `dhall repl`/`dhall hash`/`dhall format`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/435
    * See: https://github.com/dhall-lang/dhall-haskell/pull/452
* Add `with-http` cabal flag to disable support for remote imports
    * See: https://github.com/dhall-lang/dhall-haskell/pull/482
* Added `inputFrom` and `inputFromWith`
    * These allow naming the file that the expression is coming from for better
      error messages
    * See: https://github.com/dhall-lang/dhall-haskell/pull/464
* Performance improvements
    * See: https://github.com/dhall-lang/dhall-haskell/pull/420
* Tutorial recommends GitHub for Prelude instead of IPFS
    * See: https://github.com/dhall-lang/dhall-haskell/pull/479
* Pretty-print expressions in type errors
    * See: https://github.com/dhall-lang/dhall-haskell/pull/429
* Formatting improvements
    * See: https://github.com/dhall-lang/dhall-haskell/pull/398
    * See: https://github.com/dhall-lang/dhall-haskell/pull/458
* Diff improvements
    * See: https://github.com/dhall-lang/dhall-haskell/pull/455
    * See: https://github.com/dhall-lang/dhall-haskell/pull/470
    * See: https://github.com/dhall-lang/dhall-haskell/pull/478

1.14.0

* BREAKING CHANGE TO THE LANGUAGE: Switch grammar of `Natural` and `Integer`
    * `Natural` number literals are now unsigned and `Integer` literals always
      require a sign
    * This is a **VERY** disruptive change to most Dhall code in the wild but
      was unanimously agreed upon here:
      https://github.com/dhall-lang/dhall-lang/issues/138
    * See also: https://github.com/dhall-lang/dhall-haskell/pull/381
* BREAKING CHANGE TO THE LANGUAGE: Drop support for importing directories
    * Importing `dir/` used to resolve to `dir/@`, which is no longer supported
    * See: https://github.com/dhall-lang/dhall-haskell/pull/384
* BREAKING CHANGE TO THE LANGUAGE: Change to the grammar for imports
    * File path components can no longer contain `#` or `?` characters
    * URL imports must now contain at least one path component
    * URL path components must match the grammar for file path components
    * See: https://github.com/dhall-lang/dhall-haskell/pull/390
* BREAKING CHANGE TO THE API: Rename `Path{,Mode,Hashed,Type}` to
  `Import{,Mode,Hashed,Type}`
    * In practice this change is not breaking for the most common use cases
      since this also provides a `Path` type synonym for backwards compatibility
    * See: https://github.com/dhall-lang/dhall-haskell/pull/376
* BUG FIX: Fix α-equivalence bug when type-checking `merge`
    * `merge` expressions would sometimes reject valid code due to a
       type-checking bug
    * See: https://github.com/dhall-lang/dhall-haskell/pull/394
* Improve import caching
    * See: https://github.com/dhall-lang/dhall-haskell/pull/388
    * See: https://github.com/dhall-lang/dhall-haskell/pull/392
* Increase upper bound on `tasty`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/382
* Fix lower bound on `insert-ordered-containers`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/377

1.13.1

* Increase upper bound on `ansi-terminal` and `megaparsec`

1.13.0

* BUG FIX: Fix semantic integrity hashing support
    * Both parsing and pretty-printing semantic hashes were broken since version
      1.11.0
    * See: https://github.com/dhall-lang/dhall-haskell/pull/345
* BUG FIX: Allow leading whitespace in interpolated expresssions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/369
* BUG FIX: Fix `deriving (Interpret)` for sum types
    * The types of alternatives were not correctly included in the corresponding
      Dhall type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/348
* BREAKING CHANGE TO LANGUAGE: Records cannot store both types and terms
    * Records can also not store type-level functions (like `List`)
        * Records might be allowed to store type-level functions again in the
          future
    * This fixes a potential soundness bug
    * The primarily practical consequence of this change is that if you are
      hosting a "package" then you will need to split terms and types from your
      package into different records for your users to import
    * This also implies removing the `./Monoid` type-level function from the
      `./Prelude/package.dhall` record
    * See: https://github.com/dhall-lang/dhall-haskell/pull/335
* BREAKING CHANGE TO THE API: Replace `trifecta` with `megaparsec`
    * This change the API to use the `Parser` type from `megaparsec`
    * This also slightly changes the type of `exprFromText`
    * If you program using the type classes provided by the `parsers` library
      then this is not a breaking change as that interface is preserved
    * See: https://github.com/dhall-lang/dhall-haskell/pull/268
* BREAKING CHANGE TO THE API: New `⩓` operator for merging record types
    * Example: `{ foo : Text } ⩓ { bar : Bool } = { foo : Text, bar : Bool }`
    * This is breaking because it adds a new constructor to the `Expr` type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/342
* BREAKING CHANGE TO THE API: New support for projecting a subset of fields
    * Example: `{ x = 1, y = 2, z = 3 }.{ x, y } = { x = 1, y = 2 }`
    * This is breaking because it adds a new constructor to the `Expr` type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/350
* API+UX feature: New support for pretty-printing diffs of Dhall expressions
    * Error messages also use this feature to simplify large type mismatches
    * There is also a new `Dhall.Diff` module
    * See: https://github.com/dhall-lang/dhall-haskell/pull/336
* Add `version`, `resolve`, `type`, and `normalize` sub-commands to interpreter
    * See: https://github.com/dhall-lang/dhall-haskell/pull/352
* Support GHC 7.10.3
    * See: https://github.com/dhall-lang/dhall-haskell/pull/340
* `:type` command in `dhall-repl` now only displays the type
    * Before it would also display the original expression
    * See: https://github.com/dhall-lang/dhall-haskell/pull/344
* Trim dependency tree
    * See: https://github.com/dhall-lang/dhall-haskell/pull/351
    * See: https://github.com/dhall-lang/dhall-haskell/pull/268
    * See: https://github.com/dhall-lang/dhall-haskell/pull/355

1.12.0

* Additional changes to support GHC 8.4
    * See: https://github.com/dhall-lang/dhall-haskell/pull/331
* BREAKING CHANGE TO API: Replace dependency on `text-format` with `formatting`
    * This replace the `Data.Text.Buildable.Buildable` instances with
      `Formatting.Buildable.Buildable` instances, which is why this is a
       breaking change
    * `text-format` is no longer maintained and blocking GHC 8.4 support
    * See: https://github.com/dhall-lang/dhall-haskell/pull/330

1.11.1

* Support GHC 8.4
    * See: https://github.com/dhall-lang/dhall-haskell/pull/321
* Fix α-normalization bug
    * Note that this is not a type-checking bug.  This only affects users who
      were directly using the `alphaNormalize` function from the Haskell API
      because `let` expressions were not correctly α-normalized
    * See: https://github.com/dhall-lang/dhall-haskell/pull/319
* Slight tweak to syntax highlighting
    * See: https://github.com/dhall-lang/dhall-haskell/pull/324
* Increase upper bound on `ansi-terminal` and `exceptions`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/322
    * See: https://github.com/dhall-lang/dhall-haskell/pull/325

1.11.0

* BREAKING CHANGE TO THE API: Fix `{Natural,Optional,List}/build` semantics to
  match standard
    * This is a breaking change because the `OptionalLit` and `ListLit`
      constructors changed their representations to efficiently support the
      standard semantics
    * `ListLit` now stores a `Data.Sequence.Seq` instead of a
      `Data.Vector.Vector`
    * `OptionalLit` now stores a `Maybe` instead of a `Data.Vector.Vector`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/300
* BREAKING CHANGE TO THE COMMAND LINE: `dhall` executable always formats output
    * Previously you had to opt into formatting using `--pretty`
    * Now formatting is obligatory and the `--pretty` flag is gone
    * See: https://github.com/dhall-lang/dhall-haskell/pull/303
* Feature: New `:save` command for `dhall-repl`
    * Now you can save an expression to a file: `./yourFile = someExpression`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/309
* Improvement: Add new simplifications to match standard
    * See: https://github.com/dhall-lang/dhall-haskell/pull/312
    * See: https://github.com/dhall-lang/dhall-haskell/pull/316
* Improvement: Fix equivalence check to match standard
    * Practically this means that more corner cases of the language correctly
      type-check than before
* Improvement: New `--plain` flag to disable syntax highlighting
    * See: https://github.com/dhall-lang/dhall-haskell/pull/310
* Improvement: Prelude now provides an umbrella `package.dhall` import
    * This is primarily for convenience
    * See: https://github.com/dhall-lang/dhall-haskell/pull/298
* Improvement: Context is now normalized
    * See: https://github.com/dhall-lang/dhall-haskell/pull/302
* Replace `cryptohash` dependency with `cryptonite`
    * See: https://github.com/dhall-lang/dhall-haskell/commit/5d2012927a062ec8bdf2bbaba77150344f38db77
* Increase upper bound on exceptions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/306
* Fix type error in tutorial
    * See: https://github.com/dhall-lang/dhall-haskell/commit/5a9126b2f684d3236fc1e8e20e206cfaf47d97db

1.10.0

* Feature: Records/unions can now have fields/alternatives that are types
    * i.e. `{ foo = Text, bar = List }` is legal now
    * See: https://github.com/dhall-lang/dhall-haskell/pull/273
* Feature: New `dhall-repl` for interactively evaluating Dhall expressions
    * See: https://github.com/dhall-lang/dhall-haskell/pull/266
* Feature: Syntax highlighting
    * See: https://github.com/dhall-lang/dhall-haskell/pull/260
* Feature: BREAKING CHANGE TO THE API: `dhall-format` preserves field order
    * This changes the syntax tree to use an `InsOrdHashMap` instead of a `Map`
* BREAKING CHANGE TO THE API: Use Haskell's `Scientific` type
    * This is fixes the interpreter to correct handle really large/small numbers
    * This also allows marshaling into Haskell's `Scientific` type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/256
* BREAKING CHANGE TO THE API: Remove `system-filepath`/`system-fileio` dependencies
    * Now the library uses `Prelude.FilePath`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/248
* Feature: Labels can now begin with reserved names
    * i.e. `List/map` is now a legal label
    * See: https://github.com/dhall-lang/dhall-haskell/pull/255
* Fix: Rendered labels are now correctly escaped if they are numbers
    * See: https://github.com/dhall-lang/dhall-haskell/pull/252
* Add the instance `Interpret String`.
    * See: https://github.com/dhall-lang/dhall-haskell/pull/247
* Fix: Custom contexts passed to `typeWith` are now checked
    * This prevents a custom context from triggering an infinite loop
    * See: https://github.com/dhall-lang/dhall-haskell/pull/259

1.9.1

* `dhall-format` now emits single-quoted strings for multi-line strings
    * See: https://github.com/dhall-lang/dhall-haskell/pull/237
* Improved error messages for list elements with the wrong type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/236
* Change `lens` dependency to `lens-family-core`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/238

1.9.0

* Feature: BREAKING CHANGE TO LANGUAGE AND API: Add `constructors` keyword
    * This new keyword generates constructors from a union type
        * See the updated Haskell tutorial for more details
    * This means that `constructors` is now a reserved keyword
    * This adds a new `Constructors` constructor to the `Expr` type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/199
* Feature: BREAKING CHANGE TO THE API: `dhall-format` preserves interpolation
    * This changes the `TextLit` constructor to represent an interpolated `Text`
      literal
    * See: https://github.com/dhall-lang/dhall-haskell/pull/220
* Feature: You can now define type synonyms using `let`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/202
* Feature: Extend valid set of quoted labels
    * See: https://github.com/dhall-lang/dhall-lang/pull/65
    * See: https://github.com/dhall-lang/dhall-lang/pull/77
* Performance: Improve startup time when importing files, but not URLs
    * See: https://github.com/dhall-lang/dhall-haskell/pull/194
* Security: `localhost`/`127.0.0.1` imports no longer count as local imports
    * Specifically: they cannot import environment variables or files
    * See: https://github.com/dhall-lang/dhall-haskell/pull/197
* Security: Fix potential type-checking bug
    * See: https://github.com/dhall-lang/dhall-haskell/pull/198
* Fix: BREAKING CHANGE TO API: Improve localization of error messages
    * This required fixing the type of `normalize`/`shift`/`subst` to preserve
      the first type parameter of `Expr` (i.e. they no longer delete `Note`
      constructors)
    * A new `denote` function was added for the explicit purpose of deleting
      `Note` constructors
    * See: https://github.com/dhall-lang/dhall-haskell/pull/218
* Expose `MissingEnvironmentVariable` exception type
    * See: https://github.com/dhall-lang/dhall-haskell/pull/196
* Add `genericAuto`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/195
* Add `inputWith`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/222
* Add`loadWithContext`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/215
* Add `pair`/`unit`/`string`/`list`
    * See: https://github.com/dhall-lang/dhall-haskell/pull/227

1.8.2

* Add `typeWithA` for type-checking custom `Embed`ded values
* Fix `dhall{,-*}` executables to ignore ambient locale and use UTF8
* Increase upper bound on `tasty` dependency

1.8.1

* `dhall` executable can now format output using `--pretty`
* Improved Unicode suppport on Windows

1.8.0

* BREAKING CHANGE TO LANGUAGE: Add support for import integrity checks
    * In practice, the likelihood of this breaking code in the wild is
      astronomically low
    * This would only break code of the form `sha256:aaa...aaa` (i.e. a
      variabled named `sha256` with a type annotation for a type with a name
      64 characters long drawn from the first 6 characters of the alphabet)
* BUG FIX: Fix parsing of single quotes in single-quoted strings
* BUG FIX: Fix superfluous parentheses introduced by `dhall-format`
* New `dhall-hash` executable
    * This goes hand-in-hand with the added support for integrity checks since
      the executable lets you compute the current hash of an import

1.7.0

* BREAKING CHANGE TO LANGUAGE: Update parser to match standardized grammar
    * Trailing commas and bars no longer supported for union and record literals
    * Paths no longer permit commas
    * URL grammar is now RFC-compliant
    * Environment variables can now be quoted to support full range of
      POSIX-compliant names
    * Text literals support full set of JSON escape sequences (such as `\u2192`)
* BREAKING CHANGE TO LANGUAGE: Single quoted strings strip leading newlines
* BUG FIX: Fixed type-checking infinite loops due to non-type-checked variables
  in context
* BUG FIX: Fixed type-checking bug due to missing context when type-checking
  certain expressions
* BUG FIX: Fixed type-checking bug due to off-by-one errors in name shadowing
  logic
* New `dhall-format` executable to automatically format code
* Performance optimizations to `Natural/fold` and `List/fold`
* Improved parsing performance (over 3x faster)
* Union literals can now specify the set value anywhere in the literal
    * i.e. `< A : Integer | B = False | C : Text >`
* New `Inject` instance for `()`
* Several tutorial fixes and improvements

1.6.0

* BREAKING CHANGE TO THE API: Drop support for GHC 7.*
* BREAKING CHANGE TO THE API: Add support for customizing Dhall import
    * This is a breaking change because this changes the type of `loadWith`
* BREAKING CHANGE TO THE API: Add field to `UnboundVariable` error containing
* BUG FIX: Fix parsing single quotes in string literals
  the name of the unbound variable
* Add `List/concatMap` to the Prelude
* You can now derive `Inject` and `Interpret` for types with unlabeled fields
* Add new instances for `Interpret`:
    * `[]`
    * `(,)`
* Add new instance for `Inject`
    * `[]`, `Data.Set.Set`, `Data.Sequence.Seq`
    * `(,)`
    * `Int`, `Word8`, `Word16`, `Word32`, `Word64`
* Add `Eq` instance for `Src`

1.5.1

* Increase upper bound on `vector` and `optparse-generic`

1.5.0

* BREAKING CHANGE: Add list concatenation operator: `(#)`
    * This is a breaking change because it adds a new constructor to the `Expr`
      type which breaks exhaustive pattern matches
* BREAKING CHANGE: Add `Interpret` support for lazy `Text`
    * This is a breaking change because it renames `text` to `strictText`
* Add `Interpret` instance for decoding (a limited subset of) Dhall functions
* Dhall no longer requires Template Haskell to compile
    * This helps with cross-compilation
* Add `rawInput` utility for decoding a Haskell value from the `Expr` type
* Add `loadWith`/`normalizeWith` utilities for normalizing/importing modules
  with a custom context
* Export `Type` constructor

1.4.2

* Fix missing `Prelude` files in package archive uploaded to Hackage

1.4.1

* Fix missing `tests/Tutorial.hs` module in package archive uploaded to Hackage

1.4.0

* BREAKING CHANGE TO THE LANGUAGE AND API: You can now supply custom headers for
  URL imports with the new `using` keyword
    * This is a breaking change to the language because this adds a new reserved
      `using` keyword
    * This is a breaking change to the API because this adds a new field to the
      `URL` constructor to store optional custom headers
* BUG FIX: `:` is no longer a disallowed path character
    * This was breaking URL imports with a port
* BUG FIX: If you import a home-anchored path (i.e. `~/foo`) and that imports a
  relative path (like `./bar`), then the canonical path of the relative import
  should be home-anchored (i.e. `~/bar`).  However, there was a bug that made
  lose the home anchor (i.e. `./foo/bar`), which this release fixes
  likely fail due to no longer being home-anchored (i.e. `./foob
* Add support for string interpolation
* `merge` no longer requires a type annotation if you are merging at least one
  alternative
* Expanded Prelude
    * `./Prelude/Optional/all`
    * `./Prelude/Optional/any`
    * `./Prelude/Optional/filter`
    * `./Prelude/Optional/length`
    * `./Prelude/Optional/null`
    * `./Prelude/Text/concatMap`
    * `./Prelude/Text/concatMapSep`
    * `./Prelude/Text/concatSep`
* Rearrange detailed error messages to put summary information at the bottom of
  the message

1.3.0

* BREAKING CHANGE TO THE API: Add support for new primitives, specifically:
    * `(//)` - Right-biased and shallow record merge
    * `Optional/build` (now a built-in in order to support build/fold fusion)
    * `Natural/show`
    * `Integer/show`
    * `Double/show`
    * `Natural/toInteger`
    * These all add new constructors to the `Expr` type, which would break
      exhaustive pattern matches
* BREAKING CHANGE TO THE LANGUAGE: Imported paths and URLs no longer support
  the characters: "()[]{}<>:"
    * This reduces the number of cases where you have to add a space after
      imports
    * Note that this does not exclude the `:` in the URL scheme (i.e. `http://`)
* Increase connection timeout for imports
* Variable names now allow the `-` character for all but the first character
* You can now escape identifiers with backticks
    * This lets you name identifiers so that they don't conflict with reserved
      key words
    * This is most useful when converting Dhall to other file formats (like
      JSON) where you might need to emit a field that conflicts with one of
      Dhall's reserved keywords
* New `--version` flag for the `dhall` executable

1.2.0

* BREAKING CHANGE: Add support for customizing derived `Interpret` instances
    * This is a breaking change to the Dhall library API since this changes the
      signature of the `Interpret` class by replacing the `auto` method with a
      more general `autoWith` method.  This `autoWith` now takes an
      `InterpretOptions` argument that lets you customize derived field and
      constuctor names
    * In practice user programs that use the common path will be unaffected by
      this change
    * This is not a breaking change to the Dhall language
* BREAKING CHANGE: Type annotations now bind more tightly than lambda
  abstraction
    * This is a breaking change to the Dhall language.  An expression like this:

      ```
      λ(x : A) → y : B
      ```

      ... used to parenthesized implicitly as:

      ```
      (λ(x : A) → y) : T
      ```

      ... but is now parenthesized implicitly as:

      ```
      λ(x : A) → (y : T)
      ```

      This is now consistent with Haskell's precedence and also consistent with
      the precedence of `List` and `Optional` type annotations

    * This change affects programs with an expression like this:

      ```
      -- Assuming that `y : B`
      λ(x : A) → y : A → B
      ```

      The above program would type-check before this change but not type-check
      after this change.  You would you need to fix the above program by either
      changing the type signature to annotate just the type of `y` like this:

      ```
      λ(x : A) → y : B
      ```

      ... or by adding explicit parentheses like this:

      ```
      (λ(x : A) → y) : A → B
      ```

    * This is not a breaking change to the Dhall library API
* BREAKING CHANGE: Add support for importing a path's contents as raw `Text` by
  adding `as Text` after the import
    * This is a breaking change to the Dhall language
    * This is technically a breaking change, but is extremely unlikely to affect
      you program.  This only changes the behavior of old programs that had an
      expression of the form:

      ```
      /some/imported/function as Text
      ```

      ... where `/some/imported/function` is an imported function being applied
      to two arguments, the first of which is a bound variable named `as` and
      the second of which is the type `Text`
    * This is not a breaking change to the Dhall library API
* BREAKING CHANGE: Add support for importing environment variables using
  `env:VAR` syntax
    * This is a breaking change to the Dhall library API since it adds a new
      `Path` constructor
    * This also technically a breaking change to the Dhall language but
      extremely unlikely to affect your program.  This only changes the behavior
      of old programs that had an expression of the form:

      ```
      env:VAR
      ```

      ... where `env` was the name of a bound variable and `:VAR` was a type
      annotation without spaces around the type annotation operator

      After this change the program would be interpreted as an import of the
      contents for the environment variable named `VAR`
* BREAKING CHANGE: Support importing paths relative to home directory using
  `~/some/path` syntax
    * This is a breaking change to the Dhall library API since it adds a new
      field to the `File` constructor indicating whether or not the imported
      path is relative to the home directory
    * This is not a breaking change to the Dhall language and the new syntax
      does not override any old syntax
* Permit trailing commas and bars in record/union syntax
* Improve location information for parsing errors

1.1.0

* BREAKING CHANGE: Non-empty lists no longer require a type annotation
    * This is a breaking change to the Haskell library, not the Dhall language
    * This change does not break existing Dhall programs
    * The `Expr` type was modified in a non-backwards-compatible way
* Add new `exprA` parser
* Add new `InvalidType` exception if `input` fails on an invalid `Type`
* Improve documentation and tutorial

1.0.2

* Add support for Nix-style "double single-quote" multi-line string literals
* Add `isNormalized`
* Improve documentation and tutorial
* Build against wider range of `http-client` versions

1.0.1

* Initial release

1.0.0

* Accidental premature upload to Hackage.  This release was blacklisted
