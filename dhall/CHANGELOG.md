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
