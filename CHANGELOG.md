HEAD

* Add the instance `Interpret String`.
    * See: https://github.com/dhall-lang/dhall-haskell/pull/247

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
