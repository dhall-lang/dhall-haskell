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
