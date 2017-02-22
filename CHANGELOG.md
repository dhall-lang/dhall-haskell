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
