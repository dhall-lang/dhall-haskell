1.0.0

* [Generate documentation from header comment](https://github.com/dhall-lang/dhall-haskell/pull/1929)
    * `dhall-docs` will now render a module header from a comment at
      the beginning of a Dhall file
    * The comment syntax is essentially markdown with the same whitespace
      and indentation rules as multi-line strings in Dhall
    * Both block comments and multiple single-line comments are supported
* [Jump to imports](https://github.com/dhall-lang/dhall-haskell/pull/1959)
    * The documentation will now generate links for relative and remote imports found within the
      rendered source code
* [Add support for non-`let` type annotations](https://github.com/dhall-lang/dhall-haskell/pull/1928)
    * `dhall-docs` can now also extract the type of a module from a type
      annotation on a bare expression (i.e. an expression that is not a
      `let` expression)

0.0.1

* Initial beta release
