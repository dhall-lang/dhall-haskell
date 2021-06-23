1.2.7

* Build against `dhall-1.39.0`

1.2.6

* [Allow bytestring-0.11](https://github.com/dhall-lang/dhall-haskell/pull/2144)

1.2.5

* Build against `dhall-1.38.0`, `tasty-1.4`, and `tasty-silver-3.2`

1.2.4

* Build against `dhall-1.37.0`

1.2.3

* [Fix command-line completions for files](https://github.com/dhall-lang/dhall-haskell/pull/2034)

1.2.2

* [The `--documents` flag now wraps non-`List`s in a document](https://github.com/dhall-lang/dhall-haskell/pull/1977)

1.2.1

* [Format documents with leading `---`](https://github.com/dhall-lang/dhall-haskell/pull/1865)
    * Now if you use the `--documents` flag the first document will also
      include a leading `---`

1.2.0

* BREAKING CHANGE: [Add `--generated-comment` flag for `dhall-to-yaml{-ng}`](https://github.com/dhall-lang/dhall-haskell/pull/1840)
    * You can now optionally add a comment header to the YAML output
      indicating that the file is generated and should not be hand-edited
    * This is a breaking change because this adds a new `noEdit` field to the
      options type
    * In practice this breakage won't affect most users
* [Produce output compatible with YAML 1.1](https://github.com/dhall-lang/dhall-haskell/pull/1788)
    * Special strings like `on` are now quoted in order to avoid being
      misinterpreted as boolean values by YAML 1.1 implementations
* [Show JSON/YAML path on error reporting](https://github.com/dhall-lang/dhall-haskell/pull/1799)
    * Error messages will now include the path to the error in the diagnostic
      output

1.1.0

* BREAKING CHANGE: [Add `yaml-to-dhall` support for inferring the schema](https://github.com/dhall-lang/dhall-haskell/pull/1773)
    * You no longer need to provide the command with an explicit schema.  The
      command will infer a reasonably close schema from the provided YAML
    * This is a breaking change because the `schema` field of the `Options` type
      now has type `Maybe Text` instead of `Text`
* [Add `yaml-to-dhall type` subcommand](https://github.com/dhall-lang/dhall-haskell/pull/1776)
    * You can use this subcommand to print the inferred schema for a YAML value,
      so that you can edit the schema and use it for subsequent invocations.
* [Add `yaml-to-dhall` support for using `toMap`](https://github.com/dhall-lang/dhall-haskell/pull/1745)
    * Now if you specify a `Map` as the schema, the generated Dhall code will
      use `toMap` to improve the appearance

1.0.3

* [yaml: Single-quote date/bool string fields](https://github.com/dhall-lang/dhall-haskell/commits/master/dhall-json)

1.0.2

* Build against `dhall-1.30.0`

1.0.1

* Build against `dhall-1.29.0`

1.0.0

* Initial release
