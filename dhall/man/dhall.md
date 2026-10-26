% dhall (1)

# NAME

dhall - a programmable configuration language

# DESCRIPTION

**dhall** is the command-line interface to the Dhall language

# SYNOPSIS

  dhall -\-file check.dhall

  dhall -\-file error.dhall -\-explain

  dhall hash -\-file to-hash.dhall

  dhall freeze -\-file with-imports.dhall

  dhall repl

  dhall diff \'./file1.dhall' \'./file2.dhall'

  dhall type -\-quiet -\-file check-ci.dhall

# SUBCOMMANDS

**version** - Display version information

**resolve** - Resolve imports

**type** - Infer type of expression

**normalize** - Normalize an expression

**repl** - Open a REPL

**diff** - Show difference between two expressions

**hash** - Compute hash of expression

**lint** - Lint an expression, removing dead code

**tags** - Generate etags (Emacs) file

**format** - Format an expression

**freeze** - Add hashes to import statements

**encode** - Encode a Dhall expression (CBOR)

**decode** - Decode a Dhall expression

**text** - Render a Dhall expression to text

**to-directory-tree** - Convert nested record of Text into a directory

# OPTIONS

**-h** **-\-help**
:   Display help

**-\-file**
:   Name of file containing Dhall source

**-\-output**
:   Output file

**-\-annotate**
:   Add type annotation to output

**-\-alpha**
:   alpha-normalize output

**-\-no-cache**
:   Don't use cache to resolve imports

**-\-no-remote**
:   Reject http and https imports used as code, as Text, or as Bytes, including integrity-checked cache hits. as Location, local files, and env imports are unchanged.

    This is not the same as compiling with `-f-with-http`. That Cabal flag removes the HTTP client. Nixpkgs uses it so a build can still resolve hashed URL imports from a semantic cache that Nix has already filled; a cache miss fails instead of opening a connection. `--no-remote` leaves the HTTP client linked and rejects those cache hits. as Location fetches nothing under either control.

**-\-explain**
:   Explain error messages in detail

**-\-version**
:   Display version information

**-\-plain**
:   Disable syntax highlighting in output

**-\-ascii**
:   Format code using only ASCII syntax

**-\-censor**
:   Hide source code from error messages

# EDITOR INTEGRATION

Up-to-date information on editor integration is available at:

https://docs.dhall-lang.org/howtos/Text-Editor-Configuration.html

# BUGS

Please report any bugs you may come across to
https://github.com/dhall-lang/dhall-haskell/issues.
