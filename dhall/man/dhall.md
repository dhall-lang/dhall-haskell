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

  dhall diff <(cat file1.dhall) <(cat file2.dhall)

  dhall type -\- quiet -\-file check-ci.dhall

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

Editor integration for Vim, Emacs, Atom, and VS Code is available at:

https://github.com/vmchale/dhall-vim

https://github.com/psibi/dhall-mode

https://github.com/jmitchell/atom-language-dhall

https://github.com/dhall-lang/vscode-language-dhall

# BUGS

Please report any bugs you may come across to
https://github.com/dhall-language/dhall-haskell/issues.
