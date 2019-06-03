# `dhall-lsp-server`

This is a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server implementation for the [Dhall](https://dhall-lang.org) programming language; in order to use it you need to have a corresponding *language server client* installed in your target editor. For the time being the only supported editor is VSCode with the [vscode-dhall-lsp-server](https://github.com/PanAeon/vscode-dhall-lsp-server) client, but in the future we hope to support all mainstream editors.


For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Features
* Underlines (type/syntax/...) errors in the source code (error diagnostics).
* Exposes a formatting command (equivalent to running `dhall format`).

## Roadmap (TBD)
* Expose a linting command that removes unused let bindings etc. (cf. `dhall lint`).
* Suggest source code improvements to the user based on the linter, i.e. underline unused let bindings etc.
* Display type information on hovering over an identifier.
* Expose a command that annotates a let binding with its inferred type.

For further insights into the ongoing development have a look at the corresponding [GSoC proposal](https://eggbaconandspam.github.io/gsoc-proposal.pdf).
