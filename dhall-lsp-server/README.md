
# dhall-lsp-server
[![Travis](https://travis-ci.org/PanAeon/dhall-lsp-server.svg?branch=master)](https://travis-ci.org/PanAeon/dhall-lsp-server)

**This project is in alpha state !!!**

This is a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server implementation for the [Dhall](https://dhall-lang.org) programming language.


## Installation

### From source

[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) should be installed. 

```bash
cd ./dhall-lsp-server
stack install
```

Stack will copy executables to the current user's executable directory. On macOS this is `/Users/<username>/.local/bin`. On linux this should be `/Home/<username>/.local/bin`.
If you are using VSCode there's also an option in the [VSCode Dhall plugin](https://github.com/PanAeon/vscode-dhall-lsp-server) to specify the path to the executable directly, which might be useful if you have multiple executables or you can't use global PATH for some reason.


