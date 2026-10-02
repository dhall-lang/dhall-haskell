# Dhall language support in VSCode/ium

The Dhall language integration consists of the following parts:
- The VSCode/ium plugin "Dhall Language Support" *([vscode-language-dhall](https://github.com/dhall-lang/vscode-language-dhall))* adds syntax highlighting for Dhall files.
- The VSCode/ium plugin "Dhall LSP Server" *([vscode-dhall-lsp-server](https://github.com/dhall-lang/vscode-dhall-lsp-server))* implements the LSP client &ndash; yes, there is a naming issue here &ndash; that communicates with the backend via the [LSP protocol](https://microsoft.github.io/language-server-protocol/specification) to provide advanced language features like error diagnostics or type information, etc.
- [*dhall-lsp-server*](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-lsp-server), which is part of the [*dhall-haskell*](https://github.com/dhall-lang/dhall-haskell) project, implements the actual LSP server (i.e. the backend). Any editor that speaks LSP can use it. The VS Code extension in [vscode-dhall-lsp-server](https://github.com/dhall-lang/vscode-dhall-lsp-server) is one client; Neovim, Emacs, Helix, Zed and Sublime are others. Point the client at the `dhall-lsp-server` executable. Semantic tokens and folding appear only when that client requests them.

# Installation

The "official" releases can be installed as follows:

- **vscode-language-dhall** should be installed directly from VSCode/ium via the extensions marketplace.
- **vscode-dhall-lsp-server** can also be installed directly from the marketplace.
- **dhall-lsp-server** can be installed from hackage with `cabal install dhall-lsp-server`. See the
[`dhall-haskell` `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md) for pre-built binaries, as well as comprehensive installation and development instructions using *cabal*, *stack* or *nix*.

## Installing the latest development versions

**Note&nbsp;** The versions of *vscode-dhall-lsp-server* and *dhall-lsp-server* need not necessarily match: an older client version will simply not expose all commands available in the backend, while an older server might not implement all commands exposed in the UI.

**vscode-dhall-lsp-server**
1. You need to have *npm* installed (e.g. using your favourite package manager).
2. Install the *typescript* compiler with `npm install -g typescript`. I recommend running `npm config set prefix '~/.local'` first to have npm install the executable in `~/.local/bin`; this avoids having to use *sudo* and polluting the rest of the system.
2. Check out a copy of the vscode-dhall-lsp-server repository into the VSCode/ium extensions folder
   ```
   git clone git@github.com:dhall-lang/vscode-dhall-lsp-server.git ~/.vscode-oss/extensions/vscode-dhall-lsp-server
   ```
   (replace `~/.vscode-oss/` with `~/.vscode/` if you use VSCode instead of VSCodium).
3. Run the remaining commands in the checked-out directory
   ```
   cd ~/.vscode-oss/extensions/vscode-dhall-lsp-server
   ```
4. Run `npm install` to fetch all library dependencies.
5. Run `npm run compile` to compile the typescript code to javascript.
6. Start (restart) VSCode/ium.

**dhall-lsp-server&nbsp;**
For detailed instructions as well as instructions using cabal or nix, see [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md). To install dhall-lsp-server using *stack*:
1. Clone `git@github.com:dhall-lang/dhall-haskell.git`.
2. Inside the checked out repository run `stack install dhall-lsp-server`.


# Usage / Features

The server speaks standard LSP, so Neovim, Emacs, Helix, Zed and Sublime can use it as well as VS Code. A client only shows a feature when it requests that method. Semantic tokens and folding ranges need a client that asks for them (the VS Code extension does after `vscode-languageclient` 8). Definition, references, rename, symbols and code actions work with older clients. Opening a non-file import needs a client filesystem for the `dhall-import` scheme, described under Imports.

- **Diagnostics&nbsp;**
The file is parsed and typechecked when you open it, when you save it, and shortly after you stop typing. Every failed import is reported, including failures inside an imported file. You can hover over the offending code to see the error message. A later syntax error does not hide an earlier type error whose code is unchanged. An unused `let` is marked unnecessary on the binding's name.

- **Go to definition, references, highlight and rename&nbsp;**
Names bound by `let`, lambda, `forall` and record fields resolve in the file, including `x@n`. A field of an imported value, such as `Lib.customFunction`, opens that import at the field. A local import opens the file itself. A remote import, an environment import, or a hash read from the semantic cache opens a read-only `dhall-import:///<name>.dhall` view. When this session has no source text for that hash, the view is the alpha-beta-normal form decoded from the cache CBOR. Relative imports inside a remote view resolve from that URL. Rename edits each of those sites in the current file.

- **Symbols, folding and semantic tokens&nbsp;**
Document symbols list the bindings. Folding ranges cover a multi-line `let` value (`let a = …` collapses so the next `let` or `in` stays visible), including later bindings in a `let x = … let y = … in …` chain. Lambda, function type, record, union, non-empty list, `if`, `merge`, or text literal also fold, one range per start line. Record folds end at the closing `}`, so a `.field` on the same line is not part of the fold. Semantic tokens mark name declarations and uses. If the buffer has a syntax error, navigation keeps using the last successful parse.

- **Code actions&nbsp;**
Quick Fix lists an edit only when it applies to the selection. "Normalize selection" and "Extract let" are offered when the selection parses as an expression. Normalize replaces that selection with its normal form, using the enclosing `let` bindings even when the file as a whole does not type-check, and is refused when the form is larger than `maxOutputSize` (default 16KiB) or the evaluation limit is hit. An empty selection is reported as empty. Extract let lifts the selection into `let extracted = … in extracted`. "Explain error" is a Quick Fix offered for a type error or a parse error: for the diagnostic under the cursor, even with an empty selection, or when the selection meets the error's range. It opens the explanation with `window/showDocument` at a `dhall-explain:` URI, which the VS Code client serves from memory when it registers that scheme. A failed assertion names both sides in the diagnostic; an assertion whose sides have different types names both types. Each side is cut at 2048 characters. "Remove unused let" deletes the unused binding under the cursor. The commands `dhall.server.lint`, `dhall.server.annotateLet`, `dhall.server.freezeImport` and `dhall.server.freezeAllImports` stay available from the editor.

- **Imports&nbsp;**
Each `dhall-import:` view is also written under `$XDG_CACHE_HOME/dhall-lsp/sources/<name>.dhall`. The server returns those bytes from the custom request `dhall/importSource`. VS Code can open the view only when the extension registers a read-only filesystem for the `dhall-import` scheme and lists that scheme in its document selector. The marketplace extension does not register that filesystem, so Go to Definition fails with `Unable to resolve resource dhall-import:/<name>.dhall`. "Show original source" is the command `dhall.server.showOriginalSource` and fetches only when you run it.

- **Output size&nbsp;**
`vscode-dhall-lsp-server.maxOutputSize` is the maximum rendered size of a normal form the server will show, in bytes. The default is 16384 (16KiB). It does not change ordinary evaluation outside the server.

- **Clickable imports&nbsp;**
Local and remote imports are underlined and clickable. If the buffer has a syntax error, the links come from the last successful parse, the same fallback navigation uses. A parse failure is not logged as a document-link error.

- **Type on hover&nbsp;**
You can hover over any part of the code and it will tell you the type of the subexpression at that point &ndash; if you highlight an identifier you see its type; if you highlight the `->` in a function you will see the type of the entire function. Hover reports a type when that subexpression can still be typed, even if another `let` or an import in the file fails. It does not repeat the diagnostic already shown by the editor. A missing import's diagnostic text is cut at 2048 characters. If the buffer has a syntax error, hover uses the last successful parse when the hovered slice is unchanged.

- **Code completion&nbsp;**
As you type you will be offered completions for:
  - environment variables
  - local imports
  - identifiers in scope (as well as built-ins)
  - record projections from 'easy-to-parse' records (of the form `ident1.ident2`[`.ident3...`])
  - union constructors from 'easy-to-parse' unions
  - fields and constructors after an expression, such as `(f x).` or `{ a = 1 }.`, when the file typechecks once the dot is removed. Completions come from the type (or from a union expression that is already a union), without normalizing the expression

  This is the only feature that works even when the file does not parse (or typecheck).

- **Formatting and Linting&nbsp;**
Right click and select "Format Document" to run the file through the Dhall formatter. The command "Lint and Format" can be selected via the *Command Palette* (Ctrl+Shift+P); this will run the linter over the file, removing unused let bindings and formatting the result.

The default formatting behavior is to infer the character set used in the file (Unicode/ASCII operators).
This can be overriden by using the Dhall LSP settings. For example in VS Code's `settings.json`:
  - `"vscode-dhall-lsp-server.character-set": "ascii"` to always format using the ASCII character set
  - `"vscode-dhall-lsp-server.character-set": "unicode"` to always format using the Unicode character set

- **Annotate lets&nbsp;**
Right-click the bound identifier in a `let` binding and select "Annotate Let binding with its type" to do exactly that.

- **Inline let&nbsp;**
On a `let` binder, "Inline let" replaces each use with the bound value, adding parentheses when the value contains a space, and deletes the binding. It is refused when a binder between the `let` and a use would capture a name from the value, when the body uses `name@n`, or when the value contains `assert`. The editor lists it in the lightbulb (Quick Fix) and under Refactor ▸ Inline.

- **Organize imports&nbsp;**
"Organize imports" walks every `let` in the file, including nested scopes. It keeps bindings whose value is a bare import (`let x = ./path` or `let x = https://…`, not `let x = 1 + ./path`), drops unused ones, and hoists the rest to a top-level multi-let sorted by name, headed by `-- Imports.`. Existing comments at the top of the file are left in place and `-- Imports.` is inserted after them. A bare import cannot mention bound variables, so the move is semantics-preserving when names do not clash. The cursor does not have to sit on an import. The action is offered as both a Quick Fix and a Source Action (`source.organizeImports`, Shift+Alt+O). It is listed as disabled when two import lets share a name, when hoisting would be captured by another binder of the same name, or when the imports are already organized.

- **Freeze imports&nbsp;**
Right-click an import statement and select "Freeze (refreeze) import" to add (or update) a semantic hash annotation to the import. You can also select "Freeze (refreeze) all imports" from the *Command Palette* to freeze all imports at once. With the cursor on an import, Quick Fix also offers "Freeze import", "Unfreeze import" and "Unfreeze all imports". Unfreeze deletes the hash. A `missing` import is left unchanged, because without its hash it always fails. Ordinary analysis does not normalize a hashed import to check its hash. "Check import hash", offered on a hashed import, does that check and reports whether the annotation matches.

  Note that this feature behaves slightly differently from the `dhall freeze` command in that the hash annotations are inserted without re-formatting the rest of the code!

# Developer notes

**dhall-lsp-server&nbsp;**
See [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md) for general development instructions.
- You can also build using `stack build dhall-lsp-server` and point `vscode-dhall-lsp-server.executable` in the VSCode/ium settings to the stack build directory to avoid overriding the already installed version of the LSP server.
- You can use standard `Debug.Trace`/`putStrLn` debugging; the output will show up in the "Output" panel in VSCode/ium.
- To log all LSP communication set `vscode-dhall-lsp-server.trace.server` to `verbose` in VSCode/ium.

**vscode-dhall-lsp-server**
- Instead of working in `~/vscode-oss/extensions/...` directly, you can open a clone of the git repository in VSCode/ium and use the built-in debugging capabilities for extensions: press F5 (or click the green play button in the debugging tab) to launch a new VSCode/ium window with the modified extension (potentially shadowing the installed version).
- To package a release:
  1. Make sure you have *npm* and *tsc* installed.
  2. Use `npm install -g vsce` to install the *vsce* executable.
  3. Run `vsce package` inside the git repo to package the extension, resulting in a file `vscode-dhall-lsp-server-x.x.x.vsix`.
  4. You can install the packaged extension directly by opening the `.vsix` file from within VSCod/ium.
  
**Integration tests**

The `dhall-lsp-server:tests` testsuite depends on the `dhall-lsp-server` executable. Since `stack` isn't aware of this dependency, `stack test dhall-lsp-server:tests` may use an old executable version. Run these tests with

    stack test dhall-lsp-server:tests dhall-lsp-server:dhall-lsp-server
    
to ensure that the executable is up-to-date.
