# `dhall-try`

In-browser Dhall interpreter used by [try.dhall-lang.org](https://try.dhall-lang.org).
It is compiled with **GHC's JavaScript backend** (`pkgsCross.ghcjs` on Nixpkgs
26.05), not legacy GHCJS 8.10 (`haskell.packages.ghcjs`, removed in Nixpkgs
25.11).

## Build with Nix

The JS products are **not** part of a bare `nix-build` at the repo root (so a
JS failure cannot block native tarballs).  Build them explicitly:

```console
$ nix-build nix/javascript.nix -A dhall-try
```

Equivalent:

```console
$ nix-build -A javascript.dhall-try
$ nix-build dhall-try
```

The first build of GHC's JS backend and its package set is large and often
uncached (hours).  Linux (`x86_64-linux`) is the well-supported host; Darwin
may work but is less tested.

The minified interpreter lands at:

```
result/bin/dhall-try.jsexe/all.min.js
```

(`all.js` is the unminified GHC output; `all.min.js` is Closure Compiler.)

To open a shell with the JS GHC and `dhall-try` dependencies:

```console
$ nix-shell dhall-try/shell.nix
```

Do **not** pass `compiler = "ghcjs"` to `nix/shared.nix` on this Nixpkgs pin:
that attribute throws.  The JS package set is `pkgs.pkgsCross.ghcjs.haskell.packages.ghc96`.

## How the JS sources are selected

- `dhall/dhall.cabal` uses `ghcjs-src` when `arch(javascript)`, `impl(ghcjs)`,
  or `-fjavascript` is set.  The flag exists because `cabal2nix` evaluates the
  Cabal file on the **build** platform, not the JS target.
- HTTP uses `fetch`; SHA-256 uses Web Crypto (browser) or Node `crypto`.
- `dhall-try` talks to the Ace editor via `GHC.JS.Prim`.  GHC 9.6's JS `base`
  has no `GHC.JS.Foreign.Callback` (that landed in 9.8), so `src/Callback.hs`
  wraps the same RTS helpers (`h$makeCallback` / `h$run`).  There is no
  `ghcjs-base` / `ghcjs-xhr` dependency.

## How to contribute

You will most likely want to edit [`index.html`](./index.html) if you want to
improve the site.  The vast majority of the site logic is embedded within that
monolithic document, including a substantial amount of inline JavaScript, inline
CSS, and all of the code examples.

The [`src`](./src) directory contains the Haskell for the live demo
(`dhall` / `dhall-json` compiled to JavaScript).  Change it when you need new
Haskell-derived behaviour.

Website assembly for dhall-lang.org still lives in the `dhall-lang` repository
(`nixops/website.nix`).  After this interpreter builds, that overlay should
stop using `dhall-haskell-old.json` (commit `8f62fdf`, August 2021) and take
`dhall-try` from current `dhall-haskell`.

For installation or development of the **native** packages, see the
[`dhall-haskell` README](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md).
