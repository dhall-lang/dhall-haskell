# `dhall-try`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md#build-the-website)

## How to contribute

You will most likely want to edit [`index.html`](./index.html) if you want to
improve the site.  The vast majority of the site logic is embedded within that
monolithic document, including a substantial amount of inline JavaScript, inline
CSS, and all of the code examples.

The [`src`](./src) directory contains the code for interpreting the live code
demo, powered by the `dhall`/`dhall-json` packages compiled to JavaScript using
GHCJS.  You only need to modify that Haskell source code if you would like to
extend the site with new Haskell-derived functionality.

The [`website.nix`](../nix/website.nix) file contains the top-level logic for
building the site, including bundling of JavaScript/CSS/image assets.  You will
also want to refer to [`shared.nix`](../nix/shared.nix) for related logic to
build each bundled dependency.
