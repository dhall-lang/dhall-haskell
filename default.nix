let
  shared = import ./nix/shared.nix {};

  shared_ghcjs = import ./nix/shared.nix { compiler = "ghcjs"; };

in
  { inherit (shared.possibly-static)
      dhall
      dhall-bash
      dhall-csv
      dhall-docs
      dhall-json
      dhall-lsp-server
      dhall-openapi
      dhall-nix
      dhall-nixpkgs
      dhall-toml
      dhall-yaml
    ;

    # Legacy GHCJS 8.10 (`haskell.packages.ghcjs`) was removed in Nixpkgs 25.11.
    # The JavaScript backend now lives under `pkgsCross.ghcjs`; restoring
    # `dhall-try` is a separate change that ports `dhall/ghcjs-src` and
    # `dhall-try` off `ghcjs-base`.

    # inherit (shared_ghcjs) dhall-try;
    # dhall-ghcjs = shared_ghcjs.dhall;
  }
