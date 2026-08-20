let
  shared = import ./nix/shared.nix {};

in
  { inherit (shared.possibly-static)
      dhall
      dhall-bash
      dhall-csv
      dhall-docs
      dhall-json
      dhall-lsp-server
      dhall-nix
      dhall-nixpkgs
      dhall-openapi
      dhall-toml
      dhall-yaml
    ;

    # GHC's JavaScript backend via pkgsCross.ghcjs. Nested so a bare
    # `nix-build` does not compile JS. See nix/javascript.nix.
    javascript = import ./nix/javascript.nix {};
  }
