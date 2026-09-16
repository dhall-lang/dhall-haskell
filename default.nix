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
      dhall-openapi
      dhall-nix
      dhall-nixpkgs
      dhall-toml
      dhall-yaml
    ;
  }
