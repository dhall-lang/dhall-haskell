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

    # We can re-enable these once https://github.com/NixOS/nixpkgs/issues/133271
    # is fixed and Nixpkgs is upgraded to a version that incorporates GHCJS

    inherit (shared_ghcjs) dhall-try;
    dhall-ghcjs = shared_ghcjs.dhall;
  }
