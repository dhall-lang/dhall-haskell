let
  shared = import ./nix/shared.nix {};

  shared_ghcjs = import ./nix/shared.nix { compiler = "ghcjs"; };

in
  { inherit (shared.possibly-static)
      dhall
      dhall-bash
      dhall-docs
      dhall-json
      dhall-lsp-server
      dhall-nix
      dhall-yaml
    ;

    inherit (shared_ghcjs) dhall-try;
  }
