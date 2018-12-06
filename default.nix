let
  shared = import ./nix/shared.nix {};

  shared_ghcjs = import ./nix/shared.nix { compiler = "ghcjs"; };

in
  { inherit (shared) dhall dhall-bash dhall-json dhall-text dhall-try;

    inherit (shared_ghcjs) try-dhall try-dhall-www;
  }
