let
  shared = import ./nix/shared.nix {};

  shared_ghcjs = import ./nix/shared.nix { compiler = "ghcjs"; };

in
  { inherit (shared) dhall dhall-bash dhall-json dhall-text;

    inherit (shared_ghcjs) dhall-try try-dhall-server try-dhall-static;
  }
