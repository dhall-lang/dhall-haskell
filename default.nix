let
  shared = import ./nix/shared.nix {};

  shared_ghcjs = import ./nix/shared.nix { compiler = "ghcjs"; };

  shared_ghcjs_linux =
    import ./nix/shared.nix { compiler = "ghcjs"; system = "x86_64-linux"; };

in
  { inherit (shared) dhall dhall-bash dhall-json dhall-text;

    inherit (shared_ghcjs) dhall-try;

    inherit (shared_ghcjs_linux) try-dhall;
  }
