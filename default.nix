let
  shared = import ./nix/shared.nix {};

in
  { inherit (shared) dhall dhall-bash dhall-json dhall-text; }
