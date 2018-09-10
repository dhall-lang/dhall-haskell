let
  default = (import ./default.nix);

in
  { dhall = default.all;

    inherit (default) tarball;

    coverage = default.dhall;
  }
