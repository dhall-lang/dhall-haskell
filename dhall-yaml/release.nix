let
  default = (import ./default.nix);

in
  { dhall-yaml = default.all;

    inherit (default) tarball;
  }
