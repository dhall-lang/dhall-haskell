let
  default = (import ./default.nix);

in
  { dhall-json = default.all;

    inherit (default) tarball;
  }
