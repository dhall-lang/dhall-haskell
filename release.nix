let
  default_7_10_3 = import ./default.nix { compiler = "ghc7103"; };

  default_8_4_3 = import ./default.nix { compiler = "ghc843"; };

in
  { dhall = default_8_4_3.all;

    "coverage-8.4.3" = default_8_4_3.dhall;

    "coverage-7.10.3" = default_7_10_3.dhall;

    inherit (default_8_4_3) tarball;
  }
