let
  default_7_10_3 = import ./default.nix { compiler = "ghc7103"; };

  default_8_4_3 = import ./default.nix { compiler = "ghc843"; };

in
  { dhall = default_8_4_3.aggregate
      { name = "dhall";

        constituents = [
          default_7_10_3.dhall
          default_8_4_3.dhall
          default_8_4_3.tarball
          default_8_4_3.pwd
        ];
      };

    "coverage" = default_8_4_3.dhall;

    inherit (default_8_4_3) tarball;
  }
