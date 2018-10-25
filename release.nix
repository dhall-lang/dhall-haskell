let
  shared_7_10_3 =
    import ./shared.nix { compiler = "ghc7103"; coverage = true; };

  shared_8_4_3 =
    import ./shared.nix { compiler = "ghc843"; coverage = true; };

in
  { dhall = shared_8_4_3.aggregate
      { name = "dhall";

        constituents = [
          shared_7_10_3.dhall
          shared_8_4_3.dhall
          shared_8_4_3.tarball
          shared_8_4_3.pwd
        ];
      };

    "coverage" = shared_8_4_3.dhall;

    inherit (shared_8_4_3) tarball;
  }
