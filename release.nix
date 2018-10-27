let
  shared_7_10_3 =
    import ./nix/shared.nix { compiler = "ghc7103"; coverage = true; };

  shared_8_4_3 =
    import ./nix/shared.nix { compiler = "ghc843"; coverage = true; };

in
  { dhall = shared_8_4_3.aggregate
      { name = "dhall";

        constituents = [
          shared_7_10_3.dhall
          shared_8_4_3.dhall
          shared_8_4_3.dhall-bash
          shared_8_4_3.dhall-json
          shared_8_4_3.dhall-text
          shared_8_4_3.tarball-dhall
          shared_8_4_3.tarball-dhall-bash
          shared_8_4_3.tarball-dhall-json
          shared_8_4_3.tarball-dhall-text
          shared_8_4_3.pwd
        ];
      };

    "coverage-dhall" = shared_8_4_3.dhall;

    inherit (shared_8_4_3) tarball-dhall tarball-dhall-bash tarball-dhall-json tarball-dhall-text;
  }
