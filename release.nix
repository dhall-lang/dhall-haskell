let
  shared_7_10_3 =
    import ./nix/shared.nix { compiler = "ghc7103"; };

  shared =
    import ./nix/shared.nix { };

  coverage =
    import ./nix/shared.nix { coverage = true; };

in
  { dhall = shared.aggregate
      { name = "dhall";

        constituents = [
          shared_7_10_3.dhall
          shared.dhall
          shared.dhall-bash
          shared.dhall-json
          shared.dhall-text
          shared.tarball-dhall
          shared.tarball-dhall-bash
          shared.tarball-dhall-json
          shared.tarball-dhall-text
          shared.pwd
        ];
      };

    "coverage-dhall" = coverage.dhall;

    inherit (shared) tarball-dhall tarball-dhall-bash tarball-dhall-json tarball-dhall-text;
  }
