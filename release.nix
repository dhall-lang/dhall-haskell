let
  shared_7_10_3 =
    import ./nix/shared.nix { compiler = "ghc7103"; };

  shared_8_6_1 =
    import ./nix/shared.nix { compiler = "ghc861"; };

  shared_ghcjs =
    import ./nix/shared.nix { compiler = "ghcjs"; };

  shared =
    import ./nix/shared.nix { };

  coverage =
    import ./nix/shared.nix { coverage = true; };

in
  { dhall = shared.aggregate
      { name = "dhall";

        constituents = [
          # Eta only requires the `dhall` package to build using GHC 7.10.3.
          # This is why we don't need to test the other `dhall-*` packages on
          # GHC 7.10.3
          shared_7_10_3.dhall

          # Verify that the packages build against the latest version of the
          # compiler
          shared_8_6_1.dhall
          shared_8_6_1.dhall
          shared_8_6_1.dhall-bash
          shared_8_6_1.dhall-json
          shared_8_6_1.dhall-text

          # However, we still use GHC 8.4.3 to build the distributed tarballs
          # due to a bug in GHC 8.6.1.  See:
          #
          # https://ghc.haskell.org/trac/ghc/ticket/15696
          shared.tarball-dhall
          shared.tarball-dhall-bash
          shared.tarball-dhall-json
          shared.tarball-dhall-text

          # Verify that `try-dhall` server and dependencies can be built, which
          # includes building `dhall` using GHCJS
          shared_ghcjs.try-dhall

          # This is the only `dhall` build that runs the test suite
          coverage.dhall

          shared.pwd
        ];
      };

    "coverage-dhall" = coverage.dhall;

    inherit (shared)
      tarball-dhall
      tarball-dhall-bash
      tarball-dhall-json
      tarball-dhall-text
    ;
  }
