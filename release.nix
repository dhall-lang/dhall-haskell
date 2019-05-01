{ src ? { rev = ""; }, ... }:

let
  shared_7_10_3 =
    import ./nix/shared.nix { compiler = "ghc7103"; };

  shared_8_6_5 =
    import ./nix/shared.nix { compiler = "ghc865"; };

  shared_ghcjs =
    import ./nix/shared.nix { compiler = "ghcjs"; };

  shared =
    import ./nix/shared.nix { };

  shared_linux =
    import ./nix/shared.nix { system = "x86_64-linux"; };

  coverage =
    import ./nix/shared.nix { coverage = true; };

in
  { dhall = shared.aggregate
      { name = "dhall";

        constituents = [
          # Verify that the packages build against the oldest supported version
          # of the compiler
          shared_7_10_3.dhall
          shared_7_10_3.dhall-bash
          shared_7_10_3.dhall-json
          # `base-noprelude` depends on a specific version of `base`
          # shared_7_10_3.dhall-lsp-server
          # `hnix` depends on `lens-family-th`, which doesn't support GHC 7.10.3
          # shared_7_10_3.dhall-nix
          shared_7_10_3.dhall-text

          # Verify that the packages build against the latest supported version
          # of the compiler
          shared_8_6_5.dhall
          shared_8_6_5.dhall-bash
          shared_8_6_5.dhall-json
          # `base-noprelude` depends on a specific version of `base`
          # shared_8_6_5.dhall-lsp-server
          # `hnix` depends on `unix-2.7.*` and doesn't work with GHC 8.6
          # shared_8_6_5.dhall-nix
          shared_8_6_5.dhall-text

          shared_8_6_5.tarball-dhall
          shared_8_6_5.tarball-dhall-bash
          shared_8_6_5.tarball-dhall-json
          shared_8_6_5.tarball-dhall-lsp-server
          shared_8_6_5.tarball-dhall-nix
          shared_8_6_5.tarball-dhall-text

          shared_ghcjs.tarball-website

          # This is the only `dhall` build that runs the test suite
          coverage.dhall

          (shared.trivial src.rev)
        ];
      };

    "coverage-dhall" = coverage.dhall;

    inherit (shared_ghcjs) tarball-website;

    inherit (shared)
      tarball-dhall
      tarball-dhall-bash
      tarball-dhall-json
      tarball-dhall-lsp-server
      tarball-dhall-nix
      tarball-dhall-text
    ;

    linux-dhall            = shared_linux.possibly-static.dhall;
    linux-dhall-bash       = shared_linux.possibly-static.dhall-bash;
    linux-dhall-json       = shared_linux.possibly-static.dhall-json;
    linux-dhall-lsp-server = shared_linux.possibly-static.dhall-lsp-server;
    linux-dhall-nix        = shared_linux.possibly-static.dhall-nix;
    linux-dhall-text       = shared_linux.possibly-static.dhall-text;
  }
