{ src ? { rev = ""; }, ... }:

let
  shared_7_10_3 =
    import ./nix/shared.nix { compiler = "ghc7103"; };

  shared_8_6_1 =
    import ./nix/shared.nix { compiler = "ghc861"; };

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
          shared_8_6_1.dhall
          shared_8_6_1.dhall-bash
          shared_8_6_1.dhall-json
          # `base-noprelude` depends on a specific version of `base`
          # shared_8_6_1.dhall-lsp-server
          # `hnix` depends on `unix-2.7.*` and doesn't work with GHC 8.6
          # shared_8_6_1.dhall-nix
          shared_8_6_1.dhall-text

          # However, we still use GHC 8.4.3 to build the distributed tarballs
          # due to a bug in GHC 8.6.1.  See:
          #
          # https://ghc.haskell.org/trac/ghc/ticket/15696
          shared.tarball-dhall
          shared.tarball-dhall-bash
          shared.tarball-dhall-json
          shared.tarball-dhall-lsp-server
          shared.tarball-dhall-nix
          shared.tarball-dhall-text

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

    inherit (shared_linux)
      image-dhall
      image-dhall-bash
      image-dhall-json
      image-dhall-lsp-server
      image-dhall-nix
      image-dhall-text
    ;
  }
