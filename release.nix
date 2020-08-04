let pinned = import ./nix/pinnedNixpkgs.nix;

in { src ? { rev = ""; }
, nixpkgs ? pinned.nixpkgs
, nixpkgsStaticLinux ? pinned.nixpkgsStaticLinux
}:

let
  callShared = args:
    import ./nix/shared.nix ({ inherit nixpkgs nixpkgsStaticLinux; } // args);

  shared_8_6_1 = callShared { compiler = "ghc861"; };

  shared_ghcjs = callShared { compiler = "ghcjs"; };

  shared = callShared { };

  shared_linux = callShared { system = "x86_64-linux"; };

  shared_linux_8_6_1 = callShared { compiler = "ghc861"; system = "x86_64-linux"; };

  coverage = callShared { coverage = true; };

in
  { dhall = shared.aggregate
      { name = "dhall";

        constituents = [
          # Verify that the packages build against the latest supported version
          # of the compiler
          shared_8_6_1.dhall
          shared_8_6_1.dhall-bash
          shared_8_6_1.dhall-docs
          shared_8_6_1.dhall-json
          shared_8_6_1.dhall-yaml
          # `base-noprelude` depends on a specific version of `base`
          # shared_8_6_1.dhall-lsp-server
          # `hnix` depends on `unix-2.7.*` and doesn't work with GHC 8.6
          # shared_8_6_1.dhall-nix
          # shared_8_6_1.dhall-nixpkgs

          shared_ghcjs.dhall-try

          # However, we still use GHC 8.4.3 to build the distributed tarballs
          # due to a bug in GHC 8.6.1.  See:
          #
          # https://ghc.haskell.org/trac/ghc/ticket/15696
          shared.tarball-dhall
          shared.tarball-dhall-bash
          # path-io >= 1.6.0 → directory >= 1.3.20 → ghc >= 8.6
          # see http://hackage.haskell.org/package/path-io-1.6.0/changelog`
          shared_8_6_1.tarball-dhall-docs
          shared.tarball-dhall-json
          shared.tarball-dhall-lsp-server
          shared.tarball-dhall-nix
          shared.tarball-dhall-nixpkgs
          shared.tarball-dhall-yaml

          # This is the only `dhall` build that runs the test suite
          coverage.dhall
          coverage.dhall-json
          coverage.dhall-yaml

          # Check that the package builds with HTTP support compiled out
          shared.dhall-no-http

          (shared.trivial src.rev)
        ];
      };

    "coverage-dhall" = coverage.dhall;

    inherit (shared)
      tarball-dhall
      tarball-dhall-bash
      tarball-dhall-json
      tarball-dhall-lsp-server
      tarball-dhall-nix
      tarball-dhall-nixpkgs
      tarball-dhall-yaml
    ;
    inherit (shared_8_6_1) tarball-dhall-docs;

    linux-dhall            = shared_linux.possibly-static.dhall;
    linux-dhall-docs       = shared_linux.possibly-static.dhall-docs;
    linux-dhall-json       = shared_linux.possibly-static.dhall-json;
    linux-dhall-lsp-server = shared_linux.possibly-static.dhall-lsp-server;
    linux-dhall-nix        = shared_linux.possibly-static.dhall-nix;
    linux-dhall-nixpkgs    = shared_linux.possibly-static.dhall-nixpkgs;
    linux-dhall-yaml       = shared_linux.possibly-static.dhall-yaml;
    linux-dhall-bash       = shared_linux_8_6_1.possibly-static.dhall-bash;

    inherit (shared_linux)
      image-dhall
      image-dhall-bash
      image-dhall-json
      image-dhall-lsp-server
      image-dhall-nix
      image-dhall-nixpkgs
      image-dhall-yaml
    ;
    inherit (shared_linux_8_6_1) image-dhall-docs;

    inherit (shared_8_6_1) generate-dhall-docs;
  }
