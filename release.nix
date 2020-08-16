let pinned = import ./nix/pinnedNixpkgs.nix;

in { src ? { rev = ""; }
   , nixpkgs ? pinned.nixpkgs
   }:

let
  callShared = args: import ./nix/shared.nix ({ inherit nixpkgs; } // args);

  shared_8_6_1 = callShared { compiler = "ghc861"; };

  shared_ghcjs = callShared { compiler = "ghcjs"; };

  shared = callShared { };

  shared_linux = callShared { system = "x86_64-linux"; };

  coverage = callShared { coverage = true; };

in
  { dhall = shared.aggregate
      { name = "dhall";

        constituents = [
          shared_ghcjs.dhall-try

          shared.tarball-dhall
          shared.tarball-dhall-bash
          shared.tarball-dhall-docs
          shared.tarball-dhall-json
          shared.tarball-dhall-lsp-server
          shared.tarball-dhall-nix
          shared.tarball-dhall-nixpkgs
          shared.tarball-dhall-openapi
          shared.tarball-dhall-yaml

          # These are the only `dhall` builds that run the test suite in CI
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
      tarball-dhall-docs
      tarball-dhall-json
      tarball-dhall-lsp-server
      tarball-dhall-nix
      tarball-dhall-nixpkgs
      tarball-dhall-openapi
      tarball-dhall-yaml

      prelude-dhall-docs
      test-dhall-docs
#     kubernetes-dhall-docs
    ;

    linux-dhall            = shared_linux.possibly-static.dhall;
    linux-dhall-bash       = shared_linux.possibly-static.dhall-bash;
    linux-dhall-docs       = shared_linux.possibly-static.dhall-docs;
    linux-dhall-json       = shared_linux.possibly-static.dhall-json;
    linux-dhall-lsp-server = shared_linux.possibly-static.dhall-lsp-server;
    linux-dhall-nix        = shared_linux.possibly-static.dhall-nix;
    linux-dhall-nixpkgs    = shared_linux.possibly-static.dhall-nixpkgs;
    linux-dhall-openapi    = shared_linux.possibly-static.dhall-openapi;
    linux-dhall-yaml       = shared_linux.possibly-static.dhall-yaml;

    inherit (shared_linux)
      image-dhall
      image-dhall-bash
      image-dhall-docs
      image-dhall-json
      image-dhall-lsp-server
      image-dhall-nix
      image-dhall-nixpkgs
      image-dhall-openapi
      image-dhall-yaml
    ;
  }
