# You can build this repository using Nix by running:
#
#     $ nix-build release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs:
      let
        parser-combinators =
          { mkDerivation, base, stdenv }:
          mkDerivation {
            pname = "parser-combinators";
            version = "0.4.0";
            src = pkgs.fetchurl {
              sha256 = "1azkz0a6ikym02s8wydjcklp7rz8k512bs4s9lp9g1g03m0yj95i";
              url = https://hackage.haskell.org/package/parser-combinators-0.4.0/parser-combinators-0.4.0.tar.gz;
            };
            libraryHaskellDepends = [ base ];
            homepage = "https://github.com/mrkkrp/parser-combinators";
            description = "Lightweight package providing commonly useful parser combinators";
            license = stdenv.lib.licenses.bsd3;
          };
      in
        {
          haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: {
              dhall = haskellPackagesNew.callPackage ./default.nix { };

              parser-combinators =
                haskellPackagesOld.callPackage parser-combinators {};

              megaparsec =
                pkgs.haskell.lib.overrideCabal haskellPackagesNew.megaparsec_6_1_1 (drv: {
                  version = "6.4.0";
                  src = pkgs.fetchurl {
                    url = https://hackage.haskell.org/package/megaparsec-6.4.0/megaparsec-6.4.0.tar.gz;
                    sha256 = "0h9azhs0dfrc359vrbd1jljrg3yfdbwd4p62cxqkn7mnh8913jpd";
                  };
                });
            };
          };
        };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { dhall = pkgs.haskellPackages.dhall;
  }
