# You can build this repository using Nix by running:
#
#     $ nix-build -A dhall release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A dhall.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall = haskellPackagesNew.callPackage ./default.nix { };

          prettyprinter = haskellPackagesNew.callPackage ./prettyprinter.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { dhall = pkgs.haskellPackages.dhall;
  }
