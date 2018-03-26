let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall =
            # Remove this once it is available in Nixpkgs 18.03
            let
              failOnAllWarnings =
                drv: pkgs.haskell.lib.appendConfigureFlag drv "--ghc-option=-Wall --ghc-option=-Werror";


            in
              failOnAllWarnings
                (pkgs.haskell.lib.justStaticExecutables
                  (haskellPackagesNew.callPackage ./default.nix { })
                );

          formatting =
            haskellPackagesNew.callPackage ./nix/formatting.nix { };

          prettyprinter =
            haskellPackagesNew.callPackage ./nix/prettyprinter.nix { };

          parser-combinators =
            haskellPackagesOld.callPackage ./nix/parser-combinators.nix {};

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

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall;
  }
