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
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall;
  }
