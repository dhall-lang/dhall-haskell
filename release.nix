let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall =
            pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./default.nix { })
            ;

          megaparsec = haskellPackagesNew.callPackage ./megaparsec.nix { };

          parser-combinators = haskellPackagesNew.callPackage ./parser-combinators.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall;
  }
