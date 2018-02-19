let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall =
            pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./default.nix { })
            ;

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
