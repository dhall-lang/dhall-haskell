# You can build this repository using Nix by running:
#
#     $ nix-build -A dhall-nix release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A dhall-nix.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall-nix = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { dhall-nix = pkgs.haskellPackages.dhall-nix;

    test =
      let
        dhallToNix = code :
          let
            file = builtins.toFile "dhall-expr" code;

            drv = pkgs.stdenv.mkDerivation {
              name = "dhall-expr-as-nix";

              buildCommand = ''
                dhall-to-nix <<< "${file}" > $out
              '';

              buildInputs = [ pkgs.haskellPackages.dhall-nix ];
            };

          in
            import "${drv}";

      in if (dhallToNix "{ foo = 1, bar = False}").bar
         then pkgs.haskellPackages.lens-family-core
         else pkgs.haskellPackages.pipes;
  }
