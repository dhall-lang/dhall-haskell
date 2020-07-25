let
  pkgs = import ../nix/nixpkgs.nix;

in
  pkgs.haskellPackages.dhall-kubernetes-generator.env
