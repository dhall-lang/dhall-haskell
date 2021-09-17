
let
  nixpkgsSrc = builtins.fetchTarball {
    # nixos-unstable as of 2021-09-16.
    url = "https://github.com/NixOS/nixpkgs/archive/bcd607489d76795508c48261e1ad05f5d4b7672f.tar.gz";
    sha256 = "0yjp9lrhzvyh9dc4b9dl456fr6nlchfmn85adq0vi4pnwfmh90z6";
  };
in

with import nixpkgsSrc {};

let

  myDhall = haskellPackages.dhall_1_40_1;

  dhall-nix-url-fod = haskellPackages.callCabal2nix "dhall-nix" ./dhall-nix {
    dhall = myDhall;
  };

  _dhallToNixUrlFOD = dhallFile:
    let
      drv = stdenv.mkDerivation {
        name = "dhall-compiled.nix";

        buildCommand = ''
          dhall-to-nix < "${dhallFile}" > $out
        '';

        buildInputs = [ dhall-nix-url-fod ];
      };
    in
      import drv;

  dhallToNixUrlFOD = _dhallToNixUrlFOD;

in

dhallToNixUrlFOD ./example-purescript-package/spago2.dhall {
# dhallToNixUrlFOD ./example-spago2.dhall {
  inherit dhallToNixUrlFOD cacert fetchurl runCommand;
  dhall = myDhall;
}
