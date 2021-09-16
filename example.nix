
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

  xxx =
    { dhallToNixUrlFOD, fetchurl, runCommand, dhall }:
    {
      dependencies = [ "console" "effect" "prelude" "psci-support" ];
      name = "my-project";
      packages =
        dhallToNixUrlFOD (
          let
            packageSet =
              (fetchurl {
                url = "https://gist.githubusercontent.com/cdepillabout/2683131f078753fd24723ab8bf1e1b74/raw/0de0be4b0238974a33aac07580338105fa5c42e1/example-remote-import.dhall";
                hash = "sha256-W1WfXQXIvufW57F5N7fxqRvi/YMBdvlnwlFn4eMyVLc=";
                downloadToTemp = true;
                postFetch = ''
                  env | sort
                  ${dhall}/bin/dhall --alpha --plain --file "$downloadedFile" | ${dhall}/bin/dhall encode > $out
                '';
              }).overrideAttrs (oldAttrs: {
                SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
                NIX_SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
                nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [ cacert ];
              });
          in runCommand "decodedPackageSet" {} "${dhall}/bin/dhall decode --file \"${packageSet}\" > \$out") {
        inherit dhallToNixUrlFOD fetchurl runCommand dhall;
      };
      sources = [ "src/**/*.purs" "test/**/*.purs" ];
    };

in

# dhallToNixUrlFOD ./example-purescript-package/spago2.dhall {
# dhallToNixUrlFOD ./example-spago2.dhall {
#   inherit dhallToNixUrlFOD fetchurl runCommand;
#   dhall = myDhall;
# }

xxx {
  inherit dhallToNixUrlFOD fetchurl runCommand;
  dhall = myDhall;
}
