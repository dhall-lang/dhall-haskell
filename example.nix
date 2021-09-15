with import <nixpkgs> {};


# spago:
# { name = "my-project"
# , dependencies = [ "console", "effect", "prelude", "psci-support" ]
# , packages = https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210905/packages.dhall sha256:140f3630801f2b02d5f3a405d4872e0af317e4ef187016a6b00f97d59d6275c6
# , sources = [ "src/**/*.purs", "test/**/*.purs" ]
# }

# turns into nix:
# {
#   dependencies = [ "console" "effect" "prelude" "psci-support" ];
#   name = "my-project";
#   packages = {};
#   packages =
#     dhallToNixUrlFOD (
#       let
#         packageSet = fetchurl {
#           url = "https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210905/packages.dhall";
#           hash = "sha256-FA82MIAfKwLV86QF1IcuCvMX5O8YcBamsA+X1Z1idcY=";
#           downloadToTemp = true;
#           postFetch = ''
#             ${dhall}/bin/dhall encode --file "$downloadedFile" > $out
#           '';
#         };
#       in
#         runCommand "decodedPackageSet" {} ''
#           ${dhall}/bin/dhall decode --file "${packageSet}" > $out
#         '';
#     )
#   sources = [ "src/**/*.purs" "test/**/*.purs" ];
# }

let
  _dhallToNixUrlFOD = dhallFile:
    let
      drv = stdenv.mkDerivation {
        name = "dhall-compiled.nix";

        buildCommand = ''
          dhall-to-nix <<< "${dhallFile}" > $out
        '';

        buildInputs = [ dhall-nix ];
      };
    in
      import drv;

  dhallToNixUrlFOD = _dhallToNixUrlFOD;
in

dhallToNixUrlFOD ~/temp/temp-purescript-pacakge/spago2.dhall
