{ stdenv, dhall-docs, unzip, fetchurl }:

stdenv.mkDerivation rec {
  name = "dhall-docs-artifacts";

  src = fetchurl {
    url = "https://github.com/dhall-lang/dhall-lang/archive/v17.0.0.zip";
    sha256 = "8c46f81d6131bf5dac715a3f107d0a43f4e8decaadd7ed36fbc4375b5b32eeaf";
  };

  buildInputs = [ dhall-docs unzip ];

  installPhase = ''
    mkdir -p $out
    dhall-docs --input ./Prelude --output $out/nix-support/hydra-build-products/Prelude > $out/dhall-docs.log
  '';
}

