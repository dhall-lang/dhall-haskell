{ stdenv, dhall-docs, unzip, fetchurl }:

stdenv.mkDerivation rec {
  name = "dhall-docs-artifacts";

  src = fetchurl {
    url = "https://github.com/dhall-lang/dhall-lang/archive/v17.0.0.zip";
    sha256 = "8c46f81d6131bf5dac715a3f107d0a43f4e8decaadd7ed36fbc4375b5b32eeaf";
  };

  buildInputs = [ dhall-docs unzip ];

  installPhase = ''
    mkdir -p $out/nix-support
    dhall-docs --input ./Prelude --output $out/docs > $out/dhall-docs.log
    echo "report html $out/docs/index.html" >> $out/nix-support/hydra-build-products
    echo "report log $out/dhall-docs.log" >> $out/nix-support/hydra-build-products
  '';
}

