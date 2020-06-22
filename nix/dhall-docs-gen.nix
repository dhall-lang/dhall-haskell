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
    export XDG_DATA_HOME=$out/
    dhall-docs --input ./Prelude > $out/dhall-docs.log
    echo "report html $out/dhall-docs/5b8855b55ed149bcb1e6153c9c45c9be5d2c8e085b5bb3edf51398f326157b93-Prelude" >> $out/nix-support/hydra-build-products
    echo "report log $out/dhall-docs.log" >> $out/nix-support/hydra-build-products
  '';
}

