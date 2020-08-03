{ stdenv, dhall-docs, unzip, fetchurl, tree, src, packageDir ? "./", name }:

stdenv.mkDerivation rec {
  inherit src name;

  buildInputs = [ dhall-docs unzip tree ];

  installPhase = ''
    mkdir -p $out/nix-support
    export XDG_DATA_HOME=$out/
    dhall-docs --input ${packageDir} --output-link ./dhall-docs > $out/dhall-docs.log
    htmlDir=$(readlink -f ./dhall-docs)
    tree $XDG_DATA_HOME/dhall-docs > $out/dhall-docs.log
    echo "report html $htmlDir" >> $out/nix-support/hydra-build-products
    echo "report log $out/dhall-docs.log" >> $out/nix-support/hydra-build-products
  '';
}

