{ stdenv, dhall-docs, unzip, fetchurl, tree }:

stdenv.mkDerivation rec {
  name = "dhall-docs-artifacts";

  src = fetchurl {
    url = "https://github.com/german1608/dhall-lang/archive/feat/add-dhall-extensions-to-prelude.zip";
    sha256 = "8d179809150e545c5bd7192df37fc5a636c05e1f87f16350333c0d7efc70e238";
  };

  buildInputs = [ dhall-docs unzip tree ];

  installPhase = ''
    mkdir -p $out/nix-support
    export XDG_DATA_HOME=$out/
    dhall-docs --input ./Prelude --output-link ./dhall-docs > $out/dhall-docs.log
    htmlDir=$(readlink -f ./dhall-docs)
    tree $XDG_DATA_HOME/dhall-docs > $out/dhall-docs.log
    echo "report html $htmlDir" >> $out/nix-support/hydra-build-products
    echo "report log $out/dhall-docs.log" >> $out/nix-support/hydra-build-products
  '';
}

