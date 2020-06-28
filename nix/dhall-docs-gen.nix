{ stdenv, dhall-docs, unzip, fetchurl, tree }:

stdenv.mkDerivation rec {
  name = "dhall-docs-artifacts";

  src = fetchurl {
    url = "https://github.com/german1608/dhall-lang/archive/feat/add-dhall-extensions-to-prelude.zip";
    sha256 = "ade9544719a26b7f3bf008572a794cb3878f284429d425a6c6a2e31c61189619";
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

