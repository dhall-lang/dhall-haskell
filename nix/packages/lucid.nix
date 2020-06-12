{ mkDerivation, base, bifunctors, blaze-builder, bytestring
, containers, criterion, deepseq, hashable, hspec, HUnit, mmorph
, mtl, parsec, stdenv, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "lucid";
  version = "2.9.12";
  sha256 = "304bc31b4b5d65b0e7bc4ad88ca2a2c84f64e92fa9aee7f3591486d67cb4dc94";
  revision = "1";
  editedCabalFile = "1f0whk5ncanxfjjanrf6rqyncig2xgc5mh2j0sqy3nrlyjr9aqq9";
  libraryHaskellDepends = [
    base blaze-builder bytestring containers hashable mmorph mtl text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base bifunctors hspec HUnit mtl parsec text
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring criterion deepseq text transformers
  ];
  homepage = "https://github.com/chrisdone/lucid";
  description = "Clear to write, read and edit DSL for HTML";
  license = stdenv.lib.licenses.bsd3;
}
