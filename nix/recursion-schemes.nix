{ mkDerivation, base, base-orphans, comonad, free, HUnit, stdenv
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "recursion-schemes";
  version = "5.1.3";
  sha256 = "b21736e54b1d5fb0149624e95f2d8d8fd3413bb0972eda6a70e5753d3c9f1528";
  revision = "1";
  editedCabalFile = "0zxcmga4fy42arbyv1kzbyfnsghnll3prkpva58x654x4cg4diwk";
  libraryHaskellDepends = [
    base base-orphans comonad free template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base HUnit template-haskell transformers ];
  homepage = "http://github.com/ekmett/recursion-schemes/";
  description = "Representing common recursion patterns as higher-order functions";
  license = stdenv.lib.licenses.bsd2;
}
