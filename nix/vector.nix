{ mkDerivation, base, base-orphans, deepseq, ghc-prim, HUnit
, primitive, QuickCheck, random, stdenv, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.12.0.2";
  sha256 = "52e89dacaff10bedb8653181963cae928f9674a099bb706713dae83994bbc0f3";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base base-orphans HUnit primitive QuickCheck random
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}
