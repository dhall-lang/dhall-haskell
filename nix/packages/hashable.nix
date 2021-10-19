{ mkDerivation, base, bytestring, containers, deepseq, ghc-prim
, HUnit, integer-gmp, lib, QuickCheck, random, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.3.4.1";
  sha256 = "2ca5fdf03a54718d02f02c9c9ab619ccf10fa8bcf3b0ff15b27edc26019a3196";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim integer-gmp text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}
