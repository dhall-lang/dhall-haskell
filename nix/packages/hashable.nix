{ mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
, HUnit, integer-gmp, QuickCheck, random, siphash, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.2.7.0";
  sha256 = "ecb5efc0586023f5a0dc861100621c1dbb4cbb2f0516829a16ebac39f0432abf";
  revision = "1";
  editedCabalFile = "197063dpl0wn67dp7a06yc2hxp81n24ykk7klbjx0fndm5n87dh3";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq ghc-prim integer-gmp text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion ghc-prim integer-gmp siphash text
  ];
  homepage = "http://github.com/tibbe/hashable";
  description = "A class for types that can be converted to a hash value";
  license = stdenv.lib.licenses.bsd3;
}
