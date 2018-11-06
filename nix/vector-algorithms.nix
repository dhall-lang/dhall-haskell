{ mkDerivation, base, bytestring, containers, mwc-random, primitive
, QuickCheck, stdenv, vector
}:
mkDerivation {
  pname = "vector-algorithms";
  version = "0.8.0.1";
  sha256 = "15bcde786dcf03861946885e030d3dbe3b683e1a6fc12d7317e115084f4637fe";
  libraryHaskellDepends = [ base bytestring primitive vector ];
  testHaskellDepends = [
    base bytestring containers QuickCheck vector
  ];
  benchmarkHaskellDepends = [ base mwc-random vector ];
  homepage = "https://github.com/erikd/vector-algorithms/";
  description = "Efficient algorithms for vector arrays";
  license = stdenv.lib.licenses.bsd3;
}
