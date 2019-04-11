{ mkDerivation, base, bytestring, containers, deepseq, doctest
, gauge, ghc-prim, Glob, hashable, hedgehog, mtl, QuickCheck
, stdenv, stm, tasty, tasty-hedgehog, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "relude";
  version = "0.5.0";
  sha256 = "75411b958121c813f4a0a2297542d4df85f141f52c1c081803fb48b73c691d81";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable mtl stm text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog QuickCheck tasty
    tasty-hedgehog text
  ];
  benchmarkHaskellDepends = [
    base containers gauge unordered-containers
  ];
  homepage = "https://github.com/kowainik/relude";
  description = "Custom prelude from Kowainik";
  license = stdenv.lib.licenses.mit;
}
