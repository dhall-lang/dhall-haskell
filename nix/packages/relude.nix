{ mkDerivation, base, bytestring, containers, criterion, deepseq
, doctest, ghc-prim, Glob, hashable, hedgehog, lib, mtl, stm, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "relude";
  version = "1.0.0.1";
  sha256 = "4fdcfe29205ebe8a2b976339207cffeb5bede18f8d7b3332245ac5ed5e508933";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable mtl stm text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base bytestring containers doctest Glob hedgehog text
  ];
  benchmarkHaskellDepends = [ base criterion unordered-containers ];
  homepage = "https://github.com/kowainik/relude";
  description = "Safe, performant, user-friendly and lightweight Haskell Standard Library";
  license = lib.licenses.mit;
}
