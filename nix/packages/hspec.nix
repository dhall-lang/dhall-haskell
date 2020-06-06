{ mkDerivation, base, hspec-core, hspec-discover
, hspec-expectations, QuickCheck, stdenv
}:
mkDerivation {
  pname = "hspec";
  version = "2.7.1";
  sha256 = "818cebbcbde8761289902a816f865691e40724babf87e70057ecab204f6619f5";
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}
