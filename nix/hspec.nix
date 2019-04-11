{ mkDerivation, base, hspec-core, hspec-discover
, hspec-expectations, QuickCheck, stdenv
}:
mkDerivation {
  pname = "hspec";
  version = "2.5.6";
  sha256 = "9ea6eb6ac6b49e1593e272707b760e125d3bdca2d8845d76e116c1ea8112da59";
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}
