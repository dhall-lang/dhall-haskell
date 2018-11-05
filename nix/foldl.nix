{ mkDerivation, base, bytestring, comonad, containers
, contravariant, criterion, hashable, mwc-random, primitive
, profunctors, semigroupoids, semigroups, stdenv, text
, transformers, unordered-containers, vector, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.4.5";
  sha256 = "0ba0bd8a8b4273feef61b66b6e251e70f70537c113f8b7f0e3aeab77d8af12a7";
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors semigroupoids semigroups text
    transformers unordered-containers vector vector-builder
  ];
  benchmarkHaskellDepends = [ base criterion ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
