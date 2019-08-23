{ mkDerivation, base, bytestring, containers, fetchgit, mtl, parsec
, QuickCheck, stdenv, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/vijayphoenix/HsYAML.git";
    sha256 = "0k1mw6j78d4lhbgqnc6fz0dlly0m6sb72f7z9yh82jw0ml7l9srr";
    rev = "4485eab8b1c9b6286e19b92159ac6279921bdde1";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers mtl parsec text
  ];
  testHaskellDepends = [
    base bytestring containers mtl QuickCheck tasty tasty-quickcheck
    text
  ];
  homepage = "https://github.com/hvr/HsYAML";
  description = "Pure Haskell YAML 1.2 parsing and encoding library";
  license = stdenv.lib.licenses.gpl2;
}
