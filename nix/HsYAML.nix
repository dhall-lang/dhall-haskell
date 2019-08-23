{ mkDerivation, base, bytestring, containers, fetchgit, mtl, parsec
, QuickCheck, stdenv, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/vijayphoenix/HsYAML.git";
    sha256 = "0szh12avk9jr0rlqyxf8idc3n7dll6cppnpa54p4f73w1hbwazw1";
    rev = "9354f53c69e9178a8e7c5c17b47137e23d4179df";
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
