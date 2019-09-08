{ mkDerivation, base, bytestring, containers, fetchgit, mtl, parsec
, QuickCheck, stdenv, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/haskell-hvr/HsYAML.git";
    sha256 = "1vyi3s105j8vxrqvp7ynxyafvkzaa3j152pch96pns13193laac5";
    rev = "b6a721514b4c0e562007322f064eedc431f751f9";
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
  description = "Pure Haskell YAML 1.2 processor";
  license = stdenv.lib.licenses.gpl2;
}
