{ mkDerivation, base, bytestring, containers, deepseq, mtl, parsec
, QuickCheck, stdenv, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.2.0.0";
  sha256 = "1862d7860d6a8255c92c7aa081d4946b0e838907b2d12730b96aba8d95cd1ff5";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq mtl parsec text
  ];
  testHaskellDepends = [
    base bytestring containers mtl QuickCheck tasty tasty-quickcheck
    text
  ];
  homepage = "https://github.com/hvr/HsYAML";
  description = "Pure Haskell YAML 1.2 processor";
  license = stdenv.lib.licenses.gpl2;
}
