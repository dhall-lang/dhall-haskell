{ mkDerivation, base, bytestring, containers, QuickCheck, stdenv
, template-haskell, text, th-lift, transformers, vector
}:
mkDerivation {
  pname = "th-lift-instances";
  version = "0.1.13";
  sha256 = "4ecf55e742f0e40ad915ee26dbea19cc7320452a9b217d490af1393a52f9b07e";
  libraryHaskellDepends = [
    base bytestring containers template-haskell text th-lift
    transformers vector
  ];
  testHaskellDepends = [
    base bytestring containers QuickCheck template-haskell text vector
  ];
  homepage = "http://github.com/bennofs/th-lift-instances/";
  description = "Lift instances for template-haskell for common data types";
  license = stdenv.lib.licenses.bsd3;
}
