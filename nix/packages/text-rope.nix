{ mkDerivation, base, deepseq, lib, random, tasty, tasty-bench
, tasty-quickcheck, text, vector
}:
mkDerivation {
  pname = "text-rope";
  version = "0.2";
  sha256 = "b14422855e9cc375a31c22758454013ac629671331de6ccb77b496509bcae78b";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base deepseq text vector ];
  testHaskellDepends = [ base tasty tasty-quickcheck text ];
  benchmarkHaskellDepends = [ base random tasty-bench text ];
  homepage = "https://github.com/Bodigrim/text-rope";
  description = "Text lines and ropes";
  license = lib.licenses.bsd3;
}
