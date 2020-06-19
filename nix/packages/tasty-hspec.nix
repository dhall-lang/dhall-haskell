{ mkDerivation, base, hspec, hspec-core, QuickCheck, stdenv, tasty
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "tasty-hspec";
  version = "1.1.5.1";
  sha256 = "fe889ec0f7b3991c46a07d9ff9cf09608a73a18f434a7480d2a09c79e56f3345";
  revision = "6";
  editedCabalFile = "0xa7h0p5r41m2a3l5r9ggmm4bc2a6wzgb4qvcqfl0dd2yb922bkz";
  libraryHaskellDepends = [
    base hspec hspec-core QuickCheck tasty tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/mitchellwrosen/tasty-hspec";
  description = "Hspec support for the Tasty test framework";
  license = stdenv.lib.licenses.bsd3;
}
