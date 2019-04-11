{ mkDerivation, base, hspec, hspec-core, QuickCheck, stdenv, tasty
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "tasty-hspec";
  version = "1.1.5";
  sha256 = "db0cdcf71d534cfa32a1698f1eb6be03192af09a5dd63177b697bc4ca8b81154";
  revision = "3";
  editedCabalFile = "14198y7w9y4h36b6agzmsyappkhz4gmmi6nlzj137z5siwic7igm";
  libraryHaskellDepends = [
    base hspec hspec-core QuickCheck tasty tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/mitchellwrosen/tasty-hspec";
  description = "Hspec support for the Tasty test framework";
  license = stdenv.lib.licenses.bsd3;
}
