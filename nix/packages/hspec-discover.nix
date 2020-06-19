{ mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.7.1";
  sha256 = "0b31c29b37d7d446d44b3559f794cd62b09ee5fc3f30862eccd8284e52758764";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = stdenv.lib.licenses.mit;
}
