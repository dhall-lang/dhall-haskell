{ mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.5.6";
  sha256 = "8c9689b51aa44b8278a5ff3059e0e8a609dce077df3781aad977c647a8c18a46";
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
