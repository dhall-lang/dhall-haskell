{ mkDerivation, aeson, base, bytestring, fetchgit, stdenv
, string-qq, tasty, tasty-discover, tasty-hunit, text
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "aeson-yaml";
  version = "1.0.6.0";
  src = fetchgit {
    url = "https://github.com/clovyr/aeson-yaml.git";
    sha256 = "1bp4biap1rwak2fc7xb7qmh77hp1i3g0mlg66gw96ljkpi16vqk8";
    rev = "3e697cfc99bedfc7e1d1f3d4b738d6d774a804b2";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring string-qq tasty tasty-discover tasty-hunit
    unordered-containers yaml
  ];
  testToolDepends = [ tasty-discover ];
  homepage = "https://github.com/clovyr/aeson-yaml";
  description = "Output any Aeson value as YAML (pure Haskell library)";
  license = stdenv.lib.licenses.bsd3;
}
