{ mkDerivation, base, bytestring, directory, filepath, stdenv
, template-haskell
}:
mkDerivation {
  pname = "file-embed";
  version = "0.0.11";
  sha256 = "eea5d00973808e440f346972b7477c8d8c2194a7036cc532eafeffc5189fcd50";
  libraryHaskellDepends = [
    base bytestring directory filepath template-haskell
  ];
  testHaskellDepends = [ base filepath ];
  homepage = "https://github.com/snoyberg/file-embed";
  description = "Use Template Haskell to embed file contents directly";
  license = stdenv.lib.licenses.bsd3;
}
