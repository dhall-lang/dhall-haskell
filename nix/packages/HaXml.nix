{ mkDerivation, base, bytestring, containers, directory, filepath
, polyparse, pretty, random, stdenv
}:
mkDerivation {
  pname = "HaXml";
  version = "1.25.5";
  sha256 = "cbc51ac4b6128e130f0272a7b42ab464bc865b3c238d6cce6b76e451765c1235";
  revision = "2";
  editedCabalFile = "0vlczcac2is5dbvkcwbsry1i10pbh1r316n1sq2py35alw7kzp1j";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers filepath polyparse pretty random
  ];
  executableHaskellDepends = [ base directory polyparse pretty ];
  homepage = "http://projects.haskell.org/HaXml/";
  description = "Utilities for manipulating XML documents";
  license = "LGPL";
}
