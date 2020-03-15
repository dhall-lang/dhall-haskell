{ mkDerivation, async, base, bytestring, containers, deepseq
, directory, filepath, mtl, optparse-applicative, process, stdenv
, tagged, tasty, tasty-hunit, temporary
}:
mkDerivation {
  pname = "tasty-golden";
  version = "2.3.3";
  sha256 = "1d993d43527b29297846934b455a9bdd533ec896c1ffe92eeba2aa40384c531f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers deepseq directory filepath mtl
    optparse-applicative process tagged tasty temporary
  ];
  testHaskellDepends = [
    base directory filepath process tasty tasty-hunit temporary
  ];
  homepage = "https://github.com/feuerbach/tasty-golden";
  description = "Golden tests support for tasty";
  license = stdenv.lib.licenses.mit;
}
