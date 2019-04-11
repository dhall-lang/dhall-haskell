{ mkDerivation, base, base16-bytestring, bytestring, criterion, SHA
, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha512";
  version = "0.11.100.1";
  sha256 = "10698bb9575eaa414a65d9644caa9408f9276c63447406e0a4faef91db1071a9";
  revision = "3";
  editedCabalFile = "19m1fp0i7ba84aa72d5wf59c7j0p4yr1bc43in8pspgywhsr3lfl";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha512";
  description = "Fast, pure and practical SHA-512 implementation";
  license = stdenv.lib.licenses.bsd3;
}
