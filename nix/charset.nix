{ mkDerivation, array, base, bytestring, containers, semigroups
, stdenv, unordered-containers
}:
mkDerivation {
  pname = "charset";
  version = "0.3.7.1";
  sha256 = "3d415d2883bd7bf0cc9f038e8323f19c71e07dd12a3c712f449ccb8b4daac0be";
  revision = "1";
  editedCabalFile = "1z6nxw2g9vgsjq0g159sk8mwj68lwzxzi5iv5ynha0h85jcqxszy";
  libraryHaskellDepends = [
    array base bytestring containers semigroups unordered-containers
  ];
  homepage = "http://github.com/ekmett/charset";
  description = "Fast unicode character sets based on complemented PATRICIA tries";
  license = stdenv.lib.licenses.bsd3;
}
