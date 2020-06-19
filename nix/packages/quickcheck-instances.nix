{ mkDerivation, array, base, base-compat, bytestring
, case-insensitive, containers, hashable, old-time, QuickCheck
, scientific, splitmix, stdenv, tagged, text, these, time
, time-compat, transformers, transformers-compat
, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.23";
  sha256 = "11303b91bae2ecc1569a34e3922b06d0ca4593e30654067e7b172afd782b2fbe";
  revision = "1";
  editedCabalFile = "1lir5ryv2b1hn5n5fbgs9syram71zv4p4chb9xzkxvbszl5inw8k";
  libraryHaskellDepends = [
    array base base-compat bytestring case-insensitive containers
    hashable old-time QuickCheck scientific splitmix tagged text these
    time time-compat transformers transformers-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  benchmarkHaskellDepends = [ base bytestring QuickCheck ];
  homepage = "https://github.com/phadej/qc-instances";
  description = "Common quickcheck instances";
  license = stdenv.lib.licenses.bsd3;
}
