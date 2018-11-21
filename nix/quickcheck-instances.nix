{ mkDerivation, array, base, base-compat, bytestring
, case-insensitive, containers, hashable, old-time, QuickCheck
, scientific, stdenv, tagged, text, time, transformers
, transformers-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.19";
  sha256 = "57a4aefff05313fb07a651934088d18a584f8bcfeaa02305be65525f12409a56";
  libraryHaskellDepends = [
    array base base-compat bytestring case-insensitive containers
    hashable old-time QuickCheck scientific tagged text time
    transformers transformers-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  homepage = "https://github.com/phadej/qc-instances";
  description = "Common quickcheck instances";
  license = stdenv.lib.licenses.bsd3;
}
