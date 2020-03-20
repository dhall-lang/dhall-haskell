{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, contravariant, deepseq
, Diff, directory, dlist, filepath, generic-deriving, ghc-prim
, hashable, hashable-time, integer-logarithms, primitive
, QuickCheck, quickcheck-instances, scientific, stdenv, tagged
, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, template-haskell, text, th-abstraction, time, time-compat
, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.6.0";
  sha256 = "923fb2c6e224c4c0d1848174b1010592f31cd149f538923efd87f8a6b4b3488b";
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers contravariant
    deepseq dlist ghc-prim hashable primitive scientific tagged
    template-haskell text th-abstraction time time-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers Diff directory dlist filepath
    generic-deriving ghc-prim hashable hashable-time integer-logarithms
    QuickCheck quickcheck-instances scientific tagged tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
