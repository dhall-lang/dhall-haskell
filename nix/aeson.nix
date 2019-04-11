{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, contravariant, deepseq
, directory, dlist, filepath, generic-deriving, ghc-prim, hashable
, hashable-time, integer-logarithms, primitive, QuickCheck
, quickcheck-instances, scientific, stdenv, tagged, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, time, time-locale-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.2.0";
  sha256 = "75ce71814a33d5e5568208e6806a8847e7ba47fea74d30f6a8b1b56ecb318bd0";
  revision = "1";
  editedCabalFile = "067y82gq86740j2zj4y6v7z9b5860ncg2g9lfnrpsnb9jqm7arl1";
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers contravariant
    deepseq dlist ghc-prim hashable primitive scientific tagged
    template-haskell text th-abstraction time time-locale-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers directory dlist filepath generic-deriving
    ghc-prim hashable hashable-time integer-logarithms QuickCheck
    quickcheck-instances scientific tagged tasty tasty-hunit
    tasty-quickcheck template-haskell text time time-locale-compat
    unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
