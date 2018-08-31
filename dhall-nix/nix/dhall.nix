{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, cborg, containers, contravariant, criterion, cryptonite, deepseq
, Diff, directory, doctest, exceptions, filepath, hashable
, haskeline, http-client, http-client-tls
, insert-ordered-containers, lens-family-core, megaparsec, memory
, mockery, mtl, optparse-applicative, parsers, prettyprinter
, prettyprinter-ansi-terminal, QuickCheck, quickcheck-instances
, repline, scientific, serialise, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.17.0";
  sha256 = "a029fea856224a79f0b7cc56fbb5c566d0dfd1d915d214f682006cabf1274791";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive cborg containers
    contravariant cryptonite Diff directory exceptions filepath
    hashable haskeline http-client http-client-tls
    insert-ordered-containers lens-family-core megaparsec memory mtl
    optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal repline scientific serialise
    template-haskell text transformers unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers deepseq directory doctest filepath hashable
    insert-ordered-containers mockery prettyprinter QuickCheck
    quickcheck-instances serialise tasty tasty-hunit tasty-quickcheck
    text transformers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion directory serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
