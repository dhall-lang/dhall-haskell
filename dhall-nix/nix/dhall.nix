{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, containers, contravariant, criterion, cryptonite, deepseq, Diff
, directory, doctest, exceptions, filepath, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, memory, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, repline, scientific
, stdenv, tasty, tasty-hunit, template-haskell, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.15.0";
  sha256 = "dabb3023924657156224ef1a1326408c099c431e53b059ae74b9bc427396e4dc";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive containers
    contravariant cryptonite Diff directory exceptions filepath
    haskeline http-client http-client-tls insert-ordered-containers
    lens-family-core megaparsec memory mtl optparse-applicative parsers
    prettyprinter prettyprinter-ansi-terminal repline scientific
    template-haskell text transformers unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base deepseq Diff doctest insert-ordered-containers prettyprinter
    tasty tasty-hunit text vector
  ];
  benchmarkHaskellDepends = [
    base containers criterion directory text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
