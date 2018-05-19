{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, containers, contravariant, cryptonite, deepseq, directory
, doctest, exceptions, filepath, formatting, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, memory, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, repline, scientific
, stdenv, tasty, tasty-hunit, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.14.0";
  sha256 = "f415842889ca4811f5279714446bad35583829de3c6de04e0d7d3e92f310a836";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive containers
    contravariant cryptonite directory exceptions filepath formatting
    http-client http-client-tls insert-ordered-containers
    lens-family-core megaparsec memory parsers prettyprinter
    prettyprinter-ansi-terminal scientific text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base haskeline megaparsec mtl optparse-applicative
    prettyprinter prettyprinter-ansi-terminal repline text
  ];
  testHaskellDepends = [
    base deepseq doctest insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
