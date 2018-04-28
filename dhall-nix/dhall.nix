{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, containers, contravariant, cryptonite, deepseq, directory
, exceptions, filepath, formatting, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, memory, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, repline, scientific
, stdenv, tasty, tasty-hunit, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.13.0";
  sha256 = "64bb773a0f64bc40b267f1000ea0279e1d2264841fccaba444888afd45f4c3ba";
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
    base deepseq insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
