{ mkDerivation, ansi-terminal, base, base16-bytestring, bytestring
, case-insensitive, charset, containers, contravariant, cryptohash
, deepseq, directory, exceptions, filepath, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, mtl, optparse-generic, parsers, prettyprinter
, prettyprinter-ansi-terminal, repline, scientific, stdenv, tasty
, tasty-hunit, text, text-format, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.9.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base base16-bytestring bytestring case-insensitive
    charset containers contravariant cryptohash directory exceptions
    filepath http-client http-client-tls insert-ordered-containers
    lens-family-core megaparsec parsers prettyprinter
    prettyprinter-ansi-terminal scientific text text-format
    transformers unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base containers haskeline megaparsec mtl
    optparse-generic prettyprinter prettyprinter-ansi-terminal repline
    text
  ];
  testHaskellDepends = [
    base containers deepseq insert-ordered-containers prettyprinter
    tasty tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
