{ mkDerivation, ansi-wl-pprint, base, base16-bytestring, bytestring
, case-insensitive, charset, containers, contravariant, cryptohash
, deepseq, directory, exceptions, filepath, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, megaparsec, optparse-generic, parsers, prettyprinter, scientific
, stdenv, system-filepath, tasty, tasty-hunit, text, text-format
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.9.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring case-insensitive
    charset containers contravariant cryptohash directory exceptions
    filepath http-client http-client-tls insert-ordered-containers
    lens-family-core megaparsec parsers prettyprinter scientific text
    text-format transformers unordered-containers vector
  ];
  executableHaskellDepends = [
    base megaparsec optparse-generic prettyprinter system-filepath text
  ];
  testHaskellDepends = [
    base containers deepseq insert-ordered-containers prettyprinter
    tasty tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
