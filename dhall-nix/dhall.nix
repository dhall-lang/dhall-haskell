{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base16-bytestring, bytestring, case-insensitive, containers
, contravariant, cryptonite, deepseq, directory, exceptions
, filepath, haskeline, http-client, http-client-tls
, insert-ordered-containers, lens-family-core, memory, mtl
, optparse-generic, parsers, prettyprinter
, prettyprinter-ansi-terminal, repline, scientific, stdenv, tasty
, tasty-hunit, text, text-format, transformers, trifecta
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.11.0";
  sha256 = "cec6e5f28d9329b5cddd54f271013e2d73d8d48a5b8faf7d62d2c87d9254d9ed";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring case-insensitive
    containers contravariant cryptonite directory exceptions filepath
    http-client http-client-tls insert-ordered-containers
    lens-family-core memory parsers prettyprinter
    prettyprinter-ansi-terminal scientific text text-format
    transformers trifecta unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base haskeline mtl optparse-generic prettyprinter
    prettyprinter-ansi-terminal repline text trifecta
  ];
  testHaskellDepends = [
    base deepseq insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
