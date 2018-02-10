{ mkDerivation, ansi-wl-pprint, base, base16-bytestring, bytestring
, case-insensitive, charset, containers, contravariant, cryptohash
, deepseq, exceptions, http-client, http-client-tls, lens
, optparse-generic, parsers, prettyprinter, stdenv, system-fileio
, system-filepath, tasty, tasty-hunit, text, text-format
, transformers, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.9.0";
  sha256 = "5fc53a49c48014ec07a889abcd31b79b43c95d5f36ff494cfa33e229e5cea9c4";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring case-insensitive
    charset containers contravariant cryptohash exceptions http-client
    http-client-tls lens parsers prettyprinter system-fileio
    system-filepath text text-format transformers trifecta
    unordered-containers vector
  ];
  executableHaskellDepends = [
    base optparse-generic prettyprinter system-filepath text trifecta
  ];
  testHaskellDepends = [
    base containers deepseq prettyprinter tasty tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
