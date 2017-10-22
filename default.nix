{ mkDerivation, ansi-wl-pprint, base, base16-bytestring, bytestring
, case-insensitive, charset, containers, contravariant, cryptohash
, exceptions, http-client, http-client-tls, lens, optparse-generic
, parsers, prettyprinter, stdenv, system-fileio, system-filepath
, tasty, tasty-hunit, text, text-format, transformers, trifecta
, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.7.0";
  src = ./.;
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
    base containers tasty tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
