{ mkDerivation, ansi-wl-pprint, base, bytestring, charset
, containers, http-client, http-client-tls, lens
, neat-interpolation, optparse-generic, parsers, stdenv
, system-fileio, system-filepath, tasty, tasty-hunit, text
, text-format, transformers, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring charset containers http-client
    http-client-tls lens neat-interpolation parsers system-fileio
    system-filepath text text-format transformers trifecta
    unordered-containers vector
  ];
  executableHaskellDepends = [ base optparse-generic text trifecta ];
  testHaskellDepends = [
    base neat-interpolation tasty tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
