{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, http-client, http-client-tls, microlens, microlens-mtl
, neat-interpolation, optparse-generic, parsers, stdenv
, system-fileio, system-filepath, text, text-format, transformers
, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring containers http-client
    http-client-tls microlens microlens-mtl neat-interpolation parsers
    system-fileio system-filepath text text-format transformers
    trifecta unordered-containers vector
  ];
  executableHaskellDepends = [ base optparse-generic text trifecta ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
