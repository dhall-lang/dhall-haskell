{ mkDerivation, base, filepath, stdenv, time, unix }:
mkDerivation {
  pname = "directory";
  version = "1.3.2.0";
  sha256 = "de76ab83e4c2df85aecb1d02c9a57e9712d1ce751c5064e3e5a5e7fea255d039";
  libraryHaskellDepends = [ base filepath time unix ];
  testHaskellDepends = [ base filepath time unix ];
  description = "Platform-agnostic library for filesystem operations";
  license = stdenv.lib.licenses.bsd3;
}
