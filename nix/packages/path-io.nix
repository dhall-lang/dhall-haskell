{ mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, path, stdenv, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.6.0";
  sha256 = "73e420ad029626182ad56c200640500e67fdb40b939c6e6e2a15363879ef8d41";
  revision = "1";
  editedCabalFile = "1kwrkpmwmar8nwaar02m3kfy24vl3kzm0m3iq0d4ryd84a6a0dax";
  libraryHaskellDepends = [
    base containers directory dlist exceptions filepath path temporary
    time transformers unix-compat
  ];
  testHaskellDepends = [
    base directory exceptions filepath hspec path transformers
    unix-compat
  ];
  homepage = "https://github.com/mrkkrp/path-io";
  description = "Interface to ‘directory’ package for users of ‘path’";
  license = stdenv.lib.licenses.bsd3;
}
