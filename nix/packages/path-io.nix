{ mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, path, stdenv, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.6.0";
  sha256 = "73e420ad029626182ad56c200640500e67fdb40b939c6e6e2a15363879ef8d41";
  revision = "3";
  editedCabalFile = "0rd7svl3jxzqnf8l2h4f7xwlv8av67y85bwmr40954disq714l74";
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
