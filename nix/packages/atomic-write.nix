{ mkDerivation, base, bytestring, directory, filepath, hspec
, stdenv, temporary, text, unix-compat
}:
mkDerivation {
  pname = "atomic-write";
  version = "0.2.0.7";
  sha256 = "b5f5c77884bc0332306fab89acf1c8a8582d76eabaa303c91b1c4072621c960d";
  libraryHaskellDepends = [
    base bytestring directory filepath temporary text unix-compat
  ];
  testHaskellDepends = [
    base bytestring filepath hspec temporary text unix-compat
  ];
  homepage = "https://github.com/stackbuilders/atomic-write";
  description = "Atomically write to a file";
  license = stdenv.lib.licenses.mit;
}
