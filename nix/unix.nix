{ mkDerivation, base, bytestring, stdenv, time }:
mkDerivation {
  pname = "unix";
  version = "2.7.2.2";
  sha256 = "98dd4eb1b28d65bb57f42acbe22076930c0ad5947f3c1459ab7b15abd57cdeac";
  revision = "2";
  editedCabalFile = "0d6dv944rp8g69p336b1ik9xl1f182jd8lz82ykhfjhasw8d1waf";
  libraryHaskellDepends = [ base bytestring time ];
  homepage = "https://github.com/haskell/unix";
  description = "POSIX functionality";
  license = stdenv.lib.licenses.bsd3;
}
