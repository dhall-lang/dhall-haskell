{ mkDerivation, base, bytestring, stdenv }:
mkDerivation {
  pname = "unix";
  version = "2.5.1.0";
  sha256 = "0743e0cf718baab88e37db061f0d0623864f83b6d32c6f3405f923b085f15737";
  revision = "1";
  editedCabalFile = "1j5z04sdvipf8rma5xma92q0qs2arkrr045y6v4fkc12rmpm6hbz";
  libraryHaskellDepends = [ base bytestring ];
  description = "POSIX functionality";
  license = stdenv.lib.licenses.bsd3;
}
