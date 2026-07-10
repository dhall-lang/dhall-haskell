{ mkDerivation, base, lib }:
mkDerivation {
  pname = "microlens";
  version = "0.4.14.0";
  sha256 = "56792a613e4fb0634bdedf54dcb773ac4baae8be0fec8bd88e1bb8b7a649922e";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/stevenfontanella/microlens";
  description = "A tiny lens library with no dependencies";
  license = lib.licenses.bsd3;
}
