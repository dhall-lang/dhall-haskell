{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "type-equality";
  version = "1";
  sha256 = "4728b502a211454ef682a10d7a3e817c22d06ba509df114bb267ef9d43a08ce8";
  revision = "1";
  editedCabalFile = "13lsff17dxz852f5bhjz8d1by704rzvwr67qqfc5dz5s7xc28qyk";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/hesselink/type-equality";
  description = "Data.Type.Equality compat package";
  license = stdenv.lib.licenses.bsd3;
}
