{ mkDerivation, base, stdenv, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.10.5";
  sha256 = "990aea21568956d44ab018c5dbfbaea014b9a0d5295d29ca7550149419a6fb41";
  libraryHaskellDepends = [ base unix ];
  description = "A compatibility layer for base";
  license = stdenv.lib.licenses.mit;
}
