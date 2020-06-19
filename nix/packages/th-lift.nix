{ mkDerivation, base, ghc-prim, stdenv, template-haskell
, th-abstraction
}:
mkDerivation {
  pname = "th-lift";
  version = "0.8.0.1";
  sha256 = "a05133d8eac584fe47d8ff02163bb86193ce1f5de325ba73c98e95f0daa2d8a8";
  libraryHaskellDepends = [
    base ghc-prim template-haskell th-abstraction
  ];
  testHaskellDepends = [ base ghc-prim template-haskell ];
  homepage = "http://github.com/mboes/th-lift";
  description = "Derive Template Haskell's Lift class for datatypes";
  license = stdenv.lib.licenses.bsd3;
}
