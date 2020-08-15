{ mkDerivation, base, ghc-prim, stdenv }:
mkDerivation {
  pname = "basement";
  version = "0.0.11";
  sha256 = "67582b3475a5547925399f719df21f8bbbd0ca4d4db27795c22a474f8ee6346b";
  revision = "2";
  editedCabalFile = "1l95bzmn23cmx386hk3d3r0ykdaibh9rp489lcnba5g56kiy4hxg";
  libraryHaskellDepends = [ base ghc-prim ];
  homepage = "https://github.com/haskell-foundation/foundation#readme";
  description = "Foundation scrap box of array & string";
  license = stdenv.lib.licenses.bsd3;
}
