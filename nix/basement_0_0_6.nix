{ mkDerivation, base, ghc-prim, stdenv }:
mkDerivation {
  pname = "basement";
  version = "0.0.6";
  sha256 = "9ca23b940006d8c6a7bc9c07c4ef1bf5ddb47ce82d384c5f341996e22cb95ff7";
  revision = "1";
  editedCabalFile = "0jlj6jy1fsh5xc3z1finjxsq838n3v7qz4zv344l37s1w9z8pwlf";
  libraryHaskellDepends = [ base ghc-prim ];
  homepage = "https://github.com/haskell-foundation/foundation";
  description = "Foundation scrap box of array & string";
  license = stdenv.lib.licenses.bsd3;
}
