{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parser-combinators";
  version = "0.4.0";
  sha256 = "b124e9411de085972e4d9ae8254299e8e773e964b2798eb400d5cf6814f8f3ab";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
