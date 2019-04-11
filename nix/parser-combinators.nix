{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "parser-combinators";
  version = "1.0.1";
  sha256 = "edf5ab8fa69a04334baa8707252036563a8339a96a86956c90febe93830cea32";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
