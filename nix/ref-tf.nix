{ mkDerivation, base, stdenv, stm, transformers }:
mkDerivation {
  pname = "ref-tf";
  version = "0.4.0.1";
  sha256 = "fcb522c5dca437fbd0c0132c56664a71c48fe2c06b150fcfa77d3bad5ce4be0e";
  revision = "1";
  editedCabalFile = "042nn6y3rbx9z88bkidy1ilp32grm6a1n0ny1wrzxdp46xi5r7in";
  libraryHaskellDepends = [ base stm transformers ];
  description = "A type class for monads with references using type families";
  license = stdenv.lib.licenses.bsd3;
}
