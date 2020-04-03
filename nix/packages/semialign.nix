{ mkDerivation, base, base-compat, containers, hashable
, semigroupoids, stdenv, tagged, these, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign";
  version = "1.1";
  sha256 = "f3e218bf7fb3ea8145dbf1051e3460e99a0d4064f0e76238595f996858e287d8";
  libraryHaskellDepends = [
    base base-compat containers hashable semigroupoids tagged these
    transformers unordered-containers vector
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "Align and Zip type-classes from the common Semialign ancestor";
  license = stdenv.lib.licenses.bsd3;
}
