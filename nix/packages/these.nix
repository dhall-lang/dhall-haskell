{ mkDerivation, assoc, base, base-compat, binary, deepseq, hashable
, stdenv
}:
mkDerivation {
  pname = "these";
  version = "1.1";
  sha256 = "bb0beb19e4f2722e09d02413f657c456c490ea9b256b6c9f167645a8b84cb137";
  libraryHaskellDepends = [
    assoc base base-compat binary deepseq hashable
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "An either-or-both data type";
  license = stdenv.lib.licenses.bsd3;
}
