{ mkDerivation, base, containers, hashable, lens, semialign, stdenv
, these, unordered-containers, vector
}:
mkDerivation {
  pname = "semialign-indexed";
  version = "1.1";
  sha256 = "60f1dd3df6b1b1bf6d835209f55d4deedf0587a26a236e0a54c8a4c9a1abcaac";
  libraryHaskellDepends = [
    base containers hashable lens semialign these unordered-containers
    vector
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "SemialignWithIndex, i.e. izipWith and ialignWith";
  license = stdenv.lib.licenses.bsd3;
}
