{ mkDerivation, aeson, assoc, base, base-compat, binary, deepseq
, hashable, QuickCheck, semigroupoids, stdenv, unordered-containers
}:
mkDerivation {
  pname = "these";
  version = "1.0.1";
  sha256 = "79cc1ee35d268c5871f585681c649daded2ffe5fc657c3db87a9b2f38a8917cc";
  revision = "1";
  editedCabalFile = "0923r86fnmgpx0msm68aszirh2n19nn5bccgjxfh2146jw4z7w3z";
  libraryHaskellDepends = [
    aeson assoc base base-compat binary deepseq hashable QuickCheck
    semigroupoids unordered-containers
  ];
  homepage = "https://github.com/isomorphism/these";
  description = "An either-or-both data type";
  license = stdenv.lib.licenses.bsd3;
}
