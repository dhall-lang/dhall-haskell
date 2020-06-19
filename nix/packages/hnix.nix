{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, comonad, containers, contravariant, criterion
, cryptohash-md5, cryptohash-sha1, cryptohash-sha256
, cryptohash-sha512, data-fix, deepseq, deriving-compat, Diff
, directory, exceptions, filepath, free, generic-random, Glob
, hashable, hashing, haskeline, hedgehog, hnix-store-core
, http-client, http-client-tls, http-types, interpolate
, lens-family, lens-family-core, lens-family-th, logict, megaparsec
, monad-control, monadlist, mtl, optparse-applicative
, parser-combinators, pretty-show, prettyprinter, process, ref-tf
, regex-tdfa, repline, scientific, semialign, semialign-indexed
, semigroups, serialise, some, split, stdenv, syb, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, tasty-th
, template-haskell, text, these, time, transformers
, transformers-base, unix, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "hnix";
  version = "0.8.0";
  sha256 = "15eeb80d18fb6be7103afe1ddc744c50710ef5aec993f17cda37ab67dc9f3092";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring comonad
    containers contravariant cryptohash-md5 cryptohash-sha1
    cryptohash-sha256 cryptohash-sha512 data-fix deepseq
    deriving-compat directory exceptions filepath free hashable hashing
    haskeline hnix-store-core http-client http-client-tls http-types
    interpolate lens-family lens-family-core lens-family-th logict
    megaparsec monad-control monadlist mtl optparse-applicative
    parser-combinators pretty-show prettyprinter process ref-tf
    regex-tdfa scientific semialign semialign-indexed semigroups
    serialise some split syb template-haskell text these time
    transformers transformers-base unix unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring comonad containers
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq exceptions filepath free hashing haskeline mtl
    optparse-applicative pretty-show prettyprinter ref-tf repline
    serialise template-haskell text time transformers
    unordered-containers
  ];
  testHaskellDepends = [
    base base16-bytestring bytestring containers cryptohash-md5
    cryptohash-sha1 cryptohash-sha256 cryptohash-sha512 data-fix
    deepseq Diff directory exceptions filepath generic-random Glob
    hashing hedgehog interpolate megaparsec mtl optparse-applicative
    pretty-show prettyprinter process serialise split tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck tasty-th
    template-haskell text time transformers unix unordered-containers
  ];
  benchmarkHaskellDepends = [
    base base16-bytestring bytestring containers criterion
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq exceptions filepath hashing mtl
    optparse-applicative serialise template-haskell text time
    transformers unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
