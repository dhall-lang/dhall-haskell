{ mkDerivation, aeson, ansi-wl-pprint, array, base
, base16-bytestring, binary, bytestring, containers, criterion
, cryptohash-md5, cryptohash-sha1, cryptohash-sha256
, cryptohash-sha512, data-fix, deepseq, deriving-compat, Diff
, directory, exceptions, filepath, generic-random, Glob, hashable
, hashing, haskeline, http-client, http-client-tls, http-types
, interpolate, lens-family, lens-family-core, lens-family-th
, logict, megaparsec, monadlist, mtl, optparse-applicative
, pretty-show, process, QuickCheck, quickcheck-instances
, regex-tdfa, regex-tdfa-text, repline, scientific, semigroups
, serialise, split, stdenv, syb, tasty, tasty-hunit
, tasty-quickcheck, tasty-th, template-haskell, text, these, time
, transformers, unix, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "hnix";
  version = "0.5.1";
  sha256 = "5425ad5fcd163e83194a1f09822ceb618919ec771bb33be42181bcdfe3e90be6";
  revision = "1";
  editedCabalFile = "1l8h9zc9mrqvp8hvkfz1g2inq2b95x42gscyfn6qv0jcxwn4sbpr";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint array base base16-bytestring binary bytestring
    containers cryptohash-md5 cryptohash-sha1 cryptohash-sha256
    cryptohash-sha512 data-fix deepseq deriving-compat directory
    exceptions filepath hashable hashing haskeline http-client
    http-client-tls http-types lens-family lens-family-core
    lens-family-th logict megaparsec monadlist mtl optparse-applicative
    pretty-show process regex-tdfa regex-tdfa-text scientific
    semigroups serialise split syb template-haskell text these time
    transformers unix unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring bytestring containers
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq exceptions filepath hashing haskeline mtl
    optparse-applicative pretty-show repline serialise template-haskell
    text time transformers unordered-containers
  ];
  testHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring containers
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq Diff directory exceptions filepath generic-random
    Glob hashing interpolate megaparsec mtl optparse-applicative
    pretty-show process QuickCheck quickcheck-instances serialise split
    tasty tasty-hunit tasty-quickcheck tasty-th template-haskell text
    time transformers unix unordered-containers
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring containers
    criterion cryptohash-md5 cryptohash-sha1 cryptohash-sha256
    cryptohash-sha512 data-fix deepseq exceptions filepath hashing mtl
    optparse-applicative serialise template-haskell text time
    transformers unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
