{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, comonad, containers, contravariant, criterion
, data-fix, deepseq, deriving-compat, Diff, directory, exceptions
, filepath, free, generic-random, Glob, hashable, hashing
, haskeline, hedgehog, hnix-store-core, http-client
, http-client-tls, http-types, interpolate, lens-family
, lens-family-core, lens-family-th, logict, megaparsec
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
  version = "0.9.0";
  sha256 = "5cecc7cf00e7479914be12b5021f54ff8f6096ea1ccedf20eb79be88d52b929e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring comonad
    containers contravariant data-fix deepseq deriving-compat directory
    exceptions filepath free hashable hashing hnix-store-core
    http-client http-client-tls http-types interpolate lens-family
    lens-family-core lens-family-th logict megaparsec monad-control
    monadlist mtl optparse-applicative parser-combinators pretty-show
    prettyprinter process ref-tf regex-tdfa scientific semialign
    semialign-indexed semigroups serialise some split syb
    template-haskell text these time transformers transformers-base
    unix unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring comonad containers data-fix
    deepseq exceptions filepath free haskeline mtl optparse-applicative
    pretty-show prettyprinter ref-tf repline serialise template-haskell
    text time transformers unordered-containers
  ];
  testHaskellDepends = [
    base base16-bytestring bytestring containers data-fix deepseq Diff
    directory exceptions filepath generic-random Glob hedgehog
    interpolate megaparsec mtl optparse-applicative pretty-show
    prettyprinter process serialise split tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck tasty-th template-haskell text time
    transformers unix unordered-containers
  ];
  benchmarkHaskellDepends = [
    base base16-bytestring bytestring containers criterion data-fix
    deepseq exceptions filepath mtl optparse-applicative serialise
    template-haskell text time transformers unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
