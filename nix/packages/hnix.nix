{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, comonad, containers, criterion, data-fix, deepseq
, deriving-compat, Diff, directory, exceptions, filepath, free
, gitrev, Glob, hashable, hashing, hedgehog, hnix-store-core
, http-client, http-client-tls, http-types, lens-family
, lens-family-core, lens-family-th, logict, megaparsec
, monad-control, monadlist, mtl, neat-interpolation
, optparse-applicative, parser-combinators, pretty-show
, prettyprinter, process, ref-tf, regex-tdfa, scientific, semialign
, semialign-indexed, serialise, some, split, stdenv, syb, tasty
, tasty-hedgehog, tasty-hunit, tasty-th, template-haskell, text
, these, time, transformers, transformers-base, unix
, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "hnix";
  version = "0.10.1";
  sha256 = "9e31ed9f8a051ec3b17049d23f2f93a859ed2dac72ae2df7c812922883ba45a5";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring comonad
    containers data-fix deepseq deriving-compat directory exceptions
    filepath free gitrev hashable hashing hnix-store-core http-client
    http-client-tls http-types lens-family lens-family-core
    lens-family-th logict megaparsec monad-control monadlist mtl
    neat-interpolation optparse-applicative parser-combinators
    pretty-show prettyprinter process ref-tf regex-tdfa scientific
    semialign semialign-indexed serialise some split syb
    template-haskell text these time transformers transformers-base
    unix unordered-containers vector xml
  ];
  testHaskellDepends = [
    base base16-bytestring bytestring containers data-fix deepseq Diff
    directory exceptions filepath Glob hedgehog megaparsec mtl
    neat-interpolation optparse-applicative pretty-show prettyprinter
    process serialise split tasty tasty-hedgehog tasty-hunit tasty-th
    template-haskell text time transformers unix unordered-containers
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
