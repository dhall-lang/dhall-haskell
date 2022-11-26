{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, comonad, containers, criterion, cryptonite, data-fix
, deepseq, deriving-compat, Diff, directory, exceptions, filepath
, free, gitrev, Glob, hashable, hashing, haskeline, hedgehog
, hnix-store-core, hnix-store-remote, http-client, http-client-tls
, http-types, lens-family, lens-family-core, lens-family-th, lib
, logict, megaparsec, monad-control, monadlist, mtl
, neat-interpolation, optparse-applicative, parser-combinators
, pretty-show, prettyprinter, process, ref-tf, regex-tdfa, relude
, repline, scientific, semialign, serialise, some, split, syb
, tasty, tasty-hedgehog, tasty-hunit, tasty-th, template-haskell
, text, th-lift-instances, these, time, transformers
, transformers-base, unix-compat, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "hnix";
  version = "0.16.0";
  sha256 = "d0b5a93efe6bec97b4b1af6703be6cd4935240dd1616df3ab5c006a13a374b61";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring comonad
    containers cryptonite data-fix deepseq deriving-compat directory
    exceptions filepath free gitrev hashable hashing hnix-store-core
    hnix-store-remote http-client http-client-tls http-types
    lens-family lens-family-core lens-family-th logict megaparsec
    monad-control monadlist mtl neat-interpolation optparse-applicative
    parser-combinators pretty-show prettyprinter process ref-tf
    regex-tdfa relude scientific semialign serialise some split syb
    template-haskell text th-lift-instances these time transformers
    transformers-base unix-compat unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson base comonad containers data-fix deepseq exceptions filepath
    free haskeline optparse-applicative pretty-show prettyprinter
    ref-tf relude repline serialise template-haskell time
  ];
  testHaskellDepends = [
    base containers data-fix Diff directory exceptions filepath Glob
    hedgehog megaparsec neat-interpolation optparse-applicative
    pretty-show prettyprinter process relude serialise split tasty
    tasty-hedgehog tasty-hunit tasty-th template-haskell time
    unix-compat
  ];
  benchmarkHaskellDepends = [
    base criterion data-fix exceptions filepath optparse-applicative
    relude serialise template-haskell time
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = lib.licenses.bsd3;
}
