{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, directory, doctest, exceptions, filepath, foldl
, hostname, lib, managed, optional-args, optparse-applicative
, process, stm, streaming-commons, tasty, tasty-bench, tasty-hunit
, temporary, text, time, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.6.2";
  sha256 = "1a0166b11566e956bcec6f37cfcfc4d0647d4540479545d4858d2ff8c43a5b2d";
  revision = "3";
  editedCabalFile = "19i3n3hd2a0rkdz1ikwdgwhg4ds5pcfah25vgk0jnmwf71h0qwbm";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock containers directory
    exceptions filepath foldl hostname managed optional-args
    optparse-applicative process stm streaming-commons temporary text
    time transformers unix unix-compat
  ];
  testHaskellDepends = [
    base doctest filepath tasty tasty-hunit temporary
  ];
  benchmarkHaskellDepends = [ base tasty-bench text ];
  description = "Shell programming, Haskell-style";
  license = lib.licenses.bsd3;
}
