{ mkDerivation, async, base, base-compat, base-compat-batteries
, bytestring, clock, containers, criterion, deepseq, HUnit
, math-functions, process, random, stdenv, test-framework
, test-framework-hunit, tf-random, time, vector
}:
mkDerivation {
  pname = "splitmix";
  version = "0.0.5";
  sha256 = "19f2987ba707c935656371776639588c3252cbb44b66cae16107b13e73b4cd52";
  libraryHaskellDepends = [ base deepseq random time ];
  testHaskellDepends = [
    async base base-compat base-compat-batteries bytestring containers
    deepseq HUnit math-functions process random test-framework
    test-framework-hunit tf-random vector
  ];
  benchmarkHaskellDepends = [
    base clock containers criterion random tf-random
  ];
  description = "Fast Splittable PRNG";
  license = stdenv.lib.licenses.bsd3;
}
