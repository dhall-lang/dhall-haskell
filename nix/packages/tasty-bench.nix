{ mkDerivation, base, containers, deepseq, ghc-prim, lib, tasty }:
mkDerivation {
  pname = "tasty-bench";
  version = "0.4";
  sha256 = "829c80478dcd6450f3ddab0232603850bff6bc7277b2eecf126b2fd9c26d7be2";
  libraryHaskellDepends = [ base containers deepseq ghc-prim tasty ];
  benchmarkHaskellDepends = [ base ];
  homepage = "https://github.com/Bodigrim/tasty-bench";
  description = "Featherlight benchmark framework";
  license = lib.licenses.mit;
}
