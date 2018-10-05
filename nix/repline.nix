{ mkDerivation, base, containers, haskeline, mtl, process, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.2.0.0";
  sha256 = "ecc72092d0340b896ee6bf96bf6645694dbcd33361725a2cd28c5ab5d60c02de";
  libraryHaskellDepends = [ base containers haskeline mtl process ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
