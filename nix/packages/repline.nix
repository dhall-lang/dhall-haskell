{ mkDerivation, base, containers, exceptions, haskeline, mtl
, process, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.4.0.0";
  sha256 = "43c28c49c8e16276d32d0889f37f750d7c7a8d2758f1d35a9f36e68944e457b7";
  libraryHaskellDepends = [
    base containers exceptions haskeline mtl process
  ];
  testHaskellDepends = [ base containers mtl process ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
