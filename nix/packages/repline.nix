{ mkDerivation, base, containers, fail, haskeline, mtl, process
, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.2.1.0";
  sha256 = "45c3186ff35ed650ee9c641f545a30860eedc44107fefb21da36df47aeb1ae7b";
  libraryHaskellDepends = [
    base containers fail haskeline mtl process
  ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
