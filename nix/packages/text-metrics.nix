{ mkDerivation, base, containers, criterion, deepseq, hspec
, QuickCheck, stdenv, text, vector, weigh
}:
mkDerivation {
  pname = "text-metrics";
  version = "0.3.0";
  sha256 = "3874af74060e35f01702640b353ac2180d93bb5d292a204e0ee3cadd26efbfa2";
  revision = "4";
  editedCabalFile = "017drxq9x56b345d8w5m8xdsi1zzs0z16pbdx8j35cd1lsnh3kf1";
  libraryHaskellDepends = [ base containers text vector ];
  testHaskellDepends = [ base hspec QuickCheck text ];
  benchmarkHaskellDepends = [ base criterion deepseq text weigh ];
  homepage = "https://github.com/mrkkrp/text-metrics";
  description = "Calculate various string metrics efficiently";
  license = stdenv.lib.licenses.bsd3;
}
