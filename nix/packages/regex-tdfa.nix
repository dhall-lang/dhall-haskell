{ mkDerivation, array, base, bytestring, containers, directory
, filepath, mtl, parsec, regex-base, stdenv, text, utf8-string
}:
mkDerivation {
  pname = "regex-tdfa";
  version = "1.3.1.0";
  sha256 = "15c376783d397b3b9933cf35980808feddde273bd6f2445babbccb2f76a42ec0";
  revision = "1";
  editedCabalFile = "1fhi4g2p29qnnfyb211n62g97qrw3gz1kahca7rlz43all93ihdy";
  libraryHaskellDepends = [
    array base bytestring containers mtl parsec regex-base text
  ];
  testHaskellDepends = [
    array base bytestring containers directory filepath mtl regex-base
    text utf8-string
  ];
  description = "Pure Haskell Tagged DFA Backend for \"Text.Regex\" (regex-base)";
  license = stdenv.lib.licenses.bsd3;
}
