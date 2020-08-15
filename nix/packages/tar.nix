{ mkDerivation, array, base, bytestring, bytestring-handle
, containers, criterion, deepseq, directory, filepath, QuickCheck
, stdenv, tasty, tasty-quickcheck, time
}:
mkDerivation {
  pname = "tar";
  version = "0.5.1.1";
  sha256 = "b384449f62b2b0aa3e6d2cb1004b8060b01f21ec93e7b63e7af6d8fad8a9f1de";
  revision = "2";
  editedCabalFile = "131f369a2vjzr26r7f2c2p534xvyw0s7cvgvih2ck56lqha58wbs";
  libraryHaskellDepends = [
    array base bytestring containers deepseq directory filepath time
  ];
  testHaskellDepends = [
    array base bytestring bytestring-handle containers deepseq
    directory filepath QuickCheck tasty tasty-quickcheck time
  ];
  benchmarkHaskellDepends = [
    array base bytestring containers criterion deepseq directory
    filepath time
  ];
  description = "Reading, writing and manipulating \".tar\" archive files.";
  license = stdenv.lib.licenses.bsd3;
}
