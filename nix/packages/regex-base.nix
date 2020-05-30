{ mkDerivation, array, base, bytestring, containers, mtl, stdenv
, text
}:
mkDerivation {
  pname = "regex-base";
  version = "0.94.0.0";
  sha256 = "c41f82f5fc1157c961a4cbdc0cd5561e5aa44f339ce6e706d978d97e0ca6b914";
  revision = "1";
  editedCabalFile = "13lnky4ps9as73jqrwz4aqn5sfyrcz2zj2ng52xzz512fv59baj4";
  libraryHaskellDepends = [
    array base bytestring containers mtl text
  ];
  homepage = "https://wiki.haskell.org/Regular_expressions";
  description = "Common \"Text.Regex.*\" API for Regex matching";
  license = stdenv.lib.licenses.bsd3;
}
