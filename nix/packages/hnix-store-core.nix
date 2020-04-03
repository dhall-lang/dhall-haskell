{ mkDerivation, base, base16-bytestring, base64-bytestring, binary
, bytestring, containers, cryptohash-md5, cryptohash-sha1
, cryptohash-sha256, directory, filepath, hashable, mtl, process
, regex-base, regex-tdfa-text, stdenv, tasty, tasty-discover
, tasty-hspec, tasty-hunit, tasty-quickcheck, temporary, text, unix
, unordered-containers, vector
}:
mkDerivation {
  pname = "hnix-store-core";
  version = "0.1.0.0";
  sha256 = "878c9a1dcb535b76efb7af392f9a9e75b954ce4ebb75acac91036a6c3c1d3df7";
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring containers cryptohash-md5
    cryptohash-sha1 cryptohash-sha256 directory filepath hashable mtl
    regex-base regex-tdfa-text text unix unordered-containers vector
  ];
  testHaskellDepends = [
    base base64-bytestring binary bytestring containers directory
    process tasty tasty-discover tasty-hspec tasty-hunit
    tasty-quickcheck temporary text
  ];
  testToolDepends = [ tasty-discover ];
  homepage = "https://github.com/haskell-nix/hnix-store";
  description = "Core effects for interacting with the Nix store";
  license = stdenv.lib.licenses.asl20;
}
