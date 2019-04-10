{ mkDerivation, array, async, base, blaze-builder, bytestring
, case-insensitive, containers, cookie, deepseq, directory
, exceptions, filepath, ghc-prim, hspec, http-types, memory
, mime-types, monad-control, network, network-uri, random, stdenv
, stm, streaming-commons, text, time, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.5.14";
  sha256 = "8e50409704021c51a8955b2d03bfec900ebc3e11fbaebf973f2e654d7bde3647";
  revision = "1";
  editedCabalFile = "0xw5ac4cvcd4hcwl7j12adi7sgffjryqhk0x992k3qs1cxyv5028";
  libraryHaskellDepends = [
    array base blaze-builder bytestring case-insensitive containers
    cookie deepseq exceptions filepath ghc-prim http-types memory
    mime-types network network-uri random stm streaming-commons text
    time transformers
  ];
  testHaskellDepends = [
    async base blaze-builder bytestring case-insensitive containers
    deepseq directory hspec http-types monad-control network
    network-uri streaming-commons text time transformers zlib
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = stdenv.lib.licenses.mit;
}
