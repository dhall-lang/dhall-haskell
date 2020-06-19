{ mkDerivation, aeson, base, case-insensitive, containers
, criterion, deepseq, dlist, email-validate, foldl, hashable, hspec
, hspec-megaparsec, html-entity-map, lucid, megaparsec, microlens
, microlens-th, modern-uri, mtl, parser-combinators, QuickCheck
, stdenv, text, text-metrics, unordered-containers, weigh, yaml
}:
mkDerivation {
  pname = "mmark";
  version = "0.0.7.0";
  sha256 = "9dba16ca0c1b4c0e6a83ae139ce51fffdc993efd6108dd227b1bc9bdfbe8f53c";
  revision = "2";
  editedCabalFile = "06hr9p2lqyh4zx38i601yd24acbl08p69l0pwh8266qvfripcsm9";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base case-insensitive containers deepseq dlist email-validate
    foldl hashable html-entity-map lucid megaparsec microlens
    microlens-th modern-uri mtl parser-combinators text text-metrics
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base foldl hspec hspec-megaparsec lucid megaparsec modern-uri
    QuickCheck text
  ];
  benchmarkHaskellDepends = [ base criterion text weigh ];
  homepage = "https://github.com/mmark-md/mmark";
  description = "Strict markdown processor for writers";
  license = stdenv.lib.licenses.bsd3;
}
