{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, conduit, containers, directory, filepath, hspec, HUnit, mockery
, mtl, raw-strings-qq, resourcet, scientific, stdenv
, template-haskell, temporary, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "yaml";
  version = "0.10.2.0";
  sha256 = "4338d8140f4de63252d40838dd8a7802565f2116ce0170b97a09a2f0dd60747a";
  configureFlags = [ "-f-system-libyaml" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring conduit containers directory
    filepath mtl resourcet scientific template-haskell text
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat bytestring conduit containers
    directory filepath hspec HUnit mockery mtl raw-strings-qq resourcet
    scientific template-haskell temporary text transformers
    unordered-containers vector
  ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Support for parsing and rendering YAML documents";
  license = stdenv.lib.licenses.bsd3;
}
