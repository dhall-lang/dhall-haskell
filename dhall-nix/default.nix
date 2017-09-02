{ mkDerivation, base, containers, data-fix, dhall, hnix
, neat-interpolation, optparse-generic, stdenv, text, trifecta
, vector
}:
mkDerivation {
  pname = "dhall-nix";
  version = "1.0.6";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-fix dhall hnix neat-interpolation text vector
  ];
  executableHaskellDepends = [
    base dhall hnix optparse-generic text trifecta
  ];
  description = "Dhall to Nix compiler";
  license = stdenv.lib.licenses.bsd3;
}
