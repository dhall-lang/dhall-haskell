{ mkDerivation, base, containers, data-fix, dhall, hnix
, insert-ordered-containers, neat-interpolation, optparse-generic
, scientific, stdenv, text, text-format, trifecta, vector
}:
mkDerivation {
  pname = "dhall-nix";
  version = "1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-fix dhall hnix insert-ordered-containers
    neat-interpolation scientific text text-format vector
  ];
  executableHaskellDepends = [
    base dhall hnix optparse-generic text trifecta
  ];
  description = "Dhall to Nix compiler";
  license = stdenv.lib.licenses.bsd3;
}
