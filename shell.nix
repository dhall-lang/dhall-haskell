{ args ? {} , shell ? "bash" }:
let
  shared = import ./nix/shared.nix args;
  static = shared.possibly-static;
in
shared.pkgs.runCommand "dhall-shell-${shell}" {
  buildInputs = [
    static.dhall
    static.dhall-json
    static.dhall-text
    static.dhall-bash
    static.dhall-nix
  ];
  shellHook = ''
    echo "Dhall core tools shell"
    echo "USAGE EXAMPLES"
    echo "    Default (GHC version ${(import ./nix/shared.nix {}).pkgs.haskellPackages.ghc.version}, option completions shell - bash):"
    echo "        $ nix-shell"
    echo "    Overriding default GHC compiler (see ./nix/shared.nix for available options):"
    echo "        $ nix-shell --arg args '{ compiler = "ghc7103"; }'"
    echo "    Overriding option completion shell flavor:"
    echo "        $ nix-shell --argstr shell zsh"

    source <(dhall         --${shell}-completion-script dhall)
    source <(dhall-to-text --${shell}-completion-script dhall-to-text)
    source <(dhall-to-bash --${shell}-completion-script dhall-to-bash)
    source <(dhall-to-nix  --${shell}-completion-script dhall-to-nix)
    source <(dhall-to-json --${shell}-completion-script dhall-to-json)
    source <(dhall-to-yaml --${shell}-completion-script dhall-to-yaml)
    source <(json-to-dhall --${shell}-completion-script json-to-dhall)
  '';
} ""
