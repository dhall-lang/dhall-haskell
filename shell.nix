{ args ? {} , shell ? "bash" }:
let
  shared = import ./nix/shared.nix args;
  static = shared.possibly-static;
in
shared.nixpkgs.runCommand "dhall-shell-bash" {
  buildInputs = [
    static.dhall
    static.dhall-json
    static.dhall-text
    static.dhall-bash
    static.dhall-nix
  ];
  shellHook = ''
    echo -e "\e[0;32m""\nA Dhall shell""\e[0m"
    source <(dhall         --${shell}-completion-script dhall)
    source <(dhall-to-text --${shell}-completion-script dhall-to-text)
    source <(dhall-to-bash --${shell}-completion-script dhall-to-bash)
    source <(dhall-to-nix  --${shell}-completion-script dhall-to-nix)
    source <(dhall-to-json --${shell}-completion-script dhall-to-json)
    source <(dhall-to-yaml --${shell}-completion-script dhall-to-yaml)
    source <(json-to-dhall --${shell}-completion-script json-to-dhall)
  '';
} ""
