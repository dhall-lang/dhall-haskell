{ args ? {} , shell ? "bash" }:
let
  shared = import ./nix/shared.nix args;
  static = shared.possibly-static;
in
shared.pkgs.runCommand "dhall-shell-${shell}" {
  buildInputs = [
    static.dhall
    static.dhall-json
    static.dhall-bash
    static.dhall-nix
    static.dhall-yaml
  ];

  shellHook = ''
    echo "Dhall core tools shell"
    echo "USAGE EXAMPLES"
    echo "    Default (GHC version ${(import ./nix/shared.nix {}).pkgs.haskellPackages.ghc.version}, option completions shell - bash):"
    echo "        $ nix-shell"
    echo "    Overriding default GHC compiler (see ./nix/shared.nix for available options):"
    echo "        $ nix-shell --arg args '{ compiler = "ghc822"; }'"
    echo "    Overriding option completion shell flavor:"
    echo "        $ nix-shell --argstr shell zsh"

    EXECUTABLES=(
      dhall
      dhall-docs
      dhall-to-bash
      dhall-to-nix
      dhall-to-json
      dhall-to-yaml
      dhall-to-yaml-ng
      json-to-dhall
      yaml-to-dhall
    )

    if [ "${shell}" == "zsh" ]; then
      # zsh does not support loading completions using `source`, so we need to
      # add a special case for zsh

      COMPLETIONS=$(mktemp -d)
      export ZDOTDIR=$(mktemp -d)
      trap "rm --recursive -- $COMPLETIONS $ZDOTDIR" INT EXIT

      for EXECUTABLE in "''${EXECUTABLES[@]}"; do
          "$EXECUTABLE" --${shell}-completion-script "$EXECUTABLE" > "$COMPLETIONS/_$EXECUTABLE"
      done

      echo "export fpath=($COMPLETIONS \$fpath)" >> "$ZDOTDIR/.zshenv"
      echo 'autoload -Uz compinit'               >> "$ZDOTDIR/.zshrc"
      echo 'compinit'                            >> "$ZDOTDIR/.zshrc"

      # nix-shell uses bash, even if your original shell was zsh
      zsh -i
    else
      for EXECUTABLE in "''${EXECUTABLES[@]}"; do
          source <("$EXECUTABLE" --${shell}-completion-script "$EXECUTABLE")
      done
    fi
  '';
} ""
