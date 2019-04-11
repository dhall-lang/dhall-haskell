# `dhall-nix`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

Full documentation here:

* [`dhall-nix` instructions](https://hackage.haskell.org/package/dhall-nix/docs/Dhall-Nix.html)

## Introduction

This `dhall-nix` package provides a Dhall to Nix compiler.  You can use this
compiler to program Nix using the Dhall language.  This package targets people
who wish Nix had a type system.

## Quick start

If you have Nix installed then you can build and run this package using:

```bash
$ dhall-to-nix <<< "λ(x : Bool) → x == False"
x: x == false
$ dhall-to-nix <<< "{ foo = 1, bar = True }"
{ bar = true; foo = 1; }
$ dhall-to-nix <<< "< Left = 2 | Right : Natural >"
{ Left, Right }: Left 2
```

However, this package is also designed to be used directly from Nix.  You can
use the following `dhallToNix` utility to translate Dhall source code to the
corresponding Nix expression directly within Nix:

```nix
dhallToNix = code :
  let
    file = builtins.toFile "dhall-expr" code;

    drv = pkgs.stdenv.mkDerivation {
      name = "dhall-expr-as-nix";

      buildCommand = ''
        dhall-to-nix <<< "${file}" > $out
      '';

      buildInputs = [ pkgs.haskellPackages.dhall-nix ];
    };
  in
    import "${drv}";
```

The above `dhallToNix` utility is now in `nixpkgs` so you can use
`pkgs.dhallToNix` to transform Dhall expressions to Nix expressions
