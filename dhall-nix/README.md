# `dhall-nix 1.0.1`

This `dhall-nix` package provides a Dhall to Nix compiler.  You can use this
compiler to program Nix using the Dhall language.  This package targets people
who wish Nix had a type system.

## Quick start

If you have Nix installed then you can build and run this package using:

```bash
$ nix-build -A dhall-nix release.nix
$ result/bin/dhall-to-nix <<< "λ(x : Bool) → x == False"
x: x == false
$ result/bin/dhall-to-nix <<< "{ foo = 1, bar = True }"
{ bar = true; foo = 1; }
$ result/bin/dhall-to-nix <<< "< Left = 2 | Right : Natural >"
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

The above `dhallToNix` utility is now in `nixpkgs` so you can use `pkgs.dhallToNix`
to transform Dhall expressions to Nix expressions

## Development status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Dhall-Nix-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Dhall-Nix-Library)

I don't expect this library to change unless:

* ... the Dhall language changes, which is possible but not very likely
* ... there are bugs, but the test suite in [release.nix](./release.nix) should
  protect against this

## License (BSD 3-clause)

    Copyright (c) 2017 Gabriel Gonzalez
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright notice,
          this list of conditions and the following disclaimer in the documentation
          and/or other materials provided with the distribution.
        * Neither the name of Gabriel Gonzalez nor the names of other contributors
          may be used to endorse or promote products derived from this software
          without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
