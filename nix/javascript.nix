# Haskell packages compiled with GHC's JavaScript backend via pkgsCross.ghcjs.
#
# Legacy haskell.packages.ghcjs (GHCJS 8.10) was removed in Nixpkgs 25.11.
# Do not pass compiler = "ghcjs" to ./shared.nix on this pin; it throws.
#
# Build (typically several hours, poorly cached):
#
#   nix-build nix/javascript.nix -A dhall-try
#
# The minified interpreter is then:
#
#   result/bin/dhall-try.jsexe/all.min.js
#
# A bare `nix-build` of default.nix does not include these attributes, so a JS
# compile failure cannot block native Hydra tarballs.

{ nixpkgs ? (import ./pinnedNixpkgs.nix).nixpkgs
, compiler ? "ghc96"
, system ? builtins.currentSystem
}:

let
  overlayJs = pkgsNew: pkgsOld: {
    haskellSrc = src:
      pkgsNew.lib.cleanSourceWith {
        inherit src;
        filter = path: type:
          let
            base = baseNameOf path;
          in
            !( pkgsNew.lib.hasSuffix ".nix" base
            || base == "dist"
            || base == "result"
            || base == ".git"
            );
      };

    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        # GHC's FP_GCC_VERSION treats `emcc` as GCC because `emcc --version`
        # contains "gcc", then parses `emcc -v`. Emscripten 5's banner has no
        # "version " token, so the parsed version is empty and configure
        # errors with "Need at least gcc version 4.7". The same check is in
        # GHC 9.6 through 9.14. Preset the autoconf cache; this Clang has the
        # __atomic builtins the check is guarding. GccVersion is only printed
        # afterwards. 4.7 is the minimum that passes.
        "${compiler}" = (pkgsOld.haskell.packages."${compiler}".override (
          pkgsNew.lib.optionalAttrs pkgsNew.stdenv.hostPlatform.isGhcjs {
            ghc = pkgsNew.buildPackages.haskell.compiler.ghc967.overrideAttrs (old: {
              preConfigure = (old.preConfigure or "") + ''
                export fp_cv_gcc_version=4.7
              '';
            });
          }
        )).override (old: {
          overrides =
            pkgsNew.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (haskellPackagesNew: haskellPackagesOld:
                let
                  hlib = pkgsNew.haskell.lib;
                  skip = drv:
                    hlib.dontHaddock (hlib.dontBenchmark (hlib.dontCheck drv));
                  # JS GHC ships a broken unprefixed `hsc2hs-ghc-*` wrapper
                  # (`exeprog` is missing the target prefix, so --version fails).
                  # Nixpkgs only passes --with-hsc2hs when stdenv.hasCC, which
                  # is false for pkgsCross.ghcjs, so Cabal picks the wrapper.
                  # unix-compat's cbits include sys/sysmacros.h only on Linux;
                  # Emscripten has the same macros but does not define __linux__.
                  hsc2hs =
                    "${haskellPackagesOld.ghc}/bin/${haskellPackagesOld.ghc.targetPrefix}hsc2hs";
                in {
                  mkDerivation = args:
                    haskellPackagesOld.mkDerivation (args // {
                      configureFlags = (args.configureFlags or []) ++ [
                        "--with-hsc2hs=${hsc2hs}"
                      ];
                      postPatch = (args.postPatch or "")
                        + pkgsNew.lib.optionalString ((args.pname or "") == "unix-compat") ''
                            substituteInPlace cbits/HsUnixCompat.c \
                              --replace-fail \
                                'defined(__linux__) || defined(__GNU__)' \
                                'defined(__linux__) || defined(__GNU__) || defined(__EMSCRIPTEN__)'
                          '';
                    });

                  dhall =
                    hlib.overrideCabal
                      (skip
                        (haskellPackagesNew.callCabal2nixWithOptions
                          "dhall"
                          (pkgsNew.haskellSrc ../dhall)
                          "--flag=javascript --no-check --no-haddock"
                          # cabal2nix still lists test-suite deps as function
                          # arguments even with --no-check.
                          { dhall-test-server = null; }
                        )
                      )
                      (_: { isExecutable = false; });

                  dhall-json =
                    skip
                      (haskellPackagesNew.callCabal2nixWithOptions
                        "dhall-json"
                        (pkgsNew.haskellSrc ../dhall-json)
                        "--no-check --no-haddock"
                        { }
                      );

                  dhall-try =
                    hlib.overrideCabal
                      (skip
                        (haskellPackagesNew.callCabal2nix
                          "dhall-try"
                          (pkgsNew.haskellSrc ../dhall-try)
                          { }
                        )
                      )
                      (oldDrv: {
                        postInstall = (oldDrv.postInstall or "") + ''
                          jsexe="$out/bin/dhall-try.jsexe"
                          if [ ! -f "$jsexe/all.js" ]; then
                            echo "dhall-try: expected $jsexe/all.js from the JS backend" >&2
                            find "$out" -name '*.js' | head -50 >&2
                            exit 1
                          fi
                          externs=""
                          if [ -f "$jsexe/all.js.externs" ]; then
                            externs="--externs=$jsexe/all.js.externs"
                          fi
                          ${pkgsNew.buildPackages.closurecompiler}/bin/closure-compiler \
                            "$jsexe/all.js" \
                            --jscomp_off=checkVars \
                            $externs \
                            > "$jsexe/all.min.js"
                        '';
                      });
                }
              );
        });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit system;
    config = { allowBroken = true; };
    overlays = [ overlayJs ];
  };

  hpkgs = pkgs.pkgsCross.ghcjs.haskell.packages."${compiler}";

in
  {
    inherit (hpkgs) dhall dhall-json dhall-try;

    # Cross .env shells are awkward; this at least exposes the compiler.
    shell-dhall-try = hpkgs.dhall-try.env;

    # CI garbage-collects the Nix store before saving it, and keeps only GC
    # roots. `nix-build -A dhall-try` roots the interpreter, which does not
    # reference the cross compiler. GitHub does not update a cache key once
    # it exists, so the compiler has to be in that first save or a later
    # source change rebuilds it and the rebuild is discarded.
    # The output file names the compiler store path. GHC's lib/settings
    # points at emcc, so Emscripten stays in the closure too.
    cache-roots = pkgs.runCommand "dhall-try-cache-roots" { } ''
      mkdir -p "$out"
      printf '%s\n' ${hpkgs.ghc} > "$out/ghc"
    '';
  }
