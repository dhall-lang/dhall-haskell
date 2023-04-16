let
  pinned = import ./pinnedNixpkgs.nix;
  
  defaultCompiler = "ghc8107";

in

{ nixpkgs ? pinned.nixpkgs
, compiler ? defaultCompiler
, coverage ? false
, system ? builtins.currentSystem
}:

let
  allDhallPackages = [
    "dhall"
    "dhall-bash"
    "dhall-csv"
    "dhall-docs"
    "dhall-json"
    "dhall-lsp-server"
    "dhall-nix"
    "dhall-nixpkgs"
    "dhall-openapi"
    "dhall-toml"
    "dhall-yaml"
  ];

  mass = function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toNameValue = name: {
        inherit name;

        value = function haskellPackagesOld."${name}";
      };

    in
      builtins.listToAttrs (map toNameValue names);

  overlayShared = pkgsNew: pkgsOld: {
    fixedCabal = pkgsNew.pkgsMusl.haskell.packages."${compiler}".Cabal_3_2_1_0;

    sdist = pkgsNew.callPackage ./sdist.nix { };

    dhallToNix = pkgsOld.dhallToNix.override {
      inherit (pkgsNew.haskell.packages."${compiler}") dhall-nix;
    };

    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                applyCoverage = drv:
                  if coverage
                  then
                    pkgsNew.haskell.lib.overrideCabal
                      (pkgsNew.haskell.lib.doCoverage
                        (pkgsNew.haskell.lib.doCheck drv)
                      )
                      (old: {
                          postInstall = (old.postInstall or "") + ''
                            ${pkgsNew.coreutils}/bin/mkdir --parents $out/nix-support
                            ${pkgsNew.coreutils}/bin/ln --symbolic $out/share/hpc/vanilla/html/dhall-* "$out/share/hpc/vanilla/html/dhall"
                            ${pkgsNew.coreutils}/bin/echo "report coverage $out/share/hpc/vanilla/html/dhall/hpc_index.html" >> $out/nix-support/hydra-build-products
                          '';
                        }
                      )
                  else
                    pkgsNew.haskell.lib.dontCheck drv;

                failOnAllWarnings = drv:
                  # GHCJS incorrectly detects non-exhaustive pattern matches
                  if compiler == "ghcjs"
                  then drv
                  else pkgsNew.haskell.lib.failOnAllWarnings drv;

                failOnMissingHaddocks = drv:
                  if compiler == defaultCompiler
                  then
                    drv.overrideAttrs
                    (old: {
                        postHaddock = (old.postHaddock or "") + ''
                          if ./Setup haddock 2>&1 | grep --quiet 'Missing documentation for:\|Warning:.*is out of scope'; then
                            ./Setup haddock 2>&1
                            echo "Error: Incomplete haddocks"; exit 1
                          fi
                        '';
                      }
                    )
                  else
                    drv;

                doCheckExtension =
                  mass pkgsNew.haskell.lib.doCheck
                    (   [ "dhall-bash"
                          "dhall-csv"
                          "dhall-docs"
                          # The test suite fails due to a relative reference
                          # to ../dhall/dhall-lang/
                          # "dhall-lsp-server"
                          "dhall-nix"
                          "dhall-nixpkgs"
                          "dhall-openapi"
                          "dhall-toml"
                          "dhall-yaml"
                        ]
                        # Test suite doesn't work on GHCJS
                    ++  pkgsNew.lib.optional (!(compiler == "ghcjs")) "dhall"
                        # Test suite fails on GHCJS due to `aeson` ordering
                        # HashMap values in a different order
                    ++  pkgsNew.lib.optional (!(compiler == "ghcjs")) "dhall-json"
                    );

                doBenchmarkExtension =
                  mass pkgsNew.haskell.lib.doBenchmark allDhallPackages;

                failOnAllWarningsExtension =
                  mass failOnAllWarnings [
                    "dhall"
                    "dhall-bash"
                    "dhall-csv"
                    "dhall-docs"
                    "dhall-json"
                    "dhall-lsp-server"
                    "dhall-nix"
                    "dhall-nixpkgs"
                    "dhall-openapi"
                    "dhall-toml"
                    "dhall-yaml"
                  ];

                failOnMissingHaddocksExtension =
                  mass failOnMissingHaddocks [
                    "dhall"
                  ];

                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    mkDerivation =
                      args: haskellPackagesOld.mkDerivation (args // {
                          doCheck = false;
                        }
                      );

                    dhall =
                        (applyCoverage
                          (haskellPackagesNew.callCabal2nix
                            "dhall"
                            (pkgsNew.sdist ../dhall)
                            { }
                          )
                        ).overrideAttrs (old: { XDG_CACHE_HOME=".cache"; });

                    dhall-no-http =
                      pkgsNew.haskell.lib.appendConfigureFlag
                        haskellPackagesNew.dhall
                        [ "-f-with-http" ];

                    dhall-bash =
                      haskellPackagesNew.callCabal2nix
                        "dhall-bash"
                        (pkgsNew.sdist ../dhall-bash)
                        { };

                    dhall-csv =
                      haskellPackagesNew.callCabal2nix
                        "dhall-cvs"
                        (pkgsNew.sdist ../dhall-csv)
                        { };

                    dhall-docs =
                      haskellPackagesNew.callCabal2nix
                        "dhall-docs"
                        (pkgsNew.sdist ../dhall-docs)
                        { };

                    dhall-json =
                      haskellPackagesNew.callCabal2nix
                        "dhall-json"
                        (pkgsNew.sdist ../dhall-json)
                        { };

                    dhall-nix =
                      haskellPackagesNew.callCabal2nix
                        "dhall-nix"
                        (pkgsNew.sdist ../dhall-nix)
                        { };

                    dhall-nixpkgs =
                      haskellPackagesNew.callCabal2nix
                        "dhall-nixpkgs"
                        (pkgsNew.sdist ../dhall-nixpkgs)
                        { };

                    dhall-openapi =
                      haskellPackagesNew.callCabal2nix
                        "dhall-openapi"
                        (pkgsNew.sdist ../dhall-openapi)
                        { };

                    dhall-lsp-server =
                      haskellPackagesNew.callCabal2nix
                        "dhall-lsp-server"
                        (pkgsNew.sdist ../dhall-lsp-server)
                        { };

                    dhall-toml =
                      haskellPackagesNew.callCabal2nix
                        "dhall-toml"
                        (pkgsNew.sdist ../dhall-toml)
                        { };

                    dhall-yaml =
                      haskellPackagesNew.callCabal2nix
                        "dhall-yaml"
                        (pkgsNew.sdist ../dhall-yaml)
                        { };

                    dhall-try =
                      pkgsNew.haskell.lib.overrideCabal
                        (haskellPackagesNew.callCabal2nix
                          "dhall-try"
                          (pkgsNew.sdist ../dhall-try)
                          { }
                        )
                        (old: {
                            postInstall = (old.postInstall or "") + ''
                              ${pkgsNew.closurecompiler}/bin/closure-compiler $out/bin/dhall-try.jsexe/all.js --jscomp_off=checkVars --externs=$out/bin/dhall-try.jsexe/all.js.externs > $out/bin/dhall-try.jsexe/all.min.js
                            '';
                          }
                        );

                    gauge =
                      pkgsNew.haskell.lib.appendPatch
                        haskellPackagesOld.gauge
                        (pkgsNew.fetchpatch {
                          url = "https://github.com/vincenthz/hs-gauge/commit/303a6b611804c85b9a6bc1cea5de4e6ce3429d24.patch";

                          sha256 = "sha256-4osUMo0cvTvyDTXF8lY9tQbFqLywRwsc3RkHIhqSriQ=";
                        });
                  };

              in
                pkgsNew.lib.fold
                  pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./packages; })
                    extension
                    doCheckExtension
                    doBenchmarkExtension
                    failOnAllWarningsExtension
                    failOnMissingHaddocksExtension
                  ];
          }
        );
      };
    };

    # we only reference git repositories with cabal2nix
    nix-prefetch-scripts = pkgsOld.nix-prefetch-scripts.override {
      mercurial = null;
      bazaar = null;
      cvs = null;
      subversion = null;
    };
  };

  pkgs = import nixpkgs {
    inherit system;

    config = { allowBroken = true; };

    overlays = [ overlayShared ];
  };

  overlayStaticLinux = pkgsNew: pkgsOld: {
    cabal2nix = pkgs.cabal2nix;

    haskell = pkgsOld.haskell // {
      lib = pkgsOld.haskell.lib // {
        useFixedCabal = drv:
          (pkgsNew.haskell.lib.overrideCabal drv (old: {
              setupHaskellDepends = (old.setupHaskellDepends or []) ++ [ pkgsNew.fixedCabal ];
            }
          )).overrideAttrs (old: {
              preCompileBuildDriver = (old.preCompileBuildDriver or "") + ''
                cabalPackageId=$(basename --suffix=.conf ${pkgsNew.fixedCabal}/lib/ghc-*/package.conf.d/*.conf)
                setupCompileFlags="$setupCompileFlags -package-id $cabalPackageId"
              '';
            }
          );

        statify = drv:
          pkgsNew.haskell.lib.appendConfigureFlags
            (pkgsNew.haskell.lib.justStaticExecutables
              (pkgsNew.haskell.lib.useFixedCabal (pkgsNew.haskell.lib.dontCheck drv))
            )
            [ "--enable-executable-static"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.ncurses.override { enableStatic = true; enableShared = true; }}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.zlib.static}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.libsodium.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ];
      };

      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    dhall-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall;

                    dhall-bash-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-bash;

                    dhall-csv-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-csv;

                    dhall-docs-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-docs;

                    dhall-json-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-json;

                    dhall-lsp-server-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-lsp-server;

                    dhall-nix-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-nix;

                    dhall-nixpkgs-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-nixpkgs;

                    dhall-openapi-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-openapi;

                    dhall-toml-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-toml;

                    dhall-yaml-static =
                        pkgsNew.haskell.lib.statify haskellPackagesOld.dhall-yaml;
                  };

              in
                pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  extension;
          }
        );
      };
    };
  };

  pkgsStaticLinux = import nixpkgs {
    config = { allowBroken = true; };
    overlays = [ overlayShared overlayStaticLinux ];
    system = "x86_64-linux";
  };

  trivial = x: pkgs.runCommand "trivial" { inherit x; } "touch $out";

  makeStaticIfPossible = name:
    if pkgs.stdenv.isLinux
    then
      pkgsStaticLinux.pkgsMusl.haskell.packages."${compiler}"."${name}-static"
    else
      pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.packages."${compiler}"."${name}");

  makeTarball = name: manDir:
    pkgsStaticLinux.releaseTools.binaryTarball rec {
      src = pkgsStaticLinux.pkgsMusl.haskell.packages."${compiler}"."${name}-static";

      installPhase = ''
        releaseName=${name}
        ${pkgsStaticLinux.coreutils}/bin/install --target-directory "$TMPDIR/inst/bin" -D $src/bin/*
        ${pkgs.lib.optionalString (manDir != null) ''
          ${pkgsStaticLinux.coreutils}/bin/install --target-directory "$TMPDIR/inst/share/man" -D ${manDir}/*
        ''}
      '';
    };

  toShell = drv: drv.env.overrideAttrs (base: {
    nativeBuildInputs = (base.nativeBuildInputs or []) ++ [ pkgs.cabal-install ];
  });

  possibly-static = {
    dhall            = makeStaticIfPossible "dhall"           ;
    dhall-bash       = makeStaticIfPossible "dhall-bash"      ;
    dhall-csv        = makeStaticIfPossible "dhall-csv"       ;
    dhall-docs       = makeStaticIfPossible "dhall-docs"      ;
    dhall-json       = makeStaticIfPossible "dhall-json"      ;
    dhall-lsp-server = makeStaticIfPossible "dhall-lsp-server";
    dhall-nix        = makeStaticIfPossible "dhall-nix"       ;
    dhall-nixpkgs    = makeStaticIfPossible "dhall-nixpkgs"   ;
    dhall-openapi    = makeStaticIfPossible "dhall-openapi"   ;
    dhall-toml       = makeStaticIfPossible "dhall-toml"      ;
    dhall-yaml       = makeStaticIfPossible "dhall-yaml"      ;
  };

  toDockerImage = name:
    let
      image =
        pkgs.dockerTools.buildImage {
          inherit name;

          contents = [ possibly-static."${name}" pkgs.cacert ];
        };

    in
      pkgs.runCommand "image-${name}" {} ''
        ${pkgs.coreutils}/bin/mkdir --parents "$out/nix-support"
        ${pkgs.coreutils}/bin/ln --symbolic '${image}' "$out/docker-image-${name}.tar.gz"
        echo "file binary-dist $out/docker-image-${name}.tar.gz" >> $out/nix-support/hydra-build-products
      '';

in
  rec {
    inherit trivial pkgs possibly-static;

    tarball-dhall            = makeTarball "dhall"            ../dhall/man;
    tarball-dhall-bash       = makeTarball "dhall-bash"       null;
    tarball-dhall-csv        = makeTarball "dhall-csv"        null;
    tarball-dhall-docs       = makeTarball "dhall-docs"       ../dhall-docs/src/Dhall/data/man;
    tarball-dhall-json       = makeTarball "dhall-json"       null;
    tarball-dhall-lsp-server = makeTarball "dhall-lsp-server" null;
    tarball-dhall-nix        = makeTarball "dhall-nix"        null;
    tarball-dhall-nixpkgs    = makeTarball "dhall-nixpkgs"    null;
    tarball-dhall-openapi    = makeTarball "dhall-openapi"    null;
    tarball-dhall-toml       = makeTarball "dhall-toml"       null;
    tarball-dhall-yaml       = makeTarball "dhall-yaml"       null;

    inherit (pkgs) tarball-website website;

    inherit (pkgs.haskell.packages."${compiler}")
      dhall
      dhall-no-http
      dhall-bash
      dhall-csv
      dhall-docs
      dhall-json
      dhall-lsp-server
      dhall-nix
      dhall-nixpkgs
      dhall-openapi
      dhall-toml
      dhall-try
      dhall-yaml
    ;

    inherit (pkgs.releaseTools) aggregate;

    shell-dhall            = toShell pkgs.haskell.packages."${compiler}".dhall           ;
    shell-dhall-bash       = toShell pkgs.haskell.packages."${compiler}".dhall-bash      ;
    shell-dhall-csv        = toShell pkgs.haskell.packages."${compiler}".dhall-csv       ;
    shell-dhall-docs       = toShell pkgs.haskell.packages."${compiler}".dhall-docs      ;
    shell-dhall-json       = toShell pkgs.haskell.packages."${compiler}".dhall-json      ;
    shell-dhall-lsp-server = toShell pkgs.haskell.packages."${compiler}".dhall-lsp-server;
    shell-dhall-nix        = toShell pkgs.haskell.packages."${compiler}".dhall-nix       ;
    shell-dhall-nixpkgs    = toShell pkgs.haskell.packages."${compiler}".dhall-nixpkgs   ;
    shell-dhall-openapi    = toShell pkgs.haskell.packages."${compiler}".dhall-openapi   ;
    shell-dhall-toml       = toShell pkgs.haskell.packages."${compiler}".dhall-toml      ;
    shell-dhall-try        = toShell pkgs.haskell.packages."${compiler}".dhall-try       ;
    shell-dhall-yaml       = toShell pkgs.haskell.packages."${compiler}".dhall-yaml      ;

    image-dhall            = toDockerImage "dhall"           ;
    image-dhall-bash       = toDockerImage "dhall-bash"      ;
    image-dhall-csv        = toDockerImage "dhall-csv"       ;
    image-dhall-docs       = toDockerImage "dhall-docs"      ;
    image-dhall-json       = toDockerImage "dhall-json"      ;
    image-dhall-lsp-server = toDockerImage "dhall-lsp-server";
    image-dhall-nix        = toDockerImage "dhall-nix"       ;
    image-dhall-nixpkgs    = toDockerImage "dhall-nixpkgs"   ;
    image-dhall-openapi    = toDockerImage "dhall-openapi"   ;
    image-dhall-toml       = toDockerImage "dhall-toml"      ;
    image-dhall-yaml       = toDockerImage "dhall-yaml"      ;

    prelude-dhall-docs = pkgs.callPackage ./dhall-docs-generator.nix {
      inherit dhall-docs;
      src = ../dhall/dhall-lang/Prelude;
      name = "prelude-dhall-docs";
    };

    test-dhall-docs = pkgs.callPackage ./dhall-docs-generator.nix {
      inherit dhall-docs;
      src = ../dhall-docs/tasty/data/package;
      name = "test-dhall-docs";
    };

    kubernetes-dhall-docs = pkgs.callPackage ./dhall-docs-generator.nix {
        inherit dhall-docs;
        src = pkgs.fetchurl {
            url = "https://github.com/dhall-lang/dhall-kubernetes/archive/f4bf4b9ddf669f7149ec32150863a93d6c4b3ef1.zip";
            sha256 = "64c23c08da4f7169adefe70d6ea541a40f9f5785a5a18563ec8895a85bd493d3";
        };
        name = "kubernetes-dhall-docs";
    };

    test-dhall =
      pkgs.mkShell
        { buildInputs =
            [ (pkgs.haskell.packages."${compiler}".ghcWithPackages
                (pkgs: [ pkgs.dhall ])
              )
            ];
        };
  }
