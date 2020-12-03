let
  pinned = import ./pinnedNixpkgs.nix;
  
  defaultCompiler = "ghc865";

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
    "dhall-docs"
    "dhall-json"
    "dhall-lsp-server"
    "dhall-nix"
    "dhall-nixpkgs"
    "dhall-openapi"
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
    applyPatchesToCabalDrv =
      cabalDrv: pkgsNew.haskell.lib.overrideCabal cabalDrv (old: {
          patches = (old.patches or []) ++ [
            (pkgsNew.makeCabalPatch {
                name = "5446.patch";
                url = "https://github.com/haskell/cabal/commit/cb221c23c274f79dcab65aef3756377af113ae21.patch";
                sha256 = "02qalj5y35lq22f19sz3c18syym53d6bdqzbnx9f6z3m7xg591p1";
              }
            )
            (pkgsNew.makeCabalPatch {
                name = "5451.patch";
                url = "https://github.com/haskell/cabal/commit/0aeb541393c0fce6099ea7b0366c956e18937791.patch";
                sha256 = "0pa9r79730n1kah8x54jrd6zraahri21jahasn7k4ng30rnnidgz";
              }
            )
          ];
        }
      );


    fixedCabal =
      let
        buildPlatformHaskellPackagesWithFixedCabal =
          pkgsNew.pkgsMusl.haskell.packages."${compiler}".override (old: {
              overrides =
                pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  (haskellPackagesNew: haskellPackagesOld: {
                      Cabal = pkgsNew.applyPatchesToCabalDrv haskellPackagesNew.Cabal_2_4_1_0;
                    }
                  );
            }
          );

      in
        buildPlatformHaskellPackagesWithFixedCabal.Cabal;

    sdist = pkgsNew.callPackage ./sdist.nix { };

    dhallToNix = pkgsOld.dhallToNix.override {
      inherit (pkgsNew.haskell.packages."${compiler}") dhall-nix;
    };

    makeCabalPatch = { name, url, sha256 }:
      let
        plainPatchFile = pkgsNew.fetchpatch { inherit name url sha256; };

      in
        pkgsNew.runCommand "${name}-Cabal-only" {} ''
          ${pkgsNew.patchutils}/bin/filterdiff \
            -p1 -i 'Cabal/*' -x 'Cabal/ChangeLog.md' \
            --strip=2 --addoldprefix=a/ --addnewprefix=b/ \
            ${plainPatchFile} > $out
        '';

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
                          ! (./Setup haddock 2>&1 | grep --quiet 'Missing documentation for:\|Warning:.*is out of scope') || (echo "Error: Incomplete haddocks"; exit 1)
                        '';
                      }
                    )
                  else
                    drv;

                doCheckExtension =
                  mass pkgsNew.haskell.lib.doCheck
                    (   [ "dhall-bash"
                          "dhall-docs"
                          # The test suite fails due to a relative reference
                          # to ../dhall/dhall-lang/
                          # "dhall-lsp-server"
                          "dhall-nix"
                          "dhall-nixpkgs"
                          "dhall-openapi"
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
                    "dhall-docs"
                    "dhall-json"
                    "dhall-lsp-server"
                    "dhall-nix"
                    "dhall-nixpkgs"
                    "dhall-openapi"
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
                      applyCoverage
                        (haskellPackagesNew.callCabal2nix
                          "dhall"
                          (pkgsNew.sdist ../dhall)
                          { }
                        );

                    dhall-no-http =
                      # The import tests fail with HTTP support compiled out
                      pkgsNew.haskell.lib.dontCheck
                        (pkgsNew.haskell.lib.appendConfigureFlag
                          haskellPackagesNew.dhall
                          [ "-f-with-http" ]
                        );

                    dhall-bash =
                      haskellPackagesNew.callCabal2nix
                        "dhall-bash"
                        (pkgsNew.sdist ../dhall-bash)
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

                    generic-random = haskellPackagesOld.generic-random_1_3_0_0;

                    haskeline = haskellPackagesNew.haskeline_0_8_0_0;

                    haskell-lsp = haskellPackagesNew.haskell-lsp_0_19_0_0;

                    haskell-lsp-types =
                      haskellPackagesNew.haskell-lsp-types_0_19_0_0;

                    HsYAML = haskellPackagesNew.HsYAML_0_2_1_0;

                    path = haskellPackagesNew.path_0_7_0;

                    path-io = haskellPackagesNew.path-io_1_6_0;

                    semialign = haskellPackagesNew.semialign_1_1;
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
                echo "Determined cabalPackageId as $cabalPackageId"
                setupCompileFlags="$setupCompileFlags -package-id $cabalPackageId"
              '';
            }
          );

        statify = drv:
          pkgsNew.haskell.lib.appendConfigureFlags
            (pkgsNew.haskell.lib.disableLibraryProfiling
              (pkgsNew.haskell.lib.disableSharedExecutables
                (pkgsNew.haskell.lib.useFixedCabal
                   (pkgsNew.haskell.lib.justStaticExecutables
                     (pkgsNew.haskell.lib.dontCheck drv)
                   )
                )
              )
            )
            [ "--enable-executable-static"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.ncurses.override { enableStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgsNew.pkgsMusl.zlib.static}/lib"
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

  makeTarball = name:
    pkgsStaticLinux.releaseTools.binaryTarball rec {
      src = pkgsStaticLinux.pkgsMusl.haskell.packages."${compiler}"."${name}-static";

      installPhase = ''
        releaseName=${name}
        ${pkgsStaticLinux.coreutils}/bin/install --target-directory "$TMPDIR/inst/bin" -D $src/bin/*
        ${pkgs.lib.optionalString (src ? data) ''
          ${pkgsStaticLinux.coreutils}/bin/install --target-directory "$TMPDIR/inst/share" -D ${src.data}/share/*/*/*/man
        ''
        }
      '';
    };

  toShell = drv: drv.env;

  possibly-static = {
    dhall            = makeStaticIfPossible "dhall"           ;
    dhall-bash       = makeStaticIfPossible "dhall-bash"      ;
    dhall-docs       = makeStaticIfPossible "dhall-docs"      ;
    dhall-json       = makeStaticIfPossible "dhall-json"      ;
    dhall-lsp-server = makeStaticIfPossible "dhall-lsp-server";
    dhall-nix        = makeStaticIfPossible "dhall-nix"       ;
    dhall-nixpkgs    = makeStaticIfPossible "dhall-nixpkgs"   ;
    dhall-openapi    = makeStaticIfPossible "dhall-openapi"   ;
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

    tarball-dhall            = makeTarball "dhall"           ;
    tarball-dhall-bash       = makeTarball "dhall-bash"      ;
    tarball-dhall-docs       = makeTarball "dhall-docs"      ;
    tarball-dhall-json       = makeTarball "dhall-json"      ;
    tarball-dhall-lsp-server = makeTarball "dhall-lsp-server";
    tarball-dhall-nix        = makeTarball "dhall-nix"       ;
    tarball-dhall-nixpkgs    = makeTarball "dhall-nixpkgs"   ;
    tarball-dhall-openapi    = makeTarball "dhall-openapi"   ;
    tarball-dhall-yaml       = makeTarball "dhall-yaml"      ;

    inherit (pkgs) tarball-website website;

    inherit (pkgs.haskell.packages."${compiler}")
      dhall
      dhall-no-http
      dhall-bash
      dhall-docs
      dhall-json
      dhall-lsp-server
      dhall-nix
      dhall-nixpkgs
      dhall-openapi
      dhall-try
      dhall-yaml
    ;

    inherit (pkgs.releaseTools) aggregate;

    shell-dhall            = pkgs.haskell.packages."${compiler}".dhall.env           ;
    shell-dhall-bash       = pkgs.haskell.packages."${compiler}".dhall-bash.env      ;
    shell-dhall-docs       = pkgs.haskell.packages."${compiler}".dhall-docs.env      ;
    shell-dhall-json       = pkgs.haskell.packages."${compiler}".dhall-json.env      ;
    shell-dhall-lsp-server = pkgs.haskell.packages."${compiler}".dhall-lsp-server.env;
    shell-dhall-nix        = pkgs.haskell.packages."${compiler}".dhall-nix.env       ;
    shell-dhall-nixpkgs    = pkgs.haskell.packages."${compiler}".dhall-nixpkgs.env   ;
    shell-dhall-openapi    = pkgs.haskell.packages."${compiler}".dhall-openapi.env   ;
    shell-dhall-try        = pkgs.haskell.packages."${compiler}".dhall-try.env       ;
    shell-dhall-yaml       = pkgs.haskell.packages."${compiler}".dhall-yaml.env      ;

    image-dhall            = toDockerImage "dhall"           ;
    image-dhall-bash       = toDockerImage "dhall-bash"      ;
    image-dhall-docs       = toDockerImage "dhall-docs"      ;
    image-dhall-json       = toDockerImage "dhall-json"      ;
    image-dhall-lsp-server = toDockerImage "dhall-lsp-server";
    image-dhall-nix        = toDockerImage "dhall-nix"       ;
    image-dhall-nixpkgs    = toDockerImage "dhall-nixpkgs"   ;
    image-dhall-openapi    = toDockerImage "dhall-openapi"   ;
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
