{
  edition = 201909;

  description = "The non-repetitive alternative to YAML";

  inputs = {

    nixpkgs = {
      uri =
        "git+https://github.com/nixos/nixpkgs.git?ref=18.09-beta&rev=1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";
      flake = false;
    };

    nixpkgsStaticLinux = {
      uri =
        "git+https://github.com/nh2/nixpkgs.git?ref=static-haskell-nix-stack-dhall-working";
      flake = false;
    };

  };

  outputs = { self, nixpkgs, nixpkgsStaticLinux }:
    let
      inherit (builtins) getAttr;

      systems = [ "x86_64-linux" ];

      mkOutput = system:
        let getDhall = attrs: getAttr "dhall" (getAttr system attrs);
        in rec {

          hydraJobs =
            import ./release.nix { inherit system nixpkgs nixpkgsStaticLinux; };

          packages =
            # the packages are taken from the hydraJobs,
            # which is the evaluation of release.nix
            with (hydraJobs); {
              dhall = linux-dhall;
              dhall-bash = linux-dhall-bash;
              dhall-json = linux-dhall-json;
              dhall-nix = linux-dhall-nix;
            };

          checks = { inherit (hydraJobs) coverage-dhall; };

          apps = let
            mkApp = { drv, name ? drv.pname or drv.name, exe ? name }: {
              inherit name;
              value = {
                type = "app";
                program = "${drv}/bin/${exe}";
              };
            };
          in builtins.listToAttrs [
            (mkApp { drv = packages.dhall; })

            (mkApp {
              drv = packages.dhall-bash;
              name = "dhall-to-bash";
            })

            (mkApp {
              drv = packages.dhall-json;
              name = "dhall-to-json";
            })

            (mkApp {
              drv = packages.dhall-json;
              name = "json-to-dhall";
            })

            (mkApp {
              drv = packages.dhall-json;
              name = "dhall-to-yaml";
            })

            (mkApp {
              drv = packages.dhall-json;
              name = "yaml-to-dhall";
            })

            (mkApp {
              drv = packages.dhall-nix;
              name = "dhall-to-nix";
            })

          ];

          defaultApp = getDhall self.apps;

          defaultPackage = getDhall self.packages;
        };

      forAllSystems = f:
        with builtins;
        let
          f' = output: system:
            let
              attrs = f system;
              f' = name: {
                inherit name;
                value = output.${name} or { } // {
                  ${system} = getAttr name attrs;
                };
              };
              attrs' = listToAttrs (map f' (attrNames attrs));
            in attrs';
        in foldl' f' { } systems;

    in forAllSystems mkOutput;

}
