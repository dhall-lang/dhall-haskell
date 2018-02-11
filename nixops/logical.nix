{ ipfs = { pkgs, ... }:

    let
      pin = { name, path }:
          { name = "pin-${name}";

            value = {
              path = [ pkgs.bash ];

              script = ''
                IPFS_PATH="/var/lib/ipfs/" ${pkgs.ipfs}/bin/ipfs add --quiet -w --recursive ${path}
              '';

              serviceConfig = {
                Type = "oneshot";
              };

              wantedBy = [ "multi-user.target" ];
            };
          };

      pinPrelude = { date }:
        let
          path = ./. + "/dhall/${date}.json";

          json = builtins.fromJSON (builtins.readFile path);

          src = pkgs.fetchgit { inherit (json) url rev sha256; };
        in
          pin { name = date; path = "${src}/Prelude"; };

      services = [
        (pinPrelude { date = "2016-12-03"; })
        (pinPrelude { date = "2017-05-16"; })
        (pinPrelude { date = "2017-06-17"; })
        (pinPrelude { date = "2017-08-28"; })
      ];

    in
      { networking.firewall.allowedTCPPorts = [ 22 4001 ];

        services.ipfs = {
          enable = true;

          enableGC = true;
        };

        systemd.services = builtins.listToAttrs services // {
          ipfs.environment.IPFS_LOW_MEM = "1";
        };
      };

  hydra = { pkgs, ... }: {
    environment = {
      etc."hydra/dhall-haskell.json".text = builtins.readFile ./dhall-haskell.json;

      systemPackages = [ pkgs.hydra ];
    };

    networking.firewall.allowedTCPPorts = [ 22 80 ];

    nixpkgs.overlays =
      let
        secureHydra = packagesNew: packagesOld: {
          hydra = packagesOld.hydra.overrideAttrs (oldAttributes: {
              patches = (oldAttributes.patches or []) ++ [ ./hydra.patch ];
            }
          );
        };

      in
        [ secureHydra ];

    services = {
      hydra = {
        enable = true;

        extraConfig = ''
          <githubstatus>
            jobs = dhall-haskell:.*:*
            inputs = src
            authorization = gabriel
          </githubstatus>
        '';

        hydraURL = "https://hydra.dhall-lang.org";

        listenHost = "127.0.0.1";

        notificationSender = "noreply@dhall-lang.org";
      };

      nginx = {
        enable = true;

        recommendedProxySettings = true;

        virtualHosts."hydra.dhall-lang.org" = {
          default = true;

          listen = [ { addr = "0.0.0.0"; port = 80; } ];

          locations."/".proxyPass = "http://127.0.0.1:3000";
        };
      };
    };

    # TODO: `/etc/hydra/machines`?
    nix.gc.automatic = true;
  };
}
