{ machine = { pkgs, ... }:

    let
      pin = { name, path }:
          { name = "pin-${name}";

            value = {
              path = [ pkgs.bash ];

              script = ''
                IPFS_PATH="/var/lib/ipfs/.ipfs" ${pkgs.ipfs}/bin/ipfs add --quiet -w --recursive ${path}
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
      ];

    in
      { networking.firewall.allowedTCPPorts = [
          22
          4001
        ];

        services.ipfs = {
          enable = true;

          enableGC = true;
        };

        systemd.services = builtins.listToAttrs services // {
          ipfs.environment.IPFS_LOW_MEM = "1";
        };
      };
}
