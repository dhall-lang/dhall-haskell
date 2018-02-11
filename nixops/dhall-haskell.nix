{ pullRequestsJSON, dhall-haskell }:

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
     rev = "89acf89f6b214377de4fffdeca597d13241a0dd0";

     sha256 = "1jn02zfpyiabgp8qmln7s04y8rxj2d3za21r299an1prqn3smr2v";
  };

  pkgs = import nixpkgs { config = {}; };

  pullRequests = builtins.fromJSON (builtins.readFile pullRequestsJSON);

  toJobset = num: info: {
    enabled = 1;

    hidden = false;

    description = info.title;

    nixexprinput = "src";

    nixexprpath = "release.nix";

    checkinterval = 20;

    schedulingshares = 1;

    enableemail = false;

    emailoverride = "";

    keepnr = 1;

    inputs = {
      src = {
        type = "git";

        value = "https://github.com/dhall-lang/dhall-haskell.git ${info.head.ref}";

        emailresponsible = false;
      };

      nixpkgs = {
        type = "git";

        value = "https://github.com/NixOS/nixpkgs.git release-17.09";

        emailresponsible = false;
      };
    };
  };

  jobsets = pkgs.lib.mapAttrs toJobset pullRequests;

in
  { jobsets = pkgs.writeText "jobsets.json" (builtins.toJSON jobsets);
  }
