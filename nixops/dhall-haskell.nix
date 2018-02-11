{ pullRequestsJSON, dhall-haskell }:

let
 pkgs = import <nixpkgs> {};

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

       value = "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";

       emailresponsible = false;
     };
   };
 };

 jobsets = pkgs.lib.mapAttrs toJobset pullRequests;

in
 { jobsets = pkgs.writeText "jobsets.json" (builtins.toJSON jobsets);
 }
