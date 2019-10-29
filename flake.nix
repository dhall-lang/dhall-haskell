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

  outputs = { self, nixpkgs, nixpkgsStaticLinux }: {
    hydraJobs = import ./release.nix {
      inherit nixpkgs nixpkgsStaticLinux;
      system = "x86_64-linux";
    };
    packages = with self.hydraJobs; {
      dhall = linux-dhall;
      dhall-bash = linux-dhall-bash;
      dhall-json = linux-dhall-json;
      dhall-lsp-server = linux-dhall-lsp-server;
      dhall-nix = linux-dhall-nix;
    };
    defaultPackage = self.packages.dhall;
    checks = { inherit (self.hydraJobs) coverage-dhall; };
  };

}
