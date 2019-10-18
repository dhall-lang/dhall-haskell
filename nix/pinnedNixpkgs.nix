let
  fetchNixpkgs =
    { rev # The Git revision of nixpkgs to fetch
    , owner ? "NixOS" # Owner of the Github repository
    , sha256 # The SHA256 hash of the unpacked archive
    , system ? builtins.currentSystem # This is overridable if necessary
    }:

    if (0 <= builtins.compareVersions builtins.nixVersion "1.12")

    # In Nix 1.12, we can just give a `sha256` to `builtins.fetchTarball`.
    then
      (builtins.fetchTarball {
        url = "https://github.com/${owner}/nixpkgs/archive/${rev}.tar.gz";
        inherit sha256;
      })

      # This hack should at least work for Nix 1.11
    else
      ((rec {
        tarball = import <nix/fetchurl.nix> {
          url = "https://github.com/${owner}/nixpkgs/archive/${rev}.tar.gz";
          sha256 = null;
        };

        builtin-paths = import <nix/config.nix>;

        script = builtins.toFile "nixpkgs-unpacker" ''
          "$coreutils/mkdir" "$out"
          cd "$out"
          "$gzip" --decompress < "$tarball" | "$tar" -x --strip-components=1
        '';

        nixpkgs = builtins.derivation ({
          name = "nixpkgs-${builtins.substring 0 6 rev}";

          builder = builtins.storePath builtin-paths.shell;

          args = [ script ];

          inherit tarball system;

          tar = builtins.storePath builtin-paths.tar;
          gzip = builtins.storePath builtin-paths.gzip;
          coreutils = builtins.storePath builtin-paths.coreutils;
        } // (if null == sha256 then
          { }
        else {
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = sha256;
        }));
      }).nixpkgs);
in {

  nixpkgs = fetchNixpkgs {
    rev = "1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";
    sha256 = "0xpqc1fhkvvv5dv1zmas2j1q27mi7j7dgyjcdh82mlgl1q63i660";
  };

  nixpkgsStaticLinux = fetchNixpkgs {
    owner = "nh2";
    rev = "925aac04f4ca58aceb83beef18cb7dae0715421b";
    sha256 = "1zr8lscjl2a5cz61f0ibyx55a94v8yyp6sjzjl2gkqjrjbg99abx";
  };

}
