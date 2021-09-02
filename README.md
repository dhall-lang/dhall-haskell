# `dhall-haskell`

You will probably want to read the language-agnostic `README` here:

* [`dhall-lang` `README`](https://github.com/dhall-lang/dhall-lang/blob/master/README.md)

This repository focuses on the Haskell bindings to Dhall and contains
the following packages:

* [`dhall`](./dhall) - [![Hackage](https://img.shields.io/hackage/v/dhall.svg)](https://hackage.haskell.org/package/dhall)
* [`dhall-bash`](./dhall-bash) - [![Hackage](https://img.shields.io/hackage/v/dhall-bash.svg)](https://hackage.haskell.org/package/dhall-bash)
* [`dhall-json`](./dhall-json) - [![Hackage](https://img.shields.io/hackage/v/dhall-json.svg)](https://hackage.haskell.org/package/dhall-json)
* [`dhall-yaml`](./dhall-yaml) - [![Hackage](https://img.shields.io/hackage/v/dhall-yaml.svg)](https://hackage.haskell.org/package/dhall-yaml)
* [`dhall-nix`](./dhall-nix) - [![Hackage](https://img.shields.io/hackage/v/dhall-nix.svg)](https://hackage.haskell.org/package/dhall-nix)
* [`dhall-lsp-server`](./dhall-lsp-server) - [![Hackage](https://img.shields.io/hackage/v/dhall-lsp-server.svg)](https://hackage.haskell.org/package/dhall-lsp-server)
* [`dhall-docs`](./dhall-docs) - [![Hackage](https://img.shields.io/hackage/v/dhall-docs.svg)](https://hackage.haskell.org/package/dhall-docs)
* [`dhall-toml`](./dhall-toml) - [![Hackage](https://img.shields.io/hackage/v/dhall-toml.svg)](https://hackage.haskell.org/package/dhall-toml)
* [`dhall-csv`](./dhall-csv) - [![Hackage](https://img.shields.io/hackage/v/dhall-csv.svg)](https://hackage.haskell.org/package/dhall-csv)

Navigate to each package's directory for their respective `README`s

## Pre-built binaries

You can download pre-built binaries for Windows, OS X and Linux on the release page:

* [`dhall-haskell` - Releases](https://github.com/dhall-lang/dhall-haskell/releases)

You can also install binaries for OS X using `brew`, like this:

```bash
$ brew install dhall-json
```

You can also install pre-built Linux binaries for `master` using Nix using
Nix's channel mechanism by following the instructions at this link:

* [https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest](https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest)

To install the Nix build products without a channel, configure your machine to
use `cache.dhall-lang.org`, as described in the [nix](#nix) section and then
visit one of the following links:

* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-bash/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-bash/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-json/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-json/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-yaml/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-yaml/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-nix/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-nix/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-lsp-server/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-lsp-server/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-docs/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-docs/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-toml/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-toml/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-csv/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-csv/latest)

You can then click the "Help" button in the bottom right corner, which will
show you a `nix-env` command that you can run to install the prebuilt
executable.

If you have the `jq` command-line tool installed then you can do this in one
command by running:

```haskell
$ nix-env -i "$(curl -L https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall/latest/api/get-info | jq -r .outPath)"
```

These instructions also work for any pull request, too, by replacing `master`
with the pull request number for any of the above URLs.

## Pre-built containers

Prebuilt containers for the latest release available from Docker Hub:

* [Docker Hub - `dhallhaskell`](https://hub.docker.com/u/dhallhaskell)

... but if you want to obtain containers for bleeding-edge builds, you can
download image archives for each package using the following URLs:

* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall/latest/download/1/docker-image-dhall.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall/latest/download/1/docker-image-dhall.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-bash/latest/download/1/docker-image-dhall-bash.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-bash/latest/download/1/docker-image-dhall-bash.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-json/latest/download/1/docker-image-dhall-json.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-json/latest/download/1/docker-image-dhall-json.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-yaml/latest/download/1/docker-image-dhall-yaml.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-yaml/latest/download/1/docker-image-dhall-yaml.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-lsp-server/latest/download/1/docker-image-dhall-lsp-server.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-lsp-server/latest/download/1/docker-image-dhall-lsp-server.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-nix/latest/download/1/docker-image-dhall-nix.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-nix/latest/download/1/docker-image-dhall-nix.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-docs/latest/download/1/docker-image-dhall-docs.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-docs/latest/download/1/docker-image-dhall-docs.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-toml/latest/download/1/docker-image-dhall-toml.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-toml/latest/download/1/docker-image-dhall-toml.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-csv/latest/download/1/docker-image-dhall-csv.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-csv/latest/download/1/docker-image-dhall-csv.tar.gz)

You can then load and run one of these archives like so:

```bash
$ NAME="dhall"

$ curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/master/image-${NAME}/latest/download/1/docker-image-${NAME}.tar.gz"

$ docker load < "docker-image-${NAME}.tar.gz"
...
Loaded image: dhall:vx95jiijmp2i07f5ynl8h6sllf34jggz

$ docker run --interactive --rm dhall:vx95jiijmp2i07f5ynl8h6sllf34jggz dhall <<< '2 + 2'
4
```

These instructions also work for any pull request, too, by replacing `master`
with the pull request number for any of the above URLs.

## Building from source

For all of the following instructions, make sure to first check out the
`dhall-lang` submodule:

```bash
$ git submodule init
$ git submodule update
```

### [cabal](https://www.haskell.org/cabal)

You can build all of the packages by running:

```console
$ cabal new-build all
```

And each of them with `cabal new-build <package-name>`, for example:

```console
$ cabal new-build dhall
```

... or you can run `cabal new-build` within each package directory.

### [nix](https://nixos.org/nix/)

You will probably want to use the shared caches hosted at `cache.dhall-lang.org`
and `dhall.cachix.org` when doing Nix development.  This is not required, but
this will save you a lot of time so that you don't have to build as many
dependencies from scratch the first time.

If your operating system is NixOS then you can add the cache using these NixOS
configuration options to access dhall packages from your declarative configuration file:

```nix
  nix = {
    binaryCaches = [
      "https://cache.nixos.org"
      "https://cache.dhall-lang.org"
      "https://dhall.cachix.org"
    ];

    binaryCachePublicKeys = [
      "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM="
      "dhall.cachix.org-1:8laGciue2JBwD49ICFtg+cIF8ddDaW7OFBjDb/dHEAo="
    ];
  };
```

If you are not using NixOS, then instead modify your `/etc/nix/nix.conf` file
by adding the following options.

Using Nix 2.0 or later:

```
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM= dhall.cachix.org-1:8laGciue2JBwD49ICFtg+cIF8ddDaW7OFBjDb/dHEAo=
substituters = https://cache.nixos.org https://cache.dhall-lang.org https://dhall.cachix.org
```

Using earlier Nix versions (i.e. Nix `1.*`):

```
binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM= dhall.cachix.org-1:8laGciue2JBwD49ICFtg+cIF8ddDaW7OFBjDb/dHEAo=
binary-caches = https://cache.nixos.org https://cache.dhall-lang.org https://dhall.cachix.org
```

Since many tests require HTTP access, you should also add this setting to
your `/etc/nix/nix.conf`:

```
sandbox = false
```

You can build all of the packages by running:

```console
$ nix-build
```

... or you can run `nix-build` within each package's respective directory to
build just that one package.

You can install all of the packages by running:

```
$ nix-env --install --file default.nix
```

... or you can run the same command within each package's respective directory
to install just that one package.

If you prefer installing the binaries locally in a nix shell environment
instead, just run `nix-shell` in the top-level directory.  This option provides
additional flexibility with respect to overriding some of the default parameters
(e.g. the compiler version), which makes it particularly useful for developers.

You can develop any package by navigating to that package's directory and
running:

```bash
$ nix-shell
[nix-shell]$ cabal configure
[nix-shell]$ cabal build
[nix-shell]$ cabal test
```

... or you can add `nix: True` to your `~/.cabal/config` file and then you can
run the same `cabal` commands without an explicit `nix-shell`:

```bash
$ cabal configure
$ cabal build
$ cabal test
```

### [stack](https://docs.haskellstack.org)

You can build all of the packages with

```console
$ stack build
```

And each of them with `stack build <package-name>`, for example:

```console
$ stack build dhall-json
```

## Profiling

Say you want to profile the `dhall-to-json` executable.

Build the containing package with profiling options:

```console
$ stack build --profile --library-profiling dhall-json
```

Run the executable on your input. For example:

```console
$ stack exec --profile --rts-options -p -- dhall-to-json <<< 'True && False'
```

This generates a `dhall-to-json.prof` file in your current directory.

## Build the website

If you don't need to change the GHCJS code, then switch to the `dhall-lang`
repository and follow these instructions instead:

* [`dhall-lang` - Build the website](https://github.com/dhall-lang/dhall-lang/blob/master/nixops/README.md#updating-dhall-langorg)

If you do need to test changes to the GHCJS code (i.e. the
[`./dhall-try`](./dhall-try) subdirectory) then stay within this repository, but
edit the `dhall/dhall-lang` submodule to make the following change:

```diff
diff --git a/release.nix b/release.nix
--- a/dhall/dhall-lang/release.nix
+++ b/dhall/dhall-lang/release.nix
       let
         json = builtins.fromJSON (builtins.readFile ./nixops/dhall-haskell.json);
 
-        dhall-haskell =
-          pkgs.fetchFromGitHub {
-            owner = "dhall-lang";
-
-            repo = "dhall-haskell";
-
-            inherit (json) rev sha256 fetchSubmodules;
-          };
+        dhall-haskell = ../..;
 
       in
         import "${dhall-haskell}/default.nix";
```

... and then build the website by running:

```bash
$ nix build --file dhall/dhall-lang/release.nix website
```

... which will incorporate any GHCJS-related changes you make

## Contributing

Read the following guide if you would like to contribute:

* [Contributing to Dhall](https://github.com/dhall-lang/dhall-lang/blob/master/.github/CONTRIBUTING.md)
