# `dhall-haskell`

You will probably want to read the language-agnostic `README` here:

* [`dhall-lang` `README`](https://github.com/dhall-lang/dhall-lang/blob/master/README.md)

This repository focuses on the Haskell bindings to Dhall and contains
the following packages:

* [`dhall`](./dhall) - [![Hackage](https://img.shields.io/hackage/v/dhall.svg)](https://hackage.haskell.org/package/dhall)
* [`dhall-bash`](./dhall-bash) - [![Hackage](https://img.shields.io/hackage/v/dhall-bash.svg)](https://hackage.haskell.org/package/dhall-bash)
* [`dhall-json`](./dhall-json) - [![Hackage](https://img.shields.io/hackage/v/dhall-json.svg)](https://hackage.haskell.org/package/dhall-json)
* [`dhall-nix`](./dhall-nix) - [![Hackage](https://img.shields.io/hackage/v/dhall-nix.svg)](https://hackage.haskell.org/package/dhall-nix)
* [`dhall-text`](./dhall-text) - [![Hackage](https://img.shields.io/hackage/v/dhall-text.svg)](https://hackage.haskell.org/package/dhall-text)

Navigate to each package's directory for their respective `README`s

## Pre-built binaries

You can download pre-built binaries for Windows and Linux on the release page:

* [`dhall-haskell` - Releases](https://github.com/dhall-lang/dhall-haskell/releases)

For OS X, use `brew` to install the desired package.  For example:

```
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
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-nix/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-nix/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-lsp-server/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-lsp-server/latest)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-text/latest](https://hydra.dhall-lang.org/job/dhall-haskell/master/linux-dhall-text/latest)

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

You can download pre-built `docker` image archives for each package using the
following URLs:

* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall/latest/download/1/docker-image-dhall.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall/latest/download/1/docker-image-dhall.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-bash/latest/download/1/docker-image-dhall-bash.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-bash/latest/download/1/docker-image-dhall-bash.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-json/latest/download/1/docker-image-dhall-json.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-json/latest/download/1/docker-image-dhall-json.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-lsp-server/latest/download/1/docker-image-dhall-lsp-server.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-lsp-server/latest/download/1/docker-image-dhall-lsp-server.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-nix/latest/download/1/docker-image-dhall-nix.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-nix/latest/download/1/docker-image-dhall-nix.tar.gz)
* [https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-text/latest/download/1/docker-image-dhall-text.tar.gz](https://hydra.dhall-lang.org/job/dhall-haskell/master/image-dhall-text/latest/download/1/docker-image-dhall-text.tar.gz)

You can then load and run one of these archives like so:

```
$ NAME="dhall"

$ curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/master/image-${NAME}/latest/download/1/docker-image-${NAME}.tar.gz"

$ docker load < "docker-image-${NAME}.tar.gz"
...
Loaded image: dhall:vx95jiijmp2i07f5ynl8h6sllf34jggz

$ docker run -i dhall:vx95jiijmp2i07f5ynl8h6sllf34jggz dhall <<< '2 + 2'
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

You will probably want to use the shared cache hosted at `cache.dhall-lang.org`
when doing Nix development.  This is not required, but this will save you a lot
of time so that you don't have to build as many dependencies from scratch the
first time.

If your operating system is NixOS then you can add the cache using these NixOS
configuration options:

```nix
  nix = {
    binaryCaches = [ "https://cache.nixos.org" "https://cache.dhall-lang.org" ];

    binaryCachePublicKeys = [
      "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM="
    ];
  };
```

If you don't use NixOS but you do use Nix 2.0 or later, then set these options
in your `/etc/nix/nix.conf` file:

```
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM=
substituters = https://cache.nixos.org https://cache.dhall-lang.org
```

If you use an older version of Nix (i.e. Nix `1.*`) then use these options
instead:

```
binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM=
binary-caches = https://cache.nixos.org https://cache.dhall-lang.org
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

If you prefer installing the binaries locally in a nix shell environment instead, just run `nix-shell` in the top-level directory.
This option provides additional flexibility with respect to overriding some of the default parameters e.g. the compiler version, which makes it particularly useful for developers.

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

## Build the website

Building the website from source is currently only supported for Nix on Linux.

You can build the static assets by running:

```bash
$ nix-build --attr website
```

... then open `./result/index.html` in your browser.

You can also download an archive containing the pre-built website from CI using
this link:

* [website.tar.bz2](http://hydra.dhall-lang.org/job/dhall-haskell/master/tarball-website/latest/download-by-type/file/binary-dist)

## Contributing

Read the following guide if you would like to contribute:

* [Contributing to Dhall](https://github.com/dhall-lang/dhall-lang/blob/master/.github/CONTRIBUTING.md)
