# IPFS mirror for Dhall Prelude

This directory contains a NixOps specification for hosting an IPFS mirror for
the Dhall Prelude on AWS using a `t2.nano` instance

To host your own mirror, install NixOps, and run:

```bash
$ nixops create -d ipfs logical.nix physical.nix
$ nixops deploy -d ipfs
```
