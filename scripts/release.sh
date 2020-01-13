#!/bin/sh

set -eux

JOBSET=master

. .travis-functions.sh

function release {
  NAME="$1"
  VERSION="$(get_cabal_version "${NAME}")"

  pushd "${NAME}"
  cabal v1-configure
  cabal v1-sdist
  cabal upload --publish "dist/${NAME}-${VERSION}.tar.gz"
  popd

  curl --location --output "${NAME}-${VERSION}-x86_64-linux.tar.bz2" "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/tarball-${NAME}/latest/download/1/${NAME}.tar.bz2"

  curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/image-${NAME}/latest/download/1/docker-image-${NAME}.tar.gz"

  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}"
  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}:${VERSION}"
}

for package in dhall-lsp-server dhall-json dhall-yaml dhall-bash dhall-nix dhall; do
  release "${package}"
done
