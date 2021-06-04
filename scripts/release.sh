#! /usr/bin/env nix-shell
#! nix-shell -i bash --packages bash cabal-install cachix curl git skopeo

set -eux

JOBSET=master

. .travis-functions.sh

function release {
  NAME="$1"
  VERSION="$(get_cabal_version "${NAME}")"

  pushd "${NAME}"
  cabal sdist
  cabal upload --publish "../dist-newstyle/sdist/${NAME}-${VERSION}.tar.gz"
  git clean --force -d -x .
  popd

  nix build --file ./default.nix "${NAME}"

  cachix push dhall result

  curl --location --output "${NAME}-${VERSION}-x86_64-linux.tar.bz2" "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/tarball-${NAME}/latest/download/1/${NAME}.tar.bz2"

  DOCKER_ARCHIVE="docker-image-${NAME}.tar.gz"

  curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/image-${NAME}/latest/download/1/${DOCKER_ARCHIVE}"

  skopeo copy --insecure-policy --dest-creds="gabriel439:$(< dockerPassword.txt)" "docker-archive:${DOCKER_ARCHIVE}" "docker://dhallhaskell/${NAME}"

  skopeo copy --insecure-policy --dest-creds="gabriel439:$(< dockerPassword.txt)" "docker-archive:${DOCKER_ARCHIVE}" "docker://dhallhaskell/${NAME}:${VERSION}"

  rm "${DOCKER_ARCHIVE}"
}

git submodule update

for package in dhall-lsp-server dhall-openapi dhall-json dhall-yaml dhall-bash dhall-nix dhall-nixpkgs dhall-docs dhall; do
  release "${package}"
done
