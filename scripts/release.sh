#! /usr/bin/env nix-shell
#! nix-shell -i bash --packages bash cabal-install cachix git skopeo ghc

set -eux

get_cabal_version() {
  grep '^Version: ' < "$1/$1.cabal" | sed -e 's/^Version:  *//g';
}

function release {
  NAME="$1"
  VERSION="$(get_cabal_version "${NAME}")"

  pushd "${NAME}"
  cabal sdist
  cabal upload --publish "../dist-newstyle/sdist/${NAME}-${VERSION}.tar.gz" || :
  popd

  BUILD="$(nix build --file ./default.nix "${NAME}" --print-out-paths)"

  cachix push dhall "${BUILD}"

  DOCKER_ARCHIVE_DIRECTORY="$(nix build --file ./release.nix "image-${NAME}" --print-out-paths)"
  DOCKER_ARCHIVE="${DOCKER_ARCHIVE_DIRECTORY}/docker-image-${NAME}.tar.gz"

  skopeo copy --insecure-policy --dest-creds="gabriel439:$(< dockerPassword.txt)" "docker-archive:${DOCKER_ARCHIVE}" "docker://dhallhaskell/${NAME}"

  skopeo copy --insecure-policy --dest-creds="gabriel439:$(< dockerPassword.txt)" "docker-archive:${DOCKER_ARCHIVE}" "docker://dhallhaskell/${NAME}:${VERSION}"
}

git submodule update

git clean --force -d -x -- dhall*

for package in dhall-lsp-server dhall-openapi dhall-toml dhall-csv dhall-json dhall-yaml dhall-bash dhall-nix dhall-nixpkgs dhall-docs dhall; do
  release "${package}"
done
