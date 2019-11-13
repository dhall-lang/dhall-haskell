JOBSET=master

function release {
  NAME="$1"
  VERSION="$2"

  cabal v1-configure
  cabal v1-sdist
  cabal upload --publish "dist/${NAME}-${VERSION}.tar.gz"

  curl --location --output "${NAME}-${VERSION}-x86_64-linux.tar.bz2" "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/tarball-${NAME}/latest/download/1/${NAME}.tar.bz2"

  curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/image-${NAME}/latest/download/1/docker-image-${NAME}.tar.gz"

  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}"
  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}:${VERSION}"
}

release dhall-lsp-server 1.0.2
release dhall-json 1.5.0
release dhall-yaml 1.5.0
release dhall-bash 1.0.24
release dhall-nix 1.1.9
release dhall 1.27.0
