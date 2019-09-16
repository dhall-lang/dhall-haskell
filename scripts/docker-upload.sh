JOBSET=master

function upload {
  NAME="$1"
  VERSION="$2"

  curl --location --remote-name "https://hydra.dhall-lang.org/job/dhall-haskell/${JOBSET}/image-${NAME}/latest/download/1/docker-image-${NAME}.tar.gz"

  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}"
  skopeo copy --dest-creds=gabriel439:$(< dockerPassword.txt) "docker-archive:docker-image-${NAME}.tar.gz" "docker://dhallhaskell/${NAME}:${VERSION}"
}

upload dhall 1.26.0
upload dhall-json 1.4.1
upload dhall-bash 1.0.23
upload dhall-nix 1.1.8
upload dhall-lsp-server 1.0.1
