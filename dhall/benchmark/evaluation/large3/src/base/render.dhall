let Frontend/Render = ./frontend/render.dhall

let Cadvisor/Render = ./cadvisor/render.dhall

let GithubProxy/Render = ./github-proxy/render.dhall

let Gitserver/Render = ./gitserver/render.dhall

let IndexedSearch/Render = ./indexed-search/render.dhall

let Jaeger/Render = ./jaeger/render.dhall

let Postgres/Render = ./postgres/render.dhall

let PreciseCodeIntel/Render = ./precise-code-intel/render.dhall

let Prometheus/Render = ./prometheus/render.dhall

let QueryRunner/Render = ./query-runner/render.dhall

let Redis/Render = ./redis/render.dhall

let Replacer/Render = ./replacer/render.dhall

let RepoUpdater/Render = ./repo-updater/render.dhall

let Searcher/Render = ./searcher/render.dhall

let Symbols/Render = ./symbols/render.dhall

let SyntaxHighlighter/Render = ./syntax-highlighter/render.dhall

let StorageClass/Render = ./storage-class/render.dhall

let IngressNginx/Render = ./ingress-nginx/render.dhall

let Configuration/global = ../configuration/global.dhall

let List/concatMap =
      ../../../../../dhall-lang/Prelude/List/concatMap.dhall

let Kubernetes/List = ../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Render =
        ( λ(c : Configuration/global.Type) →
            let allResourceLists =
                  [ Postgres/Render c
                  , Gitserver/Render c
                  , Frontend/Render c
                  , IndexedSearch/Render c
                  , Symbols/Render c
                  , Jaeger/Render c
                  , SyntaxHighlighter/Render c
                  , Searcher/Render c
                  , RepoUpdater/Render c
                  , GithubProxy/Render c
                  , Cadvisor/Render c
                  , PreciseCodeIntel/Render c
                  , Redis/Render c
                  , Replacer/Render c
                  , QueryRunner/Render c
                  , Prometheus/Render c
                  , StorageClass/Render c
                  , IngressNginx/Render c
                  ]

            in  Kubernetes/List::{
                , items =
                    List/concatMap
                      Kubernetes/List.Type
                      Kubernetes/TypesUnion
                      (λ(x : Kubernetes/List.Type) → x.items)
                      allResourceLists
                }
        )
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
