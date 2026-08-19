let Frontend/configuration = ../base/frontend/configuration.dhall

let Gitserver/configuration = ../base/gitserver/configuration.dhall

let GithubProxy/configuration = ../base/github-proxy/configuration.dhall

let Postgres/configuration = ../base/postgres/configuration.dhall

let IndexedSearch/configuration = ../base/indexed-search/configuration.dhall

let Symbols/configuration = ../base/symbols/configuration.dhall

let Jaeger/configuration = ../base/jaeger/configuration.dhall

let SyntaxHighlighter/configuration =
      ../base/syntax-highlighter/configuration.dhall

let RepoUpdater/configuration = ../base/repo-updater/configuration.dhall

let Cadvisor/configuration = ../base/cadvisor/configuration.dhall

let Grafana/configuration = ../base/grafana/configuration.dhall

let PreciseCodeIntel/configuration =
      ../base/precise-code-intel/configuration.dhall

let Redis/configuration = ../base/redis/configuration.dhall

let Replacer/configuration = ../base/replacer/configuration.dhall

let QueryRunner/configuration = ../base/query-runner/configuration.dhall

let Prometheus/configuration = ../base/prometheus/configuration.dhall

let Searcher/configuration = ../base/searcher/configuration.dhall

let StorageClass/configuration = ../base/storage-class/configuration.dhall

let IngressNginx/configuration = ../base/ingress-nginx/configuration.dhall

let configuration =
      { Type =
          { Frontend : Frontend/configuration.Type
          , Gitserver : Gitserver/configuration.Type
          , GithubProxy : GithubProxy/configuration.Type
          , Postgres : Postgres/configuration.Type
          , IndexedSearch : IndexedSearch/configuration.Type
          , Symbols : Symbols/configuration.Type
          , Jaeger : Jaeger/configuration.Type
          , SyntaxHighlighter : SyntaxHighlighter/configuration.Type
          , Searcher : Searcher/configuration.Type
          , RepoUpdater : RepoUpdater/configuration.Type
          , Cadvisor : Cadvisor/configuration.Type
          , Grafana : Grafana/configuration.Type
          , PreciseCodeIntel : PreciseCodeIntel/configuration.Type
          , Redis : Redis/configuration.Type
          , Replacer : Replacer/configuration.Type
          , QueryRunner : QueryRunner/configuration.Type
          , Prometheus : Prometheus/configuration.Type
          , StorageClass : StorageClass/configuration.Type
          , IngressNginx : IngressNginx/configuration.Type
          }
      , default =
        { Frontend = Frontend/configuration.default
        , Gitserver = Gitserver/configuration.default
        , GithubProxy = GithubProxy/configuration.default
        , Postgres = Postgres/configuration.default
        , IndexedSearch = IndexedSearch/configuration.default
        , Symbols = Symbols/configuration.default
        , Jaeger = Jaeger/configuration.default
        , SyntaxHighlighter = SyntaxHighlighter/configuration.default
        , Searcher = Searcher/configuration.default
        , RepoUpdater = RepoUpdater/configuration.default
        , Cadvisor = Cadvisor/configuration.default
        , Grafana = Grafana/configuration.default
        , PreciseCodeIntel = PreciseCodeIntel/configuration.default
        , Redis = Redis/configuration.default
        , Replacer = Replacer/configuration.default
        , QueryRunner = QueryRunner/configuration.default
        , Prometheus = Prometheus/configuration.default
        , StorageClass = StorageClass/configuration.default
        , IngressNginx = IngressNginx/configuration.default
        }
      }

in  configuration
