let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type =
          { Postgres : Configuration/container.Type
          , PostgresExporter : Configuration/container.Type
          , Init : Configuration/container.Type
          }
      , default =
        { Postgres = Configuration/container.default
        , PostgresExporter = Configuration/container.default
        , Init = Configuration/container.default
        }
      }

let deployment =
      { Type =
          { namespace : Optional Text
          , additionalAnnotations : Optional (List Util/KeyValuePair)
          , additionalLabels : Optional (List Util/KeyValuePair)
          , replicas : Optional Natural
          , Containers : containers.Type
          }
      , default =
        { namespace = None Text
        , additionalAnnotations = None (List Util/KeyValuePair)
        , additionalLabels = None (List Util/KeyValuePair)
        , replicas = None Natural
        , Containers = containers.default
        }
      }

let configuration =
      { Type =
          { Deployment : deployment.Type
          , PersistentVolumeClaim : Configuration/universal.Type
          , ConfigMap : Configuration/universal.Type
          , Service : Configuration/universal.Type
          }
      , default =
        { Deployment = deployment.default
        , PersistentVolumeClaim = Configuration/universal.default
        , ConfigMap = Configuration/universal.default
        , Service = Configuration/universal.default
        }
      }

in  configuration
