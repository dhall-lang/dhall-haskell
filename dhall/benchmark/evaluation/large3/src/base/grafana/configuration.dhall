let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type = { Grafana : Configuration/container.Type }
      , default.Grafana = Configuration/container.default
      }

let statefulset =
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
          { StatefulSet : statefulset.Type
          , Service : Configuration/universal.Type
          , ServiceAccount : Configuration/universal.Type
          , ConfigMap : Configuration/universal.Type
          }
      , default =
        { StatefulSet = statefulset.default
        , Service = Configuration/universal.default
        , ServiceAccount = Configuration/universal.default
        , ConfigMap = Configuration/universal.default
        }
      }

in  configuration
