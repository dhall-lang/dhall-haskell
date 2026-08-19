let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type =
          { Prometheus : Configuration/container.Type
          , Blackbox : Configuration/container.Type
          }
      , default =
        { Prometheus = Configuration/container.default
        , Blackbox = Configuration/container.default
        }
      }

let Deployment =
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
          { Deployment : Deployment.Type
          , ClusterRole : Configuration/universal.Type
          , ClusterRoleBinding : Configuration/universal.Type
          , Service : Configuration/universal.Type
          , ServiceAccount : Configuration/universal.Type
          , ConfigMap : Configuration/universal.Type
          }
      , default =
        { Deployment = Deployment.default
        , ClusterRole = Configuration/universal.default
        , ClusterRoleBinding = Configuration/universal.default
        , Service = Configuration/universal.default
        , ServiceAccount = Configuration/universal.default
        , ConfigMap = Configuration/universal.default
        }
      }

in  configuration
