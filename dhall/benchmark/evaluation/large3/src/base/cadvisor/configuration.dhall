let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type = { Cadvisor : Configuration/container.Type }
      , default.Cadvisor = Configuration/container.default
      }

let DaemonSet =
      { Type =
          { namespace : Optional Text
          , additionalAnnotations : Optional (List Util/KeyValuePair)
          , additionalLabels : Optional (List Util/KeyValuePair)
          , Containers : containers.Type
          }
      , default =
        { namespace = None Text
        , additionalAnnotations = None (List Util/KeyValuePair)
        , additionalLabels = None (List Util/KeyValuePair)
        , Containers = containers.default
        }
      }

let configuration =
      { Type =
          { DaemonSet : DaemonSet.Type
          , ClusterRole : Configuration/universal.Type
          , ClusterRoleBinding : Configuration/universal.Type
          , PodSecurityPolicy : Configuration/universal.Type
          , ServiceAccount : Configuration/universal.Type
          }
      , default =
        { DaemonSet = DaemonSet.default
        , ClusterRole = Configuration/universal.default
        , ClusterRoleBinding = Configuration/universal.default
        , PodSecurityPolicy = Configuration/universal.default
        , ServiceAccount = Configuration/universal.default
        }
      }

in  configuration
