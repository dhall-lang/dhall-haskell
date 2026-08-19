let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type = { JaegerAllInOne : Configuration/container.Type }
      , default.JaegerAllInOne = Configuration/container.default
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
          , Query : Configuration/universal.Type
          , Collector : Configuration/universal.Type
          }
      , default =
        { Deployment = Deployment.default
        , Query = Configuration/universal.default
        , Collector = Configuration/universal.default
        }
      }

in  configuration
