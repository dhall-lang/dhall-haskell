let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let containers =
      { Type = { SourcegraphFrontend : Configuration/container.Type }
      , default.SourcegraphFrontend = Configuration/container.default
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

let Ingress =
      { Type =
          { namespace : Optional Text
          , additionalAnnotations : Optional (List Util/KeyValuePair)
          , additionalLabels : Optional (List Util/KeyValuePair)
          , tls :
              Optional
                ( List
                    { hosts : Optional (List Text), secretName : Optional Text }
                )
          }
      , default =
        { namespace = None Text
        , additionalAnnotations = None (List Util/KeyValuePair)
        , additionalLabels = None (List Util/KeyValuePair)
        , tls =
            None
              ( List
                  { hosts : Optional (List Text), secretName : Optional Text }
              )
        }
      }

let configuration =
      { Type =
          { Deployment : Deployment.Type
          , Ingress : Ingress.Type
          , Role : Configuration/universal.Type
          , RoleBinding : Configuration/universal.Type
          , Service : Configuration/universal.Type
          , ServiceAccount : Configuration/universal.Type
          , ServiceInternal : Configuration/universal.Type
          }
      , default =
        { Deployment = Deployment.default
        , Ingress = Ingress.default
        , Role = Configuration/universal.default
        , RoleBinding = Configuration/universal.default
        , Service = Configuration/universal.default
        , ServiceAccount = Configuration/universal.default
        , ServiceInternal = Configuration/universal.default
        }
      }

in  configuration
