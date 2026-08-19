let Configuration/universal = ../../configuration/universal.dhall

let Configuration/container = ../../configuration/container.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let Cache/containers =
      { Type =
          { Cache : Configuration/container.Type
          , Exporter : Configuration/container.Type
          }
      , default =
        { Cache = Configuration/container.default
        , Exporter = Configuration/container.default
        }
      }

let Cache/deployment =
      { Type =
          { namespace : Optional Text
          , additionalAnnotations : Optional (List Util/KeyValuePair)
          , additionalLabels : Optional (List Util/KeyValuePair)
          , replicas : Optional Natural
          , Containers : Cache/containers.Type
          }
      , default =
        { namespace = None Text
        , additionalAnnotations = None (List Util/KeyValuePair)
        , additionalLabels = None (List Util/KeyValuePair)
        , replicas = None Natural
        , Containers = Cache/containers.default
        }
      }

let Cache/configuration =
      { Type =
          { Deployment : Cache/deployment.Type
          , PersistentVolumeClaim : Configuration/universal.Type
          , Service : Configuration/universal.Type
          }
      , default =
        { Deployment = Cache/deployment.default
        , PersistentVolumeClaim = Configuration/universal.default
        , Service = Configuration/universal.default
        }
      }

let Store/containers =
      { Type =
          { Store : Configuration/container.Type
          , Exporter : Configuration/container.Type
          }
      , default =
        { Store = Configuration/container.default
        , Exporter = Configuration/container.default
        }
      }

let Store/deployment =
      { Type =
          { namespace : Optional Text
          , additionalAnnotations : Optional (List Util/KeyValuePair)
          , additionalLabels : Optional (List Util/KeyValuePair)
          , replicas : Optional Natural
          , Containers : Store/containers.Type
          }
      , default =
        { namespace = None Text
        , additionalAnnotations = None (List Util/KeyValuePair)
        , additionalLabels = None (List Util/KeyValuePair)
        , replicas = None Natural
        , Containers = Store/containers.default
        }
      }

let Store/configuration =
      { Type =
          { Deployment : Store/deployment.Type
          , PersistentVolumeClaim : Configuration/universal.Type
          , Service : Configuration/universal.Type
          }
      , default =
        { Deployment = Store/deployment.default
        , PersistentVolumeClaim = Configuration/universal.default
        , Service = Configuration/universal.default
        }
      }

let configuration =
      { Type =
          { Store : Store/configuration.Type, Cache : Cache/configuration.Type }
      , default =
        { Store = Store/configuration.default
        , Cache = Cache/configuration.default
        }
      }

in  configuration
