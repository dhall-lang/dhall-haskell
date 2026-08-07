{ activeDeadlineSeconds = None Natural
, affinity = None ./../types/io.k8s.api.core.v1.Affinity.dhall
, automountServiceAccountToken = None Bool
, dnsConfig = None ./../types/io.k8s.api.core.v1.PodDNSConfig.dhall
, dnsPolicy = None Text
, enableServiceLinks = None Bool
, ephemeralContainers =
    None (List ./../types/io.k8s.api.core.v1.EphemeralContainer.dhall)
, hostAliases = None (List ./../types/io.k8s.api.core.v1.HostAlias.dhall)
, hostIPC = None Bool
, hostNetwork = None Bool
, hostPID = None Bool
, hostUsers = None Bool
, hostname = None Text
, hostnameOverride = None Text
, imagePullSecrets =
    None (List ./../types/io.k8s.api.core.v1.LocalObjectReference.dhall)
, initContainers = None (List ./../types/io.k8s.api.core.v1.Container.dhall)
, nodeName = None Text
, nodeSelector = None (List { mapKey : Text, mapValue : Text })
, os = None ./../types/io.k8s.api.core.v1.PodOS.dhall
, overhead =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, preemptionPolicy = None Text
, priority = None Integer
, priorityClassName = None Text
, readinessGates =
    None (List ./../types/io.k8s.api.core.v1.PodReadinessGate.dhall)
, resourceClaims =
    None (List ./../types/io.k8s.api.core.v1.PodResourceClaim.dhall)
, resources = None ./../types/io.k8s.api.core.v1.ResourceRequirements.dhall
, restartPolicy = None Text
, runtimeClassName = None Text
, schedulerName = None Text
, schedulingGates =
    None (List ./../types/io.k8s.api.core.v1.PodSchedulingGate.dhall)
, securityContext = None ./../types/io.k8s.api.core.v1.PodSecurityContext.dhall
, serviceAccount = None Text
, serviceAccountName = None Text
, setHostnameAsFQDN = None Bool
, shareProcessNamespace = None Bool
, subdomain = None Text
, terminationGracePeriodSeconds = None Natural
, tolerations = None (List ./../types/io.k8s.api.core.v1.Toleration.dhall)
, topologySpreadConstraints =
    None (List ./../types/io.k8s.api.core.v1.TopologySpreadConstraint.dhall)
, volumes = None (List ./../types/io.k8s.api.core.v1.Volume.dhall)
, workloadRef = None ./../types/io.k8s.api.core.v1.WorkloadReference.dhall
}
