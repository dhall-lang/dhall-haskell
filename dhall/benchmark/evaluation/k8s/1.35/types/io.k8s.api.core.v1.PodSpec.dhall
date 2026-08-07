{ containers : List ./io.k8s.api.core.v1.Container.dhall
, activeDeadlineSeconds : Optional Natural
, affinity : Optional ./io.k8s.api.core.v1.Affinity.dhall
, automountServiceAccountToken : Optional Bool
, dnsConfig : Optional ./io.k8s.api.core.v1.PodDNSConfig.dhall
, dnsPolicy : Optional Text
, enableServiceLinks : Optional Bool
, ephemeralContainers :
    Optional (List ./io.k8s.api.core.v1.EphemeralContainer.dhall)
, hostAliases : Optional (List ./io.k8s.api.core.v1.HostAlias.dhall)
, hostIPC : Optional Bool
, hostNetwork : Optional Bool
, hostPID : Optional Bool
, hostUsers : Optional Bool
, hostname : Optional Text
, hostnameOverride : Optional Text
, imagePullSecrets :
    Optional (List ./io.k8s.api.core.v1.LocalObjectReference.dhall)
, initContainers : Optional (List ./io.k8s.api.core.v1.Container.dhall)
, nodeName : Optional Text
, nodeSelector : Optional (List { mapKey : Text, mapValue : Text })
, os : Optional ./io.k8s.api.core.v1.PodOS.dhall
, overhead :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, preemptionPolicy : Optional Text
, priority : Optional Integer
, priorityClassName : Optional Text
, readinessGates : Optional (List ./io.k8s.api.core.v1.PodReadinessGate.dhall)
, resourceClaims : Optional (List ./io.k8s.api.core.v1.PodResourceClaim.dhall)
, resources : Optional ./io.k8s.api.core.v1.ResourceRequirements.dhall
, restartPolicy : Optional Text
, runtimeClassName : Optional Text
, schedulerName : Optional Text
, schedulingGates : Optional (List ./io.k8s.api.core.v1.PodSchedulingGate.dhall)
, securityContext : Optional ./io.k8s.api.core.v1.PodSecurityContext.dhall
, serviceAccount : Optional Text
, serviceAccountName : Optional Text
, setHostnameAsFQDN : Optional Bool
, shareProcessNamespace : Optional Bool
, subdomain : Optional Text
, terminationGracePeriodSeconds : Optional Natural
, tolerations : Optional (List ./io.k8s.api.core.v1.Toleration.dhall)
, topologySpreadConstraints :
    Optional (List ./io.k8s.api.core.v1.TopologySpreadConstraint.dhall)
, volumes : Optional (List ./io.k8s.api.core.v1.Volume.dhall)
, workloadRef : Optional ./io.k8s.api.core.v1.WorkloadReference.dhall
}
