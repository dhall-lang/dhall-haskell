{ appArmorProfile : Optional ./io.k8s.api.core.v1.AppArmorProfile.dhall
, fsGroup : Optional Natural
, fsGroupChangePolicy : Optional Text
, runAsGroup : Optional Natural
, runAsNonRoot : Optional Bool
, runAsUser : Optional Natural
, seLinuxChangePolicy : Optional Text
, seLinuxOptions : Optional ./io.k8s.api.core.v1.SELinuxOptions.dhall
, seccompProfile : Optional ./io.k8s.api.core.v1.SeccompProfile.dhall
, supplementalGroups : Optional (List Natural)
, supplementalGroupsPolicy : Optional Text
, sysctls : Optional (List ./io.k8s.api.core.v1.Sysctl.dhall)
, windowsOptions :
    Optional ./io.k8s.api.core.v1.WindowsSecurityContextOptions.dhall
}
