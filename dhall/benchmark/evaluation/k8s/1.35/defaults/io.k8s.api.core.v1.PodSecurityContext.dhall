{ appArmorProfile = None ./../types/io.k8s.api.core.v1.AppArmorProfile.dhall
, fsGroup = None Natural
, fsGroupChangePolicy = None Text
, runAsGroup = None Natural
, runAsNonRoot = None Bool
, runAsUser = None Natural
, seLinuxChangePolicy = None Text
, seLinuxOptions = None ./../types/io.k8s.api.core.v1.SELinuxOptions.dhall
, seccompProfile = None ./../types/io.k8s.api.core.v1.SeccompProfile.dhall
, supplementalGroups = None (List Natural)
, supplementalGroupsPolicy = None Text
, sysctls = None (List ./../types/io.k8s.api.core.v1.Sysctl.dhall)
, windowsOptions =
    None ./../types/io.k8s.api.core.v1.WindowsSecurityContextOptions.dhall
}
