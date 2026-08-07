{ args = None (List Text)
, command = None (List Text)
, env = None (List ./../types/io.k8s.api.core.v1.EnvVar.dhall)
, envFrom = None (List ./../types/io.k8s.api.core.v1.EnvFromSource.dhall)
, image = None Text
, imagePullPolicy = None Text
, lifecycle = None ./../types/io.k8s.api.core.v1.Lifecycle.dhall
, livenessProbe = None ./../types/io.k8s.api.core.v1.Probe.dhall
, ports = None (List ./../types/io.k8s.api.core.v1.ContainerPort.dhall)
, readinessProbe = None ./../types/io.k8s.api.core.v1.Probe.dhall
, resizePolicy =
    None (List ./../types/io.k8s.api.core.v1.ContainerResizePolicy.dhall)
, resources = None ./../types/io.k8s.api.core.v1.ResourceRequirements.dhall
, restartPolicy = None Text
, restartPolicyRules =
    None (List ./../types/io.k8s.api.core.v1.ContainerRestartRule.dhall)
, securityContext = None ./../types/io.k8s.api.core.v1.SecurityContext.dhall
, startupProbe = None ./../types/io.k8s.api.core.v1.Probe.dhall
, stdin = None Bool
, stdinOnce = None Bool
, targetContainerName = None Text
, terminationMessagePath = None Text
, terminationMessagePolicy = None Text
, tty = None Bool
, volumeDevices = None (List ./../types/io.k8s.api.core.v1.VolumeDevice.dhall)
, volumeMounts = None (List ./../types/io.k8s.api.core.v1.VolumeMount.dhall)
, workingDir = None Text
}
