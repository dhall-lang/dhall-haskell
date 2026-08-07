{ priorityLevelConfiguration :
    ./io.k8s.api.flowcontrol.v1.PriorityLevelConfigurationReference.dhall
, distinguisherMethod :
    Optional ./io.k8s.api.flowcontrol.v1.FlowDistinguisherMethod.dhall
, matchingPrecedence : Optional Natural
, rules :
    Optional (List ./io.k8s.api.flowcontrol.v1.PolicyRulesWithSubjects.dhall)
}
