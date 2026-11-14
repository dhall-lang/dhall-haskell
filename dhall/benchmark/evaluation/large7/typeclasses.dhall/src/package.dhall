{ Classes =
    { Equality.Type = ./Classes/Equality/Type.dhall
    , Ordering = ./Classes/Ordering/package.dhall
    }
, Instances =
    { List.ordering = ./Instances/List/ordering.dhall
    , NonEmpty.ordering = ./Instances/NonEmpty/ordering.dhall
    }
}
