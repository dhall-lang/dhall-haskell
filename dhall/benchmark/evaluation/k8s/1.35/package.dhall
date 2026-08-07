  ./schemas.dhall
    sha256:5210a16452da11f25cd89ccb7f9a673573d6f9091d8a3bed4f9169bbd2e1c43d
∧ { IntOrString =
      ( ./types.dhall
          sha256:5f826daef43b9827dd4d4a67b3fafbbc032c19293dd5f09e46088edf93959c85
      ).IntOrString
  , NatOrString =
      ( ./types.dhall
          sha256:5f826daef43b9827dd4d4a67b3fafbbc032c19293dd5f09e46088edf93959c85
      ).NatOrString
  , Resource =
      ./typesUnion.dhall
        sha256:162d1ce7d0398ac4fd9ef6df15fe5d4da8e209b1d52f83762d877947e6d363cc
  }
