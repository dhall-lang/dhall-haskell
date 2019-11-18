let Tagged
    : Type → Type
    =   λ(a : Type)
      → { field : Text
        , nesting :
              ./Nesting sha256:6284802edd41d5d725aa1ec7687e614e21ad1be7e14dd10996bfa9625105c335
            ? ./Nesting
        , contents : a
        }

in  Tagged
