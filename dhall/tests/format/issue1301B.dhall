let attribute
    : Text → Text → { mapKey : Text, mapValue : Text }
    = λ(key : Text) → λ(value : Text) → { mapKey = key, mapValue = value }

in  attribute
