{
    field: Text,
    nested: { nested_field: Natural },
    list: List Bool,
    optional: Optional Text,
    union: < A: Natural | B: Text | C >
}