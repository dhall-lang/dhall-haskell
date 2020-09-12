{ {- Comments around duplicate fields are not preserved -} foo.bar = 1
, foo = { baz = {- Comments within values are still preserved-} 2 }
, foo = qux
}
