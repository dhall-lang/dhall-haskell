{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support" ]
, packages = https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210905/packages.dhall sha256:140f3630801f2b02d5f3a405d4872e0af317e4ef187016a6b00f97d59d6275c6
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

