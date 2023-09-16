{ name = "llbm"
, dependencies =
  [ "aff", "arrays", "control", "datetime", "dom-indexed", "either", "foldable-traversable"
  , "functions", "integers", "maybe", "newtype", "numbers", "tailrec", "transformers", "tuples"
  , "effect", "prelude", "bifunctors", "exceptions"
  , "affjax", "affjax-web", "strings", "partial", "unordered-collections", "formatters"
  , "halogen", "halogen-subscriptions" 
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
