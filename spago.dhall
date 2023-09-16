{ name = "llbm"
, dependencies =
  [ "aff", "arrays", "control", "datetime", "dom-indexed", "either", "foldable-traversable", 
  , "functions", "integers", "maybe", "newtype", "numbers", "tailrec", "transformers", "tuples"
  , "effect", "prelude", "bifunctors", "exceptions"
  , "affjax", "affjax-web", "strings", "partial", "unordered-collections", "formatters"
  , "halogen", "halogen-subscriptions" ]
, packages =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230912/packages.dhall
    sha256:010c8d977b77ff7f52f8b5e650b9f0a8ee10d10e79cca284b2016765a8afaabf
, sources = [ "src/**/*.purs" ]
}
