{ name = "llbm"
, dependencies =
  [ "aff", "arrays", "control", "datetime", "dom-indexed", "either", "foldable-traversable"
  , "functions", "integers", "maybe", "newtype", "numbers", "tailrec", "transformers", "tuples"
  , "effect", "prelude", "bifunctors", "exceptions", "record", "functors"
  , "affjax", "affjax-web", "strings", "partial", "unordered-collections", "formatters"
  , "halogen", "halogen-subscriptions", "web-html", "web-events", "web-uievents"
  , "web-pointerevents", "unsafe-coerce", "console"
  ]
, packages = 
  https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230915/packages.dhall
    sha256:f362bee6cdbba335ad43d33d2017bfb1d86c02f4629c723bd66d198d944841eb
, sources = [ "src/**/*.purs" ]
}
