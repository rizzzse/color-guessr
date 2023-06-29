{ name = "my-project"
, dependencies =
  [ "aff"
  , "colors"
  , "console"
  , "control"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "strings"
  , "transformers"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
