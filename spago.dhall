{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mastomojo"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign"
  , "foreign-generic"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "milkis"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "strings-extra"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
