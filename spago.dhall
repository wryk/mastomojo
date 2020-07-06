{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mastodon-emoji-viewer"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "foreign-generic"
  , "halogen"
  , "halogen-hooks"
  , "milkis"
  , "prelude"
  , "psci-support"
  , "strings-extra"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
