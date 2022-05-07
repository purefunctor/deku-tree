{ name = "deku-tree"
, dependencies =
  [ "console"
  , "control"
  , "deku"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "partial"
  , "prelude"
  , "routing"
  , "slug"
  , "strings"
  , "typelevel-prelude"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
