{ name = "deku-tree"
, dependencies =
  [ "control"
  , "deku"
  , "effect"
  , "event"
  , "foldable-traversable"
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
