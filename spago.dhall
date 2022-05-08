{ name = "deku-tree"
, dependencies =
  [ "control"
  , "deku"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "routing"
  , "slug"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
