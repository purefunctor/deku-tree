{ name = "deku-tree"
, dependencies =
  [ "control"
  , "deku"
  , "event"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "routing"
  , "slug"
  , "strings"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
