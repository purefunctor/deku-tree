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
  , "refs"
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
