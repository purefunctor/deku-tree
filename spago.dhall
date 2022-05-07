{ name = "deku-tree"
, dependencies =
  [ "behaviors"
  , "control"
  , "deku"
  , "effect"
  , "event"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
