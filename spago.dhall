{ name = "deku-tree"
, dependencies =
  [ "behaviors"
  , "console"
  , "control"
  , "deku"
  , "effect"
  , "event"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
