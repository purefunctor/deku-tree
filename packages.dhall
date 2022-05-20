let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220506/packages.dhall
        sha256:f83b68ff07cf6557e82379e749118e6ff11eecc6be5754540aae855cd1e46917

let additions =
      { event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "monoid-extras"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.6.6"
        }
      , variant =
        { dependencies =
          [ "assert"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "record"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/natefaubion/purescript-variant.git"
        , version = "v8.0.0"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "psci-support"
          , "variant"
          ]
        , repo =
            "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.2.1"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      , deku =
        { dependencies =
          [ "arrays"
          , "bolson"
          , "control"
          , "effect"
          , "event"
          , "exceptions"
          , "fast-vect"
          , "filterable"
          , "foldable-traversable"
          , "foreign-object"
          , "heterogeneous"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "quickcheck"
          , "record"
          , "refs"
          , "safe-coerce"
          , "st"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          , "web-dom"
          , "web-events"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "v0.4.4"
        }
      , monoid-extras =
        { dependencies = [ "profunctor-lenses" ]
        , repo = "https://github.com/mikesol/purescript-monoid-extras.git"
        , version = "v0.0.1"
        }
      , bolson =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "v0.0.2"
        } 
      }

in  upstream // additions
