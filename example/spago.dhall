let toplevel = ../spago.dhall

in      toplevel
    //  { dependencies =
            toplevel.dependencies # [ "behaviors", "effect", "refs" ]
        , sources = toplevel.sources # [ "example/*.purs" ]
        }
